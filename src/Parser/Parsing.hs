{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
-- | Module dedicated to Parsing

module Parser.Parsing where

import           Parser.AST
-- Importing position from Parsec
import           Text.Parsec.Pos
-- Importing parser combinators
import           Text.Parsec hiding (Error, Ok)

-- Importing tokenizer
import qualified Text.Parsec.Expr     as Ex
import qualified Text.Parsec.Language as Lang
import qualified Text.Parsec.Token    as Tok
import qualified Text.Parsec.Prim     as P
import qualified Text.Parsec.Error    as PE

import Data.Functor
import Text.Parsec.Expr
import qualified Data.List as L
import Control.Monad
import Data.Char
import Data.Maybe (isJust)
import qualified Data.IntMap.Strict as IM
import Text.Read (readMaybe)
import Utils.Annotations
import Parser.Types
import Doc.Model (DocLine(..), DocError, linesAt, commentLines, splitDoc)

type TerminaParser = Parsec String FilePath

----------------------------------------
-- Lexer
----------------------------------------

lexer :: Tok.TokenParser FilePath
lexer = Tok.makeTokenParser langDef
  where
    reservedNames =
      -- Basic Types
      ["u8","u16","u32","u64"
      ,"i8","i16","i32","i64"
      ,"usize", "bool","char"
      ,"f32","f64"
      ,"unit"]
      ++ -- Struct and enum types
              -- Struct and enum types
             ["struct", "enum"]
      ++ -- Class types
              -- Class types
             ["class"]
      ++ -- Box Subtyping
              -- Box Subtyping
             ["box"]
      ++ -- Fixed-location subtyping
              -- Fixed-location subtyping
             ["loc"]
      ++ -- Ports Subtyping
              -- Ports Subtyping
             ["access", "sink", "in", "out"]
      ++ -- Global declarations
              -- Global declarations
             ["task", "function", "handler", "resource", "const", "constexpr", "test"]
      ++ -- Functions implemented in C
             ["extern", "noreturn"]
      ++ -- Stmt
              -- Stmt
             ["var", "let", "match", "case", "for", "if", "else", "return", "continue", "while"]
      ++ -- Trigger
              -- Trigger
             ["triggers"]
      ++ -- Provide/extend
              -- Provide/extend
             ["provides", "extends"]
      ++ -- Constants
              -- Constants
             ["true", "false", "null"]
      ++ -- Modules
              -- Modules
             ["import"]
      ++ -- Class methods
              -- Class methods
             ["procedure", "viewer", "method", "action"]
      ++ -- Casting keyword
              -- Casting keyword
             ["as"]
      ++ -- is variant operator
              -- is variant operator
             ["is"]
      ++ -- system-wide reserved names
             ["termina", "option", "config"]

    langDef =
      -- Comments are skipped by wspcs, which stops before a documentation
      -- comment, so the token parser of Parsec only skips white space.
      Lang.emptyDef{ Tok.commentStart = ""
                   , Tok.commentEnd = ""
                   , Tok.commentLine = ""
                   , Tok.nestedComments = False
                   -- | Here we define that identifiers being with a letter
                   , Tok.identStart = letter
                   -- | Rest of identifiers accepted characters
                   , Tok.identLetter = alphaNum <|> char '_'
                   -- | Operators begin with
                   , Tok.opStart = oneOf ".*/+-<>=!&|^,;(["
                   , Tok.reservedNames = reservedNames
                   , Tok.reservedOpNames = [
                       "." -- MemberAccess
                      ,"*" -- Multiplication
                      ,"/" -- Division
                      ,"+" -- Addition
                      ,"-" -- Substraction
                      ,"%" -- Modulo
                      ,"<<" -- BitwiseLeftShift
                      ,">>" -- BitwiseRightShift
                      ,"<" -- RelationalLT
                      ,"<=" -- RelationalLTE
                      ,">" -- RelationalGT
                      ,">=" -- RelationalGTE
                      ,"==" -- RelationalEqual
                      ,"!=" -- RelationalNotEqual
                      ,"&" -- BitwiseAnd / reference creation
                      ,"|" -- BitwiseOr
                      ,"^" -- BitwiseXor
                      ,"&&" -- LogicalAnd
                      ,"||" -- LogicalOr
                      ,"#" -- Attribute
                      ,":" -- Type annotation
                      ,"::" -- Enum variant
                      ,"=" -- Assignment
                      ,"->" -- Function return type/Outbound connection
                      ,"=>" -- Match case
                      ,"[" -- TArray init
                      ,"]" -- TArray init
                      ,"{" -- Field values assignments
                      ,"}" -- Field values assignments
                      ,"(" -- Parens
                      ,")" -- Parens
                      ,".." -- TArray slice and for loop range
                      ,"&mut" -- Mutable reference creation
                      ,"&priv" -- Private reference creation
                      ,"@" -- Field address assignment
                      ,"<->" -- Access port connection
                      ,"<-" -- Inbound/Sink connection
                    ]
                   -- | Is the language case sensitive? It should be
                   , Tok.caseSensitive = True
                   }

-- Comments

-- | The documentation comment that a text starts with, if any, with the length
-- of its opening delimiter. "////", "/***" and "/**/" open ordinary comments.
docOpener :: String -> Maybe (DocPlacement, DocForm, Int)
docOpener input = case input of
  '/':'/':'/':'<':_ -> Just (DocAfter, LineDoc, 4)
  '/':'/':'/':c:_ | c /= '/' -> Just (DocBefore, LineDoc, 3)
  "///" -> Just (DocBefore, LineDoc, 3)
  '/':'/':'!':_ -> Just (DocModule, LineDoc, 3)
  '/':'*':'*':'<':_ -> Just (DocAfter, BlockDoc, 4)
  '/':'*':'*':c:_ | c /= '*' && c /= '/' -> Just (DocBefore, BlockDoc, 3)
  '/':'*':'!':_ -> Just (DocModule, BlockDoc, 3)
  _ -> Nothing

-- | Skips white space and ordinary comments, and stops before a documentation
-- comment.
wspcs :: TerminaParser ()
wspcs = skipMany (skipMany1 space <|> ordinaryComment)

-- | A line or block comment that is not a documentation comment. The input is
-- looked at before any character is consumed, so a failure leaves no error
-- inside a delimiter.
ordinaryComment :: TerminaParser ()
ordinaryComment = do
  input <- getInput
  case input of
    _ | isJust (docOpener input) -> parserZero
    '/':'/':_ -> string "//" *> skipMany (satisfy (/= '\n'))
    '/':'*':_ -> string "/*" *> void (manyTill anyChar (try (string "*/")))
    _ -> parserZero

-- | A documentation comment and the white space after it. A run of line
-- comments, or a single block comment, is one comment. Its text is everything
-- between the delimiters, with the lines of a run joined by a newline. An error
-- in its tags is reported at the tag.
docComment :: DocPlacement -> TerminaParser DocComment
docComment placement = do
  current <- getState
  start <- getPosition
  (form, written) <- ((BlockDoc,) <$> blockDoc) <|> ((LineDoc,) <$> lineDocs)
  end <- getPosition
  either (\(pos, err) -> failAt pos (docTagMessage err)) (const (return ()))
    (splitDoc (commentLines form written))
  wspcs
  return (DocComment placement form (L.intercalate "\n" (map lineText written)) (Position current start end))

  where

    -- A comment after an element is a single line: another one on the next
    -- line is not on the line of the element.
    lineDocs
      | placement == DocAfter = (: []) <$> lineDoc
      | otherwise = (:) <$> lineDoc <*> many (try (wspcs *> lineDoc))
    opener form = do
      input <- getInput
      case docOpener input of
        Just (placement', form', len) | placement' == placement && form' == form ->
          void (count len anyChar)
        _ -> parserZero
    lineDoc = opener LineDoc *> (DocLine <$> getPosition <*> many (satisfy (/= '\n')))
    blockDoc = opener BlockDoc *> (linesAt <$> getPosition <*> manyTill anyChar (try (string "*/")))

-- | Fails without consuming input where a documentation comment starts. Every
-- token begins with it, so that a parse error at a comment that the grammar
-- does not take there carries 'misplacedDocComment'.
noDocComment :: TerminaParser ()
noDocComment = do
  input <- getInput
  case docOpener input of
    Just (placement, _, _) -> fail (misplacedDocComment placement)
    Nothing -> return ()

-- | The documentation comment before an element, if any, and the element. A
-- comment that no element follows is reported at its own position.
docBefore :: TerminaParser a -> TerminaParser (Maybe DocComment, a)
docBefore p = do
  pos <- getPosition
  doc <- optionMaybe (docComment DocBefore)
  case doc of
    Nothing -> (Nothing,) <$> p
    Just _ -> ((doc,) <$> p) <|> failAt pos (misplacedDocComment DocBefore)

-- | An element with its documentation comment before it in its annotation.
documented :: Annotated f => TerminaParser (f ParserAnn) -> TerminaParser (f ParserAnn)
documented p = docBefore p >>= uncurry withDoc

-- | An element with its documentation comment after it, on the same line, in
-- its annotation.
documentedAfter :: Annotated f => TerminaParser (f ParserAnn) -> TerminaParser (f ParserAnn)
documentedAfter p = do
  x <- p
  doc <- docAfter
  withDoc doc x

-- | An element with a documentation comment in its annotation. An element
-- takes one comment, so a second one is reported at the comment after it.
withDoc :: Annotated f => Maybe DocComment -> f ParserAnn -> TerminaParser (f ParserAnn)
withDoc Nothing x = return x
withDoc (Just doc) x =
  case parsedDoc ann of
    Nothing -> return (updateAnnotation x ann { parsedDoc = Just doc })
    Just previous -> failAt (startOf (afterOf previous doc)) twoDocComments

  where

    ann = getAnnotation x
    afterOf a b = if docPlacement a == DocAfter then a else b
    startOf d = case docLocation d of
      Position _ start _ -> start
      _ -> error "Internal error: a documentation comment without a position"

-- | The message of an element with a documentation comment before it and
-- another one after it.
twoDocComments :: String
twoDocComments = "An element with two documentation comments."

-- | The message of a documentation comment where the grammar takes none. Each
-- placement has its own, which the error of the parse keeps.
misplacedDocComment :: DocPlacement -> String
misplacedDocComment DocBefore = "Misplaced documentation comment before an element."
misplacedDocComment DocAfter = "Misplaced documentation comment after an element."
misplacedDocComment DocModule = "Misplaced documentation comment of the module."

-- | The message of an error in the tags of a documentation comment. The error
-- itself is in the message, from which 'docTagError' takes it back.
docTagMessage :: DocError -> String
docTagMessage err = docTagPrefix ++ show err

-- | The error in the tags of a documentation comment that a message carries.
docTagError :: String -> Maybe DocError
docTagError msg = L.stripPrefix docTagPrefix msg >>= readMaybe

docTagPrefix :: String
docTagPrefix = "Error in a documentation tag: "

-- | The documentation comment after an element on the same line, if any.
docAfter :: TerminaParser (Maybe DocComment)
docAfter = optionMaybe (docComment DocAfter)

-- Lexemes

-- | A token followed by white space and ordinary comments. The token parser of
-- Parsec only skips white space after a token, since comments are not defined
-- in langDef. A token never spans two lines, so the line where it starts is
-- the line that a documentation comment after it has to be on.
lexeme :: TerminaParser a -> TerminaParser a
lexeme p = do
  noDocComment
  line <- sourceLine <$> getPosition
  x <- p
  wspcs
  docAfterOnLine line
  return x

-- | Fails at a documentation comment after an element that is not on the line
-- of the token before it.
docAfterOnLine :: Line -> TerminaParser ()
docAfterOnLine line = do
  input <- getInput
  pos <- getPosition
  case docOpener input of
    Just (DocAfter, _, _) | sourceLine pos /= line -> failAt pos (misplacedDocComment DocAfter)
    _ -> return ()

symbol :: String -> TerminaParser String
symbol = lexeme . Tok.symbol lexer

braces :: TerminaParser a -> TerminaParser a
braces = between (symbol "{") (symbol "}")

brackets :: TerminaParser a -> TerminaParser a
brackets = between (symbol "[") (symbol "]")

parens :: TerminaParser a -> TerminaParser a
parens = between (symbol "(") (symbol ")")

angles :: TerminaParser a -> TerminaParser a
angles = between (symbol "<") (symbol ">")

comma :: TerminaParser String
comma = symbol ","

semi :: TerminaParser String
semi = symbol ";"

dot :: TerminaParser String
dot = symbol "."

stringLit :: TerminaParser String
stringLit = lexeme (Tok.stringLiteral lexer)

charLit :: TerminaParser Char
charLit = lexeme (Tok.charLiteral lexer)

reserved :: String -> TerminaParser ()
reserved = lexeme . Tok.reserved lexer

reservedOp :: String -> TerminaParser ()
reservedOp = lexeme . Tok.reservedOp lexer

identifierParser :: TerminaParser String
identifierParser = do
  startPos <- getPosition
  lexeme (Tok.identifier lexer) >>= checkUnderscores startPos

-- | Checks that an identifier does not contain two consecutive underscores.
checkUnderscores :: SourcePos -> Identifier -> TerminaParser Identifier
checkUnderscores startPos ident = do
  when ("__" `L.isInfixOf` ident) $
    failAt (incSourceColumn startPos (length ident)) ("Identifiers cannot contain two consecutive underscores: " ++ ident ++ ".")
  return ident

-- | Fails at the given position with the given message.
failAt :: SourcePos -> String -> TerminaParser a
failAt pos msg = P.mkPT $ \_ ->
  return (P.Consumed (return (P.Error (PE.newErrorMessage (PE.Message msg) pos))))

tails :: [a] -> [[a]]
tails = L.tails

number :: Integer -> TerminaParser Char -> TerminaParser Integer
number base baseDigit = do
    digits <- many1 baseDigit
    let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
    seq n (return n)

sign :: Num a => TerminaParser (a -> a)
sign = (char '-' >> return negate)
  <|> (char '+' >> return id)
  <|> return id

-- | TerminaParser for integer decimal numbers
-- This parser is used when defining regular integer literals
decimal :: TerminaParser Integer
decimal =  lexeme $ do
  f <- lexeme sign
  n <- number 10 digit
  return (f n)

hexadecimal :: TerminaParser Integer
hexadecimal = lexeme $
  char '0' >> oneOf "xX" >> number 16 hexDigit

-- | TerminaParser for floating-point numbers in decimal notation: digits '.' digits.
floating :: TerminaParser Double
floating = lexeme $ do
  f  <- lexeme sign
  ip <- many1 digit
  _  <- char '.'
  fp <- many1 digit
  return (f (read (ip ++ "." ++ fp)))

-- | TerminaParser for floating-point numbers in scientific notation:
-- digits ['.' digits] exponent.
scientific :: TerminaParser Double
scientific = lexeme $ do
  f  <- lexeme sign
  ip <- many1 digit
  fp <- option "" (('.' :) <$> (char '.' *> many1 digit))
  e  <- oneOf "eE"
  es <- option "" ((:[]) <$> oneOf "+-")
  ed <- many1 digit
  return (f (read (ip ++ fp ++ [e] ++ es ++ ed)))

floatParser :: TerminaParser TFloat
floatParser = try (flip TFloat FPScientific <$> scientific)
          <|> (flip TFloat FPDecimal <$> floating)


----------------------------------------
-- TerminaParser
----------------------------------------

typeParamParser :: TerminaParser (TypeParameter ParserAnn)
typeParamParser = do
  TypeParamIdentifier <$> identifierParser
  <|> TypeParamTypeSpec <$> typeSpecifierParser
  <|> TypeParamSize <$> expressionTermParser

--  (angles (sepBy (wspcs *> typeParamParser <* wspcs) semi)  semi)
definedTypeParser :: TerminaParser (TypeSpecifier ParserAnn)
definedTypeParser = do
  name <- identifierParser
  typeParams <- option [] (angles (sepBy (wspcs *> typeParamParser <* wspcs) semi))
  return $ TSDefinedType name typeParams

-- | Types
typeSpecifierParser :: TerminaParser (TypeSpecifier ParserAnn)
typeSpecifierParser =
  definedTypeParser
  <|> arrayParser
  <|> boxSubtypeParser
  <|> mutableReferenceParser
  <|> referenceParser
  <|> locationSubtypeParser
  <|> sinkPortParser
  <|> inPortParser
  <|> outPortParser
  <|> accessPortParser
  <|> (reserved "u8" >> return TSUInt8)
  <|> (reserved "u16" >> return TSUInt16)
  <|> (reserved "u32" >> return TSUInt32)
  <|> (reserved "u64" >> return TSUInt64)
  <|> (reserved "i8" >> return TSInt8)
  <|> (reserved "i16" >> return TSInt16)
  <|> (reserved "i32" >> return TSInt32)
  <|> (reserved "i64" >> return TSInt64)
  <|> (reserved "usize" >> return TSUSize)
  <|> (reserved "bool" >> return TSBool)
  <|> (reserved "char" >> return TSChar)
  <|> (reserved "f32" >> return TSFloat32)
  <|> (reserved "f64" >> return TSFloat64)
  <|> (reserved "unit" >> return TSUnit)

objectIdentifierParser :: TerminaParser Identifier
objectIdentifierParser = do
  startPos <- getPosition
  ident <- lexeme (try ((char '_' >> Tok.identifier lexer) <&> ('_' :)) <|> Tok.identifier lexer)
  checkUnderscores startPos ident

parameterParser :: TerminaParser (Parameter ParserAnn)
parameterParser = do
  current <- getState
  startPos <- getPosition
  identifier <- objectIdentifierParser
  reservedOp ":"
  typeSpecifier <- typeSpecifierParser
  Parameter identifier typeSpecifier . located current startPos <$> getPosition

-- | TerminaParser for a field value assignments expression
-- This expression is used to create annonymous structures to serve as right
-- hand side of an assignment expression.
-- Examples of this expression:
-- { field0 = 0 : u32, field1 = 0 : u16 } : StructIdentifier
structInitializerParser :: TerminaParser (Expression ParserAnn)
structInitializerParser = do
    current <- getState
    startpos <- getPosition
    assignments <- braces (sepBy (wspcs *> fieldAssignmentParser <* wspcs) comma)
    typeSpecifier <- optionMaybe (reservedOp ":" >> typeSpecifierParser)
    StructInitializer assignments typeSpecifier . located current startpos <$> getPosition
    where

      fieldAssignmentParser :: TerminaParser (FieldAssignment ParserAnn)
      fieldAssignmentParser = do
        try fieldValueParser <|> try fieldAddressParser <|> try fieldAccessPortConnectionParser
            <|> try fieldInboundPortConnectionParser <|> fieldOutboundPortConnectionParser

      fieldValueParser :: TerminaParser (FieldAssignment ParserAnn)
      fieldValueParser = do
        current <- getState
        startPos <- getPosition
        identifier <- identifierParser
        _ <- reservedOp "="
        expr <- expressionParser
        FieldValueAssignment identifier expr . located current startPos <$> getPosition

      fieldAddressParser :: TerminaParser (FieldAssignment ParserAnn)
      fieldAddressParser = do
        current <- getState
        startPos <- getPosition
        identifier <- identifierParser
        _ <- reservedOp "@"
        addr <- expressionParser
        FieldAddressAssignment identifier addr . located current startPos <$> getPosition

      fieldAccessPortConnectionParser :: TerminaParser (FieldAssignment ParserAnn)
      fieldAccessPortConnectionParser = do
        current <- getState
        startPos <- getPosition
        identifier <- identifierParser
        _ <- reservedOp "<->"
        port <- identifierParser
        FieldPortConnection AccessPortConnection identifier port . located current startPos <$> getPosition

      fieldInboundPortConnectionParser :: TerminaParser (FieldAssignment ParserAnn)
      fieldInboundPortConnectionParser = do
        current <- getState
        startPos <- getPosition
        identifier <- identifierParser
        _ <- reservedOp "<-"
        port <- identifierParser
        FieldPortConnection InboundPortConnection identifier port . located current startPos <$> getPosition

      fieldOutboundPortConnectionParser :: TerminaParser (FieldAssignment ParserAnn)
      fieldOutboundPortConnectionParser = do
        current <- getState
        startPos <- getPosition
        identifier <- identifierParser
        _ <- reservedOp "->"
        port <- identifierParser
        FieldPortConnection OutboundPortConnection identifier port . located current startPos <$> getPosition

-- | Parser for an element modifier
-- A modifier is of the form:
-- #[identifier(expression)]
-- where:
-- - identifier: is a mandatory identifier that names the attribute or modifier
-- - expresssion: is an optional expression between parenthesis.AST
-- Examples of a modifier:
-- #[priority(5)] for a task
-- #[packed] for a struct or union

modifierParser :: TerminaParser (Modifier ParserAnn)
modifierParser = do
  _ <- reservedOp "#"
  _ <- reservedOp "["
  identifier <- identifierParser
  expr <- optionMaybe (parens expressionParser)
  _ <- reservedOp "]"
  return $ Modifier identifier expr

arrayParser :: TerminaParser (TypeSpecifier ParserAnn)
arrayParser = do
  _ <- reservedOp "["
  typeSpecifier <- typeSpecifierParser
  _ <- semi
  size <- expressionParser
  _ <- reserved "]"
  return $ TSArray typeSpecifier size

referenceParser :: TerminaParser (TypeSpecifier ParserAnn)
referenceParser = reservedOp "&" >> TSReference Immutable <$> typeSpecifierParser

mutableReferenceParser :: TerminaParser (TypeSpecifier ParserAnn)
mutableReferenceParser = reservedOp "&mut" >> TSReference Mutable <$> typeSpecifierParser

boxSubtypeParser :: TerminaParser (TypeSpecifier ParserAnn)
boxSubtypeParser = reserved "box" >> TSBoxSubtype <$> typeSpecifierParser

locationSubtypeParser :: TerminaParser (TypeSpecifier ParserAnn)
locationSubtypeParser = reservedOp "loc" >> TSLocation <$> typeSpecifierParser

sinkPortParser :: TerminaParser (TypeSpecifier ParserAnn)
sinkPortParser = do
  _ <- reserved "sink"
  ts <- typeSpecifierParser
  _ <- reserved "triggers"
  TSSinkPort ts <$> identifierParser

accessPortParser :: TerminaParser (TypeSpecifier ParserAnn)
accessPortParser = reservedOp "access" >> TSAccessPort <$> typeSpecifierParser

outPortParser :: TerminaParser (TypeSpecifier ParserAnn)
outPortParser = reservedOp "out" >> TSOutPort <$> typeSpecifierParser

inPortParser :: TerminaParser (TypeSpecifier ParserAnn)
inPortParser = do
  _ <- reserved "in"
  ts <- typeSpecifierParser
  _ <- reserved "triggers"
  TSInPort ts <$> identifierParser

-- Expression TerminaParser
expressionParser' :: TerminaParser (Expression ParserAnn)
expressionParser' = buildPrattParser -- New parser
    [[castingPostfix]
    ,[binaryInfix "*" Multiplication Ex.AssocLeft,
      binaryInfix "/" Division Ex.AssocLeft,
      binaryInfix "%" Modulo Ex.AssocLeft]
    ,[binaryInfix "+" Addition Ex.AssocLeft,
      binaryInfix "-" Subtraction Ex.AssocLeft]
    ,[binaryInfix "<<" BitwiseLeftShift Ex.AssocLeft,
      binaryInfix ">>" BitwiseRightShift Ex.AssocLeft]
    ,[binaryInfix "<" RelationalLT Ex.AssocLeft,
      binaryInfix "<=" RelationalLTE Ex.AssocLeft,
      binaryInfix ">" RelationalGT Ex.AssocLeft,
      binaryInfix ">=" RelationalGTE Ex.AssocLeft]
    ,[binaryInfix "==" RelationalEqual Ex.AssocLeft,
      binaryInfix "!=" RelationalNotEqual Ex.AssocLeft]
    ,[binaryInfix "&" BitwiseAnd Ex.AssocLeft]
    ,[binaryInfix "|" BitwiseOr Ex.AssocLeft]
    ,[binaryInfix "^" BitwiseXor Ex.AssocLeft]
    ,[binaryInfix "&&" LogicalAnd Ex.AssocLeft]
    ,[binaryInfix "||" LogicalOr Ex.AssocLeft]
    ]
    expressionTermParser
  where
    binaryInfix s f = Ex.Infix (do
          current <- getState
          _ <- reservedOp s
          return $ \l r -> BinOp f l r (located current (getStartPosition (getAnnotation l)) (getEndPosition (getAnnotation r))))
    castingPostfix = Ex.Postfix (do
          current <- getState
          _ <- reserved "as"
          typeSpecificer <- typeSpecifierParser
          endPos <- getPosition
          return $ \parent -> Casting parent typeSpecificer (located current (getStartPosition (getAnnotation parent)) endPos))

functionCallParser :: TerminaParser (Expression ParserAnn)
functionCallParser = do
  current <- getState
  startPos <- getPosition
  ident <- identifierParser
  params <- parens (sepBy (try expressionParser) comma)
  FunctionCall ident params . located current startPos <$> getPosition

builtinVariantExprParser :: TerminaParser (Expression ParserAnn)
builtinVariantExprParser =
  (do
    current <- getState
    startPos <- getPosition
    _ <- reserved "None"
    MonadicVariantInitializer None . located current startPos <$> getPosition) <|>
  (do
    current <- getState
    startPos <- getPosition
    _ <- reserved "Some"
    someExpr <- parens expressionParser
    MonadicVariantInitializer (Some someExpr) . located current startPos <$> getPosition) <|>
  (do
    current <- getState
    startPos <- getPosition
    _ <- reserved "Ok"
    okExpr <- parens expressionParser
    MonadicVariantInitializer (Ok okExpr) . located current startPos <$> getPosition) <|>
  (do
    current <- getState
    startPos <- getPosition
    _ <- reserved "Error"
    errorExpr <- parens expressionParser
    MonadicVariantInitializer (Error errorExpr) . located current startPos <$> getPosition) <|>
  (do
    current <- getState
    startPos <- getPosition
    _ <- reserved "Success"
    MonadicVariantInitializer Success . located current startPos <$> getPosition) <|>
  (do
    current <- getState
    startPos <- getPosition
    _ <- reserved "Failure"
    failureExpr <- parens expressionParser
    MonadicVariantInitializer (Failure failureExpr) . located current startPos <$> getPosition)

enumVariantExprParser :: TerminaParser (Expression ParserAnn)
enumVariantExprParser = do
  current <- getState
  startPos <- getPosition
  enum <- identifierParser
  _ <- reservedOp "::"
  variant <- identifierParser
  parameterList <-
    option [] (parens (sepBy (try expressionParser) comma))
  EnumVariantInitializer enum variant parameterList . located current startPos <$> getPosition

isEnumVariantExprParser :: TerminaParser (Expression ParserAnn)
isEnumVariantExprParser = do
  current <- getState
  startPos <- getPosition
  object <- objectParser
  _ <- reserved "is"
  enum <- identifierParser
  _ <- reservedOp "::"
  variant <- identifierParser
  IsEnumVariantExpression object enum variant . located current startPos <$> getPosition

isMonadicVariantExprParser :: TerminaParser (Expression ParserAnn)
isMonadicVariantExprParser =
  try (do
    current <- getState
    startPos <- getPosition
    object <- objectParser
    _ <- reserved "is"
    _ <- reserved "None"
    IsMonadicVariantExpression object NoneLabel . located current startPos <$> getPosition) <|>
  try (do
    current <- getState
    startPos <- getPosition
    object <- objectParser
    _ <- reserved "is"
    _ <- reserved "Some"
    IsMonadicVariantExpression object SomeLabel . located current startPos <$> getPosition) <|>
  try (do
    current <- getState
    startPos <- getPosition
    object <- objectParser
    _ <- reserved "is"
    _ <- reserved "Ok"
    IsMonadicVariantExpression object OkLabel . located current startPos <$> getPosition) <|>
  try (do
    current <- getState
    startPos <- getPosition
    object <- objectParser
    _ <- reserved "is"
    _ <- reserved "Error"
    IsMonadicVariantExpression object ErrorLabel . located current startPos <$> getPosition) <|>
  try (do
    current <- getState
    startPos <- getPosition
    object <- objectParser
    _ <- reserved "is"
    _ <- reserved "Success"
    IsMonadicVariantExpression object SuccessLabel . located current startPos <$> getPosition) <|>
  (do
    current <- getState
    startPos <- getPosition
    object <- objectParser
    _ <- reserved "is"
    _ <- reserved "Failure"
    IsMonadicVariantExpression object FailureLabel . located current startPos <$> getPosition)

expressionParser :: TerminaParser (Expression ParserAnn)
expressionParser = try builtinVariantExprParser
  <|> try enumVariantExprParser
  <|> try mutableReferenceExprParser
  <|> try isMonadicVariantExprParser
  <|> try arrayExprListInitializerParser
  <|> arrayInitializerParser
  <|> referenceExprParser
  <|> structInitializerParser
  <|> stringInitializerParser
  <|> expressionParser'

mutableReferenceExprParser :: TerminaParser (Expression ParserAnn)
mutableReferenceExprParser = do
  current <- getState
  startPos <- getPosition
  _ <- reservedOp "&mut"
  object <- objectParser
  ReferenceExpression Mutable object . located current startPos <$> getPosition

referenceExprParser :: TerminaParser (Expression ParserAnn)
referenceExprParser = do
  current <- getState
  startPos <- getPosition
  _ <- reservedOp "&"
  object <- objectParser
  ReferenceExpression Immutable object . located current startPos <$> getPosition

expressionTermParser :: TerminaParser (Expression ParserAnn)
expressionTermParser = try isEnumVariantExprParser
  <|> try constantParser
  <|> try functionCallParser
  <|> try accessObjectParser
  <|> parensExprParser

parensExprParser :: TerminaParser (Expression ParserAnn)
parensExprParser = parens expressionParser

parensObjectParser :: TerminaParser (Object ParserAnn)
parensObjectParser = parens objectParser

----------------------------------------
-- Object Parsing

-- Object term parser.
objectTermParser :: TerminaParser (Object ParserAnn)
objectTermParser = (do
    current <- getState
    startPos <- getPosition
    ident <- objectIdentifierParser
    return $ Variable ident (located current startPos (incSourceColumn startPos (length ident))))
  <|> parensObjectParser

-- Expression parser
-- This parser is a variation of the original parser that allows us to chain
-- two or more unary expressions together. This is useful when parsing expressions
-- such as array[0][1], where the postfix operator [] is used twice. This code
-- has been directly extracted from this StackOverflow answer:
-- https://stackoverflow.com/questions/33214163/parsec-expr-repeated-prefix-with-different-priority/33534426#33534426
-- We now use it to parse expressions and object operations.
buildPrattParser :: (Stream s m t)
                      => OperatorTable s u m a
                      -> ParsecT s u m a
                      -> ParsecT s u m a
buildPrattParser table termP = parser precs where
  precs = reverse table
  prefixP = choice prefixPs <|> termP where
    prefixPs = do
      precsR@(ops:_) <- tails precs
      Prefix opP <- ops
      return $ opP <*> parser precsR
  infixP precs' lhs = choice infixPs <|> pure lhs where
    infixPs = do
      precsR@(ops:precsL) <- tails precs'
      op <- ops
      p <- case op of
        Infix opP assoc -> do
          let p precs'' = opP <*> pure lhs <*> parser precs''
          return $ case assoc of
            AssocNone  -> error "Non associative operators are not supported"
            AssocLeft  -> p precsL
            AssocRight -> p precsR
        Postfix opP ->
          return $ opP <*> pure lhs
        Prefix _ -> mzero
      return $ p >>= infixP precs'
  parser precs' = prefixP >>= infixP precs'

objectParser :: TerminaParser (Object ParserAnn)
objectParser = objectParser' objectTermParser
  where
    objectParser'
      = buildPrattParser -- New parser
      [[dereferenceMemberAccessPostfix, memberAccessPostfix, arrayOpPostfix]
      ,[dereferencePrefix]]
    arrayOpPostfix
      = Ex.Postfix (try (do
            current <- getState
            _ <- reservedOp "["
            low <- expressionParser
            _ <- reservedOp ".."
            up <- expressionParser
            _ <- reservedOp "]"
            endPos <- getPosition
            return $ \parent ->  ArraySlice parent low up (located current (getStartPosition (getAnnotation parent)) endPos)
          ) <|> (do
            current <- getState
            index <- brackets expressionParser
            endPos <- getPosition
            return $ \parent ->  ArrayIndexExpression parent index (located current (getStartPosition (getAnnotation parent)) endPos)
          ))
    dereferenceMemberAccessPostfix
      = Ex.Postfix (do
        current <- getState
        _ <- reservedOp "->"
        member <- identifierParser
        endPos <- getPosition
        return $ \parent -> DereferenceMemberAccess parent member (located current (getStartPosition (getAnnotation parent)) endPos))
    memberAccessPostfix
      = Ex.Postfix (do
        current <- getState
        _ <- reservedOp "."
        member <- identifierParser
        endPos <- getPosition
        return $ \parent ->  MemberAccess parent member (located current (getStartPosition (getAnnotation parent)) endPos))
    dereferencePrefix
      = Ex.Prefix (do
        current <- getState
        startPos <- getPosition
        _ <- reservedOp "*"
        return $ \child -> Dereference child (located current startPos (getEndPosition (getAnnotation child))))
----------------------------------------

accessObjectParser :: TerminaParser (Expression ParserAnn)
accessObjectParser =
  accessObjectParser' (AccessObject <$> objectTermParser)
  where
    accessObjectParser'
      = buildPrattParser -- New parser
      [[dereferenceMemberAccessPostfix, memberAccessPostfix, arrayOpPostfix]
      ,[dereferencePrefix]]
    arrayOpPostfix
      = Ex.Postfix (try (do
            current <- getState
            _ <- reservedOp "["
            low <- expressionParser
            _ <- reservedOp ".."
            up <- expressionParser
            _ <- reservedOp "]"
            endPos <- getPosition
            return $ \parent -> case parent of
              AccessObject obj -> AccessObject (ArraySlice obj low up (located current (getStartPosition (getAnnotation parent)) endPos))
              _ -> error "Unexpected member access to a non object"
          ) <|> (do
            current <- getState
            index <- brackets expressionParser
            endPos <- getPosition
            return $ \parent -> case parent of
              AccessObject obj -> AccessObject (ArrayIndexExpression obj index (located current (getStartPosition (getAnnotation parent)) endPos))
              _ -> error "Unexpected member access to a non object"
          ))
    dereferenceMemberAccessPostfix
      = Ex.Postfix (do
      current <- getState
      _ <- reservedOp "->"
      member <- identifierParser
      params <- optionMaybe (parens (sepBy (try expressionParser) comma))
      endPos <- getPosition
      return (\parent -> case parent of
        AccessObject obj ->
          let startPos = getStartPosition (getAnnotation parent) in
          maybe (AccessObject (DereferenceMemberAccess obj member (located current startPos endPos))) (flip (DerefMemberFunctionCall obj member) (located current startPos endPos)) params
        _ -> error "Unexpected member access to a non object"))
    memberAccessPostfix
      = Ex.Postfix (do
      current <- getState
      _ <- reservedOp "."
      member <- identifierParser
      params <- optionMaybe (parens (sepBy (try expressionParser) comma))
      endPos <- getPosition
      return (\parent -> case parent of
        AccessObject obj ->
          let startPos = getStartPosition (getAnnotation parent) in
          maybe (AccessObject (MemberAccess obj member (located current startPos endPos))) (flip (MemberFunctionCall obj member) (located current startPos endPos)) params
        _ -> error "Unexpected member access to a non object"))
    dereferencePrefix
      = Ex.Prefix (do
      current <- getState
      startPos <- getPosition
      _ <- reservedOp "*"
      return (\child -> case child of
        AccessObject obj ->
          let endPos = getStartPosition (getAnnotation child) in
          AccessObject (Dereference obj (located current startPos endPos))
        _ -> error "Unexpected member access to a non object"))

arrayInitializerParser :: TerminaParser (Expression ParserAnn)
arrayInitializerParser = do
  current <- getState
  startPos <- getPosition
  _ <- reservedOp "["
  value <- expressionParser
  _ <- semi
  size <- expressionParser
  _ <- reservedOp "]"
  ArrayInitializer value size . located current startPos <$> getPosition

stringInitializerParser :: TerminaParser (Expression ParserAnn)
stringInitializerParser = do
  current <- getState
  startPos <- getPosition
  value <- stringLit
  StringInitializer value . located current startPos <$> getPosition

arrayExprListInitializerParser :: TerminaParser (Expression ParserAnn)
arrayExprListInitializerParser = do
  current <- getState
  startPos <- getPosition
  exprs <- braces (sepBy expressionParser comma)
  ArrayExprListInitializer exprs . located current startPos <$> getPosition

-- -- Task Definition

blockParser :: TerminaParser (Block ParserAnn)
blockParser = do
  current <- getState
  blkStartPos <- getPosition
  compound <- braces $ many blockItemParser
  Block compound . located current blkStartPos <$> getPosition

returnStmtParser :: TerminaParser (Statement ParserAnn)
returnStmtParser = do
  current <- getState
  startPos <- getPosition
  _ <- reserved "return"
  ret <- optionMaybe expressionParser
  endPos <- getPosition
  _ <- semi
  return $ ReturnStmt ret (located current startPos endPos)

continueStmtParser :: TerminaParser (Statement ParserAnn)
continueStmtParser = do
  current <- getState
  startPos <- getPosition
  _ <- reserved "continue"
  ret <- expressionParser
  endPos <- getPosition
  _ <- semi
  return $ ContinueStmt ret (located current startPos endPos)

rebootStmtParser :: TerminaParser (Statement ParserAnn)
rebootStmtParser = do
  current <- getState
  startPos <- getPosition
  _ <- reserved "reboot"
  endPos <- getPosition
  _ <- semi
  return $ RebootStmt (located current startPos endPos)

functionParser :: TerminaParser (AnnASTElement ParserAnn)
functionParser = do
  current <- getState
  modifiers <- many modifierParser
  startPos <- getPosition
  reserved "function"
  name <- identifierParser
  params <- parens (sepBy parameterParser comma)
  typeSpec <- optionMaybe (do
    reservedOp "->"
    typeSpecifierParser)
  blockRet <- blockParser
  Function name params typeSpec blockRet modifiers . located current startPos <$> getPosition

-- | Declaration of a function implemented in C:
-- @extern function name(params) -> ty;@ or
-- @extern noreturn function name(params);@. The parser accepts a return type
-- on a noreturn function so that the type checker can say what is wrong with
-- it.
externFunctionParser :: TerminaParser (AnnASTElement ParserAnn)
externFunctionParser = do
  current <- getState
  modifiers <- many modifierParser
  startPos <- getPosition
  reserved "extern"
  returnMode <- option Returns (NoReturn <$ reserved "noreturn")
  reserved "function"
  name <- identifierParser
  params <- parens (sepBy parameterParser comma)
  typeSpec <- optionMaybe (do
    reservedOp "->"
    typeSpecifierParser)
  endPos <- getPosition
  _ <- semi
  return $ ExternFunction name params typeSpec returnMode modifiers (located current startPos endPos)

constLiteralParser :: TerminaParser (Const ParserAnn)
constLiteralParser = try parseLitFloat <|> parseLitInteger <|> parseLitBool <|> parseLitChar <|> parseLitNull
  where
    parseLitFloat =
      do
        num <- floatParser
        ty <- optionMaybe (reservedOp ":" >> typeSpecifierParser)
        return (F num ty)
    parseLitInteger =
      do
        num <- integerParser
        ty <- optionMaybe (reservedOp ":" >> typeSpecifierParser)
        return (I num ty)
    parseLitBool = (reserved "true" >> return (B True)) <|> (reserved "false" >> return (B False))
    parseLitChar = C <$> charLit
    parseLitNull = reserved "null" >> return Null

constantParser :: TerminaParser (Expression ParserAnn)
constantParser = do
  current <- getState
  startPos <- getPosition
  literal <- constLiteralParser
  Constant literal . located current startPos <$> getPosition

integerParser :: TerminaParser TInteger
integerParser = try hexParser <|> decParser
  where
    hexParser = flip TInteger HexRepr <$> hexadecimal
    decParser = flip TInteger DecRepr <$> decimal

mutableObjDeclarationParser :: TerminaParser (Statement ParserAnn)
mutableObjDeclarationParser = do
  current <- getState
  startPos <- getPosition
  reserved "var"
  name <- identifierParser
  reservedOp ":"
  ty <- typeSpecifierParser
  -- | A mutable object may be declared without an initializer. 
  initializer <- optionMaybe (reservedOp "=" *> expressionParser)
  endPos <- getPosition
  _ <- semi
  return $ Declaration name Mutable ty initializer (located current startPos endPos)

immutableObjDeclarationParser :: TerminaParser (Statement ParserAnn)
immutableObjDeclarationParser = do
  current <- getState
  startPos <- getPosition
  reserved "let"
  name <- identifierParser
  reservedOp ":"
  ty <- typeSpecifierParser
  _ <- reservedOp "="
  initializer <-  expressionParser
  endPos <- getPosition
  _ <- semi
  return $ Declaration name Immutable ty (Just initializer) (located current startPos endPos)

singleExprStmtParser :: TerminaParser (Statement ParserAnn)
singleExprStmtParser = do
  current <- getState
  startPos <- getPosition
  expression <- expressionParser
  endPos <- getPosition
  _ <- semi
  return $ SingleExpStmt expression (located current startPos endPos)

-- | A statement of a block, with its documentation comments before it and
-- after it, if any.
blockItemParser :: TerminaParser (Statement ParserAnn)
blockItemParser = documentedAfter (documented statementParser)

statementParser :: TerminaParser (Statement ParserAnn)
statementParser
  =   try ifElseIfStmtParser
  <|> try mutableObjDeclarationParser
  <|> try immutableObjDeclarationParser
  <|> try assignmentStmtPaser
  <|> try forLoopStmtParser
  <|> try matchStmtParser
  <|> try returnStmtParser
  <|> try continueStmtParser
  <|> try rebootStmtParser
  <|> singleExprStmtParser

assignmentStmtPaser :: TerminaParser (Statement ParserAnn)
assignmentStmtPaser = do
  current <- getState
  startPos <- getPosition
  lval <- objectParser
  _ <- reservedOp "="
  rval <- expressionParser
  endPos <- getPosition
  _ <- semi
  return $ AssignmentStmt lval rval (located current startPos endPos)

-- | A variable a match case binds, which carries its own position so that a use
-- of it inside the body can be traced back to where the case declares it.
boundVarParser :: TerminaParser (Identifier, ParserAnn)
boundVarParser = do
  current <- getState
  startPos <- getPosition
  ident <- objectIdentifierParser
  endPos <- getPosition
  return (ident, located current startPos endPos)

matchCaseParser :: TerminaParser (MatchCase ParserAnn)
matchCaseParser = do
  current <- getState
  startPos <- getPosition
  reserved "case"
  caseId <- identifierParser
  args <- try (parens (sepBy boundVarParser comma)) <|> return []
  reservedOp "=>"
  caseBlk <- blockParser
  MatchCase caseId args caseBlk . located current startPos <$> getPosition

defaultCaseParser :: TerminaParser (DefaultCase ParserAnn)
defaultCaseParser = do
  current <- getState
  startPos <- getPosition
  reserved "case"
  reserved "_"
  reservedOp "=>"
  caseBlk <- blockParser
  DefaultCase caseBlk . located current startPos <$> getPosition

-- lookAhead parser that checks for "case _"
isDefaultCase :: TerminaParser ()
isDefaultCase = do
  reserved "case"
  reserved "_" -- just check that "_" is next

-- lookAhead parser that accepts "case" not followed by "_"
isRegularCase :: TerminaParser ()
isRegularCase = do
  reserved "case"
  notFollowedBy (reserved "_")

parseCases :: [MatchCase ParserAnn] -> TerminaParser ([MatchCase ParserAnn], Maybe (DefaultCase ParserAnn))
parseCases acc = do
  -- A case may have a documentation comment before it
  docPos <- getPosition
  doc <- optionMaybe (docComment DocBefore)
  -- Check for default case first
  mDefault <- optionMaybe $ try $ lookAhead isDefaultCase >> defaultCaseParser
  case mDefault of
    Just defCase -> do
      documentedDefault <- withDoc doc defCase
      return (reverse acc, Just documentedDefault)
    Nothing -> do
      -- Check for normal case
      canContinue <- optionMaybe $ try $ lookAhead isRegularCase
      case (canContinue, doc) of
        (Just _, _) -> do
          c <- matchCaseParser >>= withDoc doc -- If this fails, it should propagate!
          parseCases (c : acc)
        (Nothing, Nothing) -> return (reverse acc, Nothing)
        (Nothing, Just _) -> failAt docPos (misplacedDocComment DocBefore)

matchStmtParser :: TerminaParser (Statement ParserAnn)
matchStmtParser = do
  current <- getState
  startPos <- getPosition
  reserved "match"
  matchExpression <- expressionParser
  (cases, defaultCase) <- braces (do
      c <- documented matchCaseParser
      (cs, dc) <- parseCases [c]
      return (cs, dc))
  MatchStmt matchExpression cases defaultCase . located current startPos <$> getPosition

elseIfParser :: TerminaParser (CondElseIf ParserAnn)
elseIfParser = do
  current <- getState
  startPos <- getPosition
  _ <- reserved "else"
  _ <- reserved "if"
  expression <- expressionParser
  elseIfBlk <- blockParser
  CondElseIf expression elseIfBlk . located current startPos <$> getPosition

ifElseIfStmtParser :: TerminaParser (Statement ParserAnn)
ifElseIfStmtParser = do
  current <- getState
  startPos <- getPosition
  _ <- reserved "if"
  expression <- expressionParser
  ifBlkStartPos <- getPosition
  ifCompound <- braces $ many blockItemParser
  ifBlkEndPos <- getPosition
  let ifBlk = Block ifCompound (located current ifBlkStartPos ifBlkEndPos)
  let condIf = CondIf expression ifBlk (located current startPos ifBlkEndPos)
  -- A documentation comment after the block belongs to the branch that follows,
  -- if any, and otherwise to the next statement.
  elseIfs <- many $ try (do
    doc <- optionMaybe (docComment DocBefore)
    elseIfParser >>= withDoc doc)
  mCondElse <- optionMaybe (do
    doc <- try (optionMaybe (docComment DocBefore) <* lookAhead (reserved "else"))
    elseBlkStartPos <- getPosition
    _ <- reserved "else"
    elseBlk <- blockParser
    (CondElse elseBlk . located current elseBlkStartPos <$> getPosition) >>= withDoc doc)
  IfElseStmt condIf elseIfs mCondElse . located current startPos <$> getPosition

forLoopStmtParser :: TerminaParser (Statement ParserAnn)
forLoopStmtParser = do
  current <- getState
  startPos <- getPosition
  _ <- reserved "for"
  identifier <- identifierParser
  reservedOp ":"
  ty <- typeSpecifierParser
  _ <- reserved "in"
  start <- expressionParser
  _ <- reservedOp ".."
  end <- expressionParser
  breakCondition <- optionMaybe (do
    reserved "while"
    expressionParser)
  forBlk <- blockParser
  ForLoopStmt identifier ty start end breakCondition forBlk . located current startPos <$> getPosition

taskDeclParser :: TerminaParser (Global ParserAnn)
taskDeclParser = do
  current <- getState
  modifiers <- many modifierParser
  startPos <- getPosition
  reserved "task"
  identifier <- identifierParser
  reservedOp ":"
  typeSpecifier <- typeSpecifierParser
  initializer <- optionMaybe (do
    reservedOp "="
    expressionParser)
  endPos <- getPosition
  _ <- semi
  return $ Task identifier typeSpecifier initializer modifiers (located current startPos endPos)

emitterDeclParser :: TerminaParser (Global ParserAnn)
emitterDeclParser = do
  current <- getState
  modifiers <- many modifierParser
  startPos <- getPosition
  reserved "emitter"
  identifier <- identifierParser
  reservedOp ":"
  typeSpecifier <- typeSpecifierParser
  initializer <- optionMaybe (do
    reservedOp "="
    expressionParser)
  endPos <- getPosition
  _ <- semi
  return $ Emitter identifier typeSpecifier initializer modifiers (located current startPos endPos)

channelDeclParser :: TerminaParser (Global ParserAnn)
channelDeclParser = do
  current <- getState
  modifiers <- many modifierParser
  startPos <- getPosition
  reserved "channel"
  identifier <- identifierParser
  reservedOp ":"
  typeSpecifier <- typeSpecifierParser
  initializer <- optionMaybe (do
    reservedOp "="
    expressionParser)
  endPos <- getPosition
  _ <- semi
  return $ Channel identifier typeSpecifier initializer modifiers (located current startPos endPos)

resourceDeclParser :: TerminaParser (Global ParserAnn)
resourceDeclParser = do
  current <- getState
  modifiers <- many modifierParser
  startPos <- getPosition
  reserved "resource"
  identifier <- identifierParser
  reservedOp ":"
  typeSpecifier <- typeSpecifierParser
  initializer <- optionMaybe (do
    reservedOp "="
    expressionParser)
  endPos <- getPosition
  _ <- semi
  return $ Resource identifier typeSpecifier initializer modifiers (located current startPos endPos)

handlerDeclParser :: TerminaParser (Global ParserAnn)
handlerDeclParser = do
  current <- getState
  modifiers <- many modifierParser
  startPos <- getPosition
  reserved "handler"
  identifier <- identifierParser
  reservedOp ":"
  typeSpecifier <- typeSpecifierParser
  initializer <- optionMaybe (do
    reservedOp "="
    expressionParser)
  endPos <- getPosition
  _ <- semi
  return $ Handler identifier typeSpecifier initializer modifiers (located current startPos endPos)

testDeclParser :: TerminaParser (Global ParserAnn)
testDeclParser = do
  current <- getState
  modifiers <- many modifierParser
  startPos <- getPosition
  reserved "test"
  identifier <- identifierParser
  reservedOp ":"
  typeSpecifier <- typeSpecifierParser
  initializer <- optionMaybe (do
    reservedOp "="
    expressionParser)
  endPos <- getPosition
  _ <- semi
  return $ Test identifier typeSpecifier initializer modifiers (located current startPos endPos)

constExprDeclParser :: TerminaParser (Global ParserAnn)
constExprDeclParser = do
  current <- getState
  modifiers <- many modifierParser
  startPos <- getPosition
  reserved "constexpr"
  identifier <- identifierParser
  reservedOp ":"
  typeSpecifier <- typeSpecifierParser
  _ <- reservedOp "="
  initializer <- expressionParser
  endPos <- getPosition
  _ <- semi
  return $ ConstExpr identifier typeSpecifier initializer modifiers (located current startPos endPos)

constDeclParser :: TerminaParser (Global ParserAnn)
constDeclParser = do
  current <- getState
  modifiers <- many modifierParser
  startPos <- getPosition
  reserved "const"
  identifier <- identifierParser
  reservedOp ":"
  typeSpecifier <- typeSpecifierParser
  _ <- reservedOp "="
  initializer <- expressionParser
  endPos <- getPosition
  _ <- semi
  return $ Const identifier typeSpecifier initializer modifiers (located current startPos endPos)

globalDeclParser :: TerminaParser (AnnASTElement  ParserAnn)
globalDeclParser = do
  g <- try taskDeclParser
    <|> try resourceDeclParser
    <|> try handlerDeclParser
    <|> try emitterDeclParser
    <|> try channelDeclParser
    <|> try testDeclParser
    <|> try constExprDeclParser
    <|> constDeclParser
  return $ GlobalDeclaration g

typeDefintionParser :: TerminaParser (AnnASTElement ParserAnn)
typeDefintionParser = structDefinitionParser <|> enumDefinitionParser <|> classDefinitionParser <|> interfaceDefinitionParser

fieldDefinitionParser :: TerminaParser (FieldDefinition ParserAnn)
fieldDefinitionParser = do
  current <- getState
  startPos <- getPosition
  identifier <- identifierParser
  _ <- reservedOp ":"
  typeSpecifier <- typeSpecifierParser
  endPos <- getPosition
  _ <- semi
  return $ FieldDefinition identifier typeSpecifier (located current startPos endPos)

structDefinitionParser :: TerminaParser (AnnASTElement ParserAnn)
structDefinitionParser = do
  current <- getState
  modifiers <- many modifierParser
  startPos <- getPosition
  reserved "struct"
  identifier <- identifierParser
  fields <- braces (many1 $ documentedAfter (documented (try fieldDefinitionParser)))
  endPos <- getPosition
  _ <- semi
  return $ TypeDefinition (Struct identifier fields modifiers) (located current startPos endPos)

classFieldDefinitionParser :: TerminaParser (ClassMember ParserAnn)
classFieldDefinitionParser = do
  current <- getState
  startPos <- getPosition
  identifier <- identifierParser
  _ <- reservedOp ":"
  typeSpecifier <- typeSpecifierParser
  endPos <- getPosition
  _ <- semi
  return $ ClassField (FieldDefinition identifier typeSpecifier (located current startPos endPos))

selfAccessKindParser :: TerminaParser AccessKind
selfAccessKindParser =
  try (reserved "&mut" >> reserved "self" >> return Mutable) <|>
  try (reserved "&priv" >> reserved "self" >> return Private) <|>
  (reserved "&self" >> return Immutable)


classMethodParser :: TerminaParser (ClassMember ParserAnn)
classMethodParser = do
  current <- getState
  startPos <- getPosition
  reserved "method"
  name <- identifierParser
  (ak, params) <- parens $
    do
      ak <- selfAccessKindParser
      params <- option [] (comma >> sepBy parameterParser comma)
      return (ak, params)
  typeSpec <- optionMaybe (reservedOp "->" >> typeSpecifierParser)
  blockRet <- blockParser
  ClassMethod ak name params typeSpec blockRet . located current startPos <$> getPosition

classActionParser :: TerminaParser (ClassMember ParserAnn)
classActionParser = do
  current <- getState
  startPos <- getPosition
  reserved "action"
  name <- identifierParser
  (ak, param) <- parens $
    do
      ak <- selfAccessKindParser
      param <- optionMaybe (comma >> parameterParser)
      return (ak, param)
  blockRet <- blockParser
  ClassAction ak name param blockRet . located current startPos <$> getPosition

-- | A case of a test class. The access to self and the parameters are
-- parsed as in a method, and the type checker rejects a case with
-- parameters.
classCaseParser :: TerminaParser (ClassMember ParserAnn)
classCaseParser = do
  current <- getState
  startPos <- getPosition
  reserved "case"
  name <- identifierParser
  (ak, params) <- parens $
    do
      ak <- selfAccessKindParser
      params <- option [] (comma >> sepBy parameterParser comma)
      return (ak, params)
  blockRet <- blockParser
  ClassCase ak name params blockRet . located current startPos <$> getPosition

classProcedureParser :: TerminaParser (ClassMember ParserAnn)
classProcedureParser = do
  current <- getState
  startPos <- getPosition
  reserved "procedure"
  name <- identifierParser
  (ak, params) <- parens $
    do
      ak <- selfAccessKindParser
      params <- option [] (comma >> sepBy parameterParser comma)
      return (ak, params)
  blockRet <- blockParser
  ClassProcedure ak name params blockRet . located current startPos <$> getPosition

interfaceProcedureParser :: TerminaParser (InterfaceMember ParserAnn)
interfaceProcedureParser = do
  current <- getState
  startPos <- getPosition
  reserved "procedure"
  name <- identifierParser
  (ak, params) <- parens $
    do
      ak <- selfAccessKindParser
      params <- option [] (comma >> sepBy parameterParser comma)
      return (ak, params)
  _ <- semi
  -- | TODO: See if we allow modifiers in interface procedures to be set
  -- by the programmers
  InterfaceProcedure ak name params [] . located current startPos <$> getPosition

classViewerParser :: TerminaParser (ClassMember ParserAnn)
classViewerParser = do
  current <- getState
  startPos <- getPosition
  reserved "viewer"
  name <- identifierParser
  params <- parens viewerParamsParser
  typeSpec <- optionMaybe (reservedOp "->" >>  typeSpecifierParser)
  blockRet <- blockParser
  ClassViewer name params typeSpec blockRet . located current startPos <$> getPosition
  where
    viewerParamsParser :: TerminaParser [Parameter ParserAnn]
    viewerParamsParser =
      reserved "&self" >> option [] (comma >> sepBy parameterParser comma)

interfaceDefinitionParser :: TerminaParser (AnnASTElement ParserAnn)
interfaceDefinitionParser = do
  current <- getState
  modifiers <- many modifierParser
  startPos <- getPosition
  reserved "interface"
  identifier <- identifierParser
  extends <- option [] (reserved "extends" >> sepBy identifierParser comma)
  procedures <- braces (many1 (documented interfaceProcedureParser))
  endPos <- getPosition
  _ <- semi
  return $ TypeDefinition (Interface RegularInterface identifier extends procedures modifiers) (located current startPos endPos)

classDefinitionParser :: TerminaParser (AnnASTElement ParserAnn)
classDefinitionParser = do
  current <- getState
  modifiers <- many modifierParser
  startPos <- getPosition
  classKind <- classKindParser
  reserved "class"
  identifier <- identifierParser
  provides <- option [] (reserved "provides" >> sepBy identifierParser comma)
  fields <-
    braces (many1 $ documented (classMethodParser
      <|> classViewerParser
      <|> classProcedureParser
      <|> classActionParser
      <|> classCaseParser
      <|> documentedAfter classFieldDefinitionParser))
  endPos <- getPosition
  _ <- semi
  return $ TypeDefinition (Class classKind identifier fields provides modifiers) (located current startPos endPos)
  where
    classKindParser :: TerminaParser ClassKind
    classKindParser =
      (reserved "task" >> return TaskClass)
      <|> (reserved "resource" >> return ResourceClass)
      <|> (reserved "handler" >> return HandlerClass)
      <|> (reserved "test" >> return TestClass)

variantDefinitionParser :: TerminaParser (EnumVariant ParserAnn)
variantDefinitionParser = do
  current <- getState
  startPos <- getPosition
  identifier <- identifierParser
  params <- try (parens (sepBy1 typeSpecifierParser comma)) <|> return []
  EnumVariant identifier params . located current startPos <$> getPosition

-- | The variants of an enum, separated by commas. The documentation comment
-- after a variant goes before or after its comma, or after the last variant.
variantListParser :: TerminaParser [EnumVariant ParserAnn]
variantListParser = do
  variant <- documented (try variantDefinitionParser)
  beforeComma <- docAfter
  more <- optionMaybe comma
  doc <- maybe docAfter (return . Just) beforeComma
  documentedVariant <- withDoc doc variant
  case more of
    Nothing -> return [documentedVariant]
    Just _ -> (documentedVariant :) <$> variantListParser

enumDefinitionParser :: TerminaParser (AnnASTElement ParserAnn)
enumDefinitionParser = do
  current <- getState
  modifiers <- many modifierParser
  startPos <- getPosition
  reserved "enum"
  identifier <- identifierParser
  variants <- braces variantListParser
  endPos <- getPosition
  _ <- semi
  return $ TypeDefinition (Enum identifier variants modifiers) (located current startPos endPos)

-- | Top Level parser. Of the declarations, only a global one takes a
-- documentation comment after it.
topLevel :: TerminaParser (AnnotatedProgram ParserAnn)
topLevel = many $ documented (
  try functionParser <|> try externFunctionParser <|> documentedAfter (try globalDeclParser)
  <|> try typeDefintionParser)

moduleIdentifierParser :: TerminaParser [ String ]
moduleIdentifierParser = do
  path <- sepBy1 (many1 (alphaNum <|> char '_')) dot
  endPos <- getPosition
  let invalid msg = failAt endPos (msg ++ ": " ++ L.intercalate "." path ++ ".")
  if not (all startsLower path) then
    invalid "Namespace and module names begin with a lowercase letter"
  else if any (any isUpper) path then
    invalid "Namespace and module names only contain lowercase letters, digits and underscores"
  else if any ("__" `L.isInfixOf`) path then
    invalid "Namespace and module names cannot contain two consecutive underscores"
  else case path of
    "termina" : _ -> invalid "User-defined modules are not allowed in the termina namespace"
    _ -> return path
  where
    startsLower (c : _) = isLower c
    startsLower [] = False

moduleImportParser :: TerminaParser (ModuleImport ParserAnn)
moduleImportParser = do
  current <- getState
  startPos <- getPosition
  reserved "import"
  ident <- moduleIdentifierParser
  endPos <- getPosition
  _ <- semi
  return $ ModuleImport ident (located current startPos endPos)

contents :: TerminaParser a -> TerminaParser a
contents p = wspcs *> p <* eof

terminaModuleParser :: TerminaParser (TerminaModule ParserAnn)
terminaModuleParser =
  wspcs *> (Termina <$> optionMaybe (docComment DocModule) <*> many moduleImportParser <*> contents topLevel)

-- | Parses the text of a module, given its qualified name and its file. The
-- positions of the tree and of the error count characters, as the error
-- printer and the LSP do, and not the columns of Parsec.
parseTerminaModule :: QualifiedName -> FilePath -> String -> Either ParseError (TerminaModule ParserAnn)
parseTerminaModule qname file src =
  case runParser terminaModuleParser qname file src of
    Left err -> Left (PE.setErrorPos (toCharacter (errorPos err)) err)
    Right ast -> Right ((fmap characterAnn ast) { moduleDoc = characterDoc <$> moduleDoc ast })

  where

    toCharacter = characterColumn src
    characterAnn (ParserAnn loc doc) = ParserAnn (characterLocation loc) (characterDoc <$> doc)
    characterDoc d = d { docLocation = characterLocation (docLocation d) }
    characterLocation (Position q start end) = Position q (toCharacter start) (toCharacter end)
    characterLocation loc = loc

-- | Turns the column of a position in a text, as Parsec counts it, into the
-- number of the character in its line. Parsec ('updatePosChar') moves a tab to
-- the next column 1 + 8k, so both differ only on a line with a tab.
characterColumn :: String -> SourcePos -> SourcePos
characterColumn src
  | '\t' `notElem` src = id
  | otherwise = \pos ->
      case IM.lookup (sourceLine pos) linesOfText of
        Just line -> setSourceColumn pos (1 + length (takeWhile (< sourceColumn pos) (scanl advance 1 line)))
        Nothing -> pos

  where

    linesOfText = IM.fromList (zip [1 ..] (lines src))
    -- The column of Parsec after a character, from the one before it.
    advance column '\t' = column + 8 - ((column - 1) `mod` 8)
    advance column _ = column + 1

getStartPosition :: ParserAnn -> SourcePos
getStartPosition ann = case parsedLocation ann of
  Position _ startPos _ -> startPos
  _ -> error "Internal error: expected Position annotation (this should not happen)"

getEndPosition :: ParserAnn -> SourcePos
getEndPosition ann = case parsedLocation ann of
  Position _ _ endPos -> endPos
  _ -> error "Internal error: expected Position annotation (this should not happen)"