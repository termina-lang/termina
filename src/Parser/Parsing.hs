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

import Data.Functor
import Text.Parsec.Expr
import qualified Data.List as L
import Control.Monad
import Data.Char
import Utils.Annotations
import Parser.Types

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
      ,"unit"]
      ++ -- Struct and enum types
             ["struct", "enum"]
      ++ -- Class types
             ["class"]
      ++ -- Box Subtyping
             ["box"]
      ++ -- Fixed-location subtyping
             ["loc"]
      ++ -- Ports Subtyping
             ["access", "sink", "in", "out"]
      ++ -- Global declarations
             ["task", "function", "handler", "resource", "const", "constexpr"]
      ++ -- Stmt
             ["var", "let", "match", "case", "for", "if", "else", "return", "continue", "while"]
      ++ -- Trigger
             ["triggers"]
      ++ -- Provide/extend
             ["provides", "extends"]
      ++ -- Constants
             ["true", "false", "null"]
      ++ -- Modules
             ["import"]
      ++ -- Class methods
             ["procedure", "viewer", "method", "action"]
      ++ -- Casting keyword
             ["as"]
      ++ -- is variant operator
             ["is"]
      ++ -- system-wide reserved names
             ["termina", "option", "config"]

    langDef =
      Lang.emptyDef{ Tok.commentStart = "/*"
                   , Tok.commentEnd = "*/"
                   , Tok.commentLine = "//"
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
                      ,"^" -- BitwiseXor
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

-- Lexemes

wspcs :: TerminaParser ()
wspcs = Tok.whiteSpace lexer

braces :: TerminaParser a -> TerminaParser a
braces = Tok.braces lexer

brackets :: TerminaParser a -> TerminaParser a
brackets = Tok.brackets lexer

parens :: TerminaParser a -> TerminaParser a
parens = Tok.parens lexer

angles :: TerminaParser a -> TerminaParser a
angles = Tok.angles lexer

comma :: TerminaParser String
comma = Tok.comma lexer

semi :: TerminaParser String
semi = Tok.semi lexer

dot :: TerminaParser String
dot = Tok.dot lexer

stringLit :: TerminaParser String
stringLit = Tok.stringLiteral lexer

charLit :: TerminaParser Char
charLit = Tok.charLiteral lexer

reserved :: String -> TerminaParser ()
reserved = Tok.reserved lexer

reservedOp :: String -> TerminaParser ()
reservedOp = Tok.reservedOp lexer

identifierParser :: TerminaParser String
identifierParser = Tok.identifier lexer

tails :: [a] -> [[a]]
tails = L.tails

number :: Integer -> TerminaParser Char -> TerminaParser Integer
number base baseDigit = do
    digits <- many1 baseDigit
    let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
    seq n (return n)

sign :: TerminaParser (Integer -> Integer)
sign = (char '-' >> return negate)
  <|> (char '+' >> return id)
  <|> return id

-- | TerminaParser for integer decimal numbers
-- This parser is used when defining regular integer literals
decimal :: TerminaParser Integer
decimal =  Tok.lexeme lexer $ do
  f <- Tok.lexeme lexer sign
  n <- number 10 digit
  return (f n)

hexadecimal :: TerminaParser Integer
hexadecimal = Tok.lexeme lexer $
  char '0' >> oneOf "xX" >> number 16 hexDigit

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
  <|> constSubtypeParser
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
  <|> (reserved "unit" >> return TSUnit)

objectIdentifierParser :: TerminaParser Identifier
objectIdentifierParser = try ((char '_' >> identifierParser) <&> ('_' :)) <|> identifierParser

parameterParser :: TerminaParser (Parameter ParserAnn)
parameterParser = do
  identifier <- objectIdentifierParser
  reservedOp ":"
  Parameter identifier <$> typeSpecifierParser

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
    StructInitializer assignments typeSpecifier . Position current startpos <$> getPosition
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
        FieldValueAssignment identifier expr . Position current startPos <$> getPosition

      fieldAddressParser :: TerminaParser (FieldAssignment ParserAnn)
      fieldAddressParser = do
        current <- getState
        startPos <- getPosition
        identifier <- identifierParser
        _ <- reservedOp "@"
        addr <- integerParser
        FieldAddressAssignment identifier addr . Position current startPos <$> getPosition

      fieldAccessPortConnectionParser :: TerminaParser (FieldAssignment ParserAnn)
      fieldAccessPortConnectionParser = do
        current <- getState
        startPos <- getPosition
        identifier <- identifierParser
        _ <- reservedOp "<->"
        port <- identifierParser
        FieldPortConnection AccessPortConnection identifier port . Position current startPos <$> getPosition

      fieldInboundPortConnectionParser :: TerminaParser (FieldAssignment ParserAnn)
      fieldInboundPortConnectionParser = do
        current <- getState
        startPos <- getPosition
        identifier <- identifierParser
        _ <- reservedOp "<-"
        port <- identifierParser
        FieldPortConnection InboundPortConnection identifier port . Position current startPos <$> getPosition

      fieldOutboundPortConnectionParser :: TerminaParser (FieldAssignment ParserAnn)
      fieldOutboundPortConnectionParser = do
        current <- getState
        startPos <- getPosition
        identifier <- identifierParser
        _ <- reservedOp "->"
        port <- identifierParser
        FieldPortConnection OutboundPortConnection identifier port . Position current startPos <$> getPosition

-- | Parser for an element modifier
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
  initializer <- optionMaybe (parens constLiteralParser)
  _ <- reservedOp "]"
  return $ Modifier identifier initializer

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

constSubtypeParser :: TerminaParser (TypeSpecifier ParserAnn)
constSubtypeParser = reservedOp "const" >> TSConstSubtype <$> typeSpecifierParser

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
          return $ \l r -> BinOp f l r (Position current (getStartPosition (getAnnotation l)) (getEndPosition (getAnnotation r))))
    castingPostfix = Ex.Postfix (do
          current <- getState
          _ <- reserved "as"
          typeSpecificer <- typeSpecifierParser
          endPos <- getPosition
          return $ \parent -> Casting parent typeSpecificer (Position current (getStartPosition (getAnnotation parent)) endPos))

functionCallParser :: TerminaParser (Expression ParserAnn)
functionCallParser = do
  current <- getState
  startPos <- getPosition
  ident <- identifierParser
  params <- parens (sepBy (try expressionParser) comma)
  FunctionCall ident params . Position current startPos <$> getPosition

builtinVariantExprParser :: TerminaParser (Expression ParserAnn)
builtinVariantExprParser =
  (do
    current <- getState
    startPos <- getPosition
    _ <- reserved "None"
    MonadicVariantInitializer None . Position current startPos <$> getPosition) <|>
  (do
    current <- getState
    startPos <- getPosition
    _ <- reserved "Some"
    someExpr <- parens expressionParser
    MonadicVariantInitializer (Some someExpr) . Position current startPos <$> getPosition) <|>
  (do
    current <- getState
    startPos <- getPosition
    _ <- reserved "Ok"
    okExpr <- parens expressionParser
    MonadicVariantInitializer (Ok okExpr) . Position current startPos <$> getPosition) <|>
  (do
    current <- getState
    startPos <- getPosition
    _ <- reserved "Error"
    errorExpr <- parens expressionParser
    MonadicVariantInitializer (Error errorExpr) . Position current startPos <$> getPosition) <|>
  (do
    current <- getState
    startPos <- getPosition
    _ <- reserved "Success"
    MonadicVariantInitializer Success . Position current startPos <$> getPosition) <|>
  (do
    current <- getState
    startPos <- getPosition
    _ <- reserved "Failure"
    failureExpr <- parens expressionParser
    MonadicVariantInitializer (Failure failureExpr) . Position current startPos <$> getPosition)

enumVariantExprParser :: TerminaParser (Expression ParserAnn)
enumVariantExprParser = do
  current <- getState
  startPos <- getPosition
  enum <- identifierParser
  _ <- reservedOp "::"
  variant <- identifierParser
  parameterList <-
    option [] (parens (sepBy (try expressionParser) comma))
  EnumVariantInitializer enum variant parameterList . Position current startPos <$> getPosition

isEnumVariantExprParser :: TerminaParser (Expression ParserAnn)
isEnumVariantExprParser = do
  current <- getState
  startPos <- getPosition
  object <- objectParser
  _ <- reserved "is"
  enum <- identifierParser
  _ <- reservedOp "::"
  variant <- identifierParser
  IsEnumVariantExpression object enum variant . Position current startPos <$> getPosition

isMonadicVariantExprParser :: TerminaParser (Expression ParserAnn)
isMonadicVariantExprParser =
  try (do
    current <- getState
    startPos <- getPosition
    object <- objectParser
    _ <- reserved "is"
    _ <- reserved "None"
    IsMonadicVariantExpression object NoneLabel . Position current startPos <$> getPosition) <|>
  try (do
    current <- getState
    startPos <- getPosition
    object <- objectParser
    _ <- reserved "is"
    _ <- reserved "Some"
    IsMonadicVariantExpression object SomeLabel . Position current startPos <$> getPosition) <|>
  try (do
    current <- getState
    startPos <- getPosition
    object <- objectParser
    _ <- reserved "is"
    _ <- reserved "Ok"
    IsMonadicVariantExpression object OkLabel . Position current startPos <$> getPosition) <|>
  try (do
    current <- getState
    startPos <- getPosition
    object <- objectParser
    _ <- reserved "is"
    _ <- reserved "Error"
    IsMonadicVariantExpression object ErrorLabel . Position current startPos <$> getPosition) <|>
  try (do
    current <- getState
    startPos <- getPosition
    object <- objectParser
    _ <- reserved "is"
    _ <- reserved "Success"
    IsMonadicVariantExpression object SuccessLabel . Position current startPos <$> getPosition) <|>
  (do
    current <- getState
    startPos <- getPosition
    object <- objectParser
    _ <- reserved "is"
    _ <- reserved "Failure"
    IsMonadicVariantExpression object FailureLabel . Position current startPos <$> getPosition)

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
  ReferenceExpression Mutable object . Position current startPos <$> getPosition

referenceExprParser :: TerminaParser (Expression ParserAnn)
referenceExprParser = do
  current <- getState
  startPos <- getPosition
  _ <- reservedOp "&"
  object <- objectParser
  ReferenceExpression Immutable object . Position current startPos <$> getPosition

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
    return $ Variable ident (Position current startPos (incSourceColumn startPos (length ident))))
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
            return $ \parent ->  ArraySlice parent low up (Position current (getStartPosition (getAnnotation parent)) endPos)
          ) <|> (do
            current <- getState
            index <- brackets expressionParser
            endPos <- getPosition
            return $ \parent ->  ArrayIndexExpression parent index (Position current (getStartPosition (getAnnotation parent)) endPos)
          ))
    dereferenceMemberAccessPostfix
      = Ex.Postfix (do
        current <- getState
        _ <- reservedOp "->"
        member <- identifierParser
        endPos <- getPosition
        return $ \parent -> DereferenceMemberAccess parent member (Position current (getStartPosition (getAnnotation parent)) endPos))
    memberAccessPostfix
      = Ex.Postfix (do
        current <- getState
        _ <- reservedOp "."
        member <- identifierParser
        endPos <- getPosition
        return $ \parent ->  MemberAccess parent member (Position current (getStartPosition (getAnnotation parent)) endPos))
    dereferencePrefix
      = Ex.Prefix (do
        current <- getState
        startPos <- getPosition
        _ <- reservedOp "*"
        return $ \child -> Dereference child (Position current startPos (getEndPosition (getAnnotation child))))
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
              AccessObject obj -> AccessObject (ArraySlice obj low up (Position current (getStartPosition (getAnnotation parent)) endPos))
              _ -> error "Unexpected member access to a non object"
          ) <|> (do
            current <- getState
            index <- brackets expressionParser
            endPos <- getPosition
            return $ \parent -> case parent of
              AccessObject obj -> AccessObject (ArrayIndexExpression obj index (Position current (getStartPosition (getAnnotation parent)) endPos))
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
          maybe (AccessObject (DereferenceMemberAccess obj member (Position current startPos endPos))) (flip (DerefMemberFunctionCall obj member) (Position current startPos endPos)) params
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
          maybe (AccessObject (MemberAccess obj member (Position current startPos endPos))) (flip (MemberFunctionCall obj member) (Position current startPos endPos)) params
        _ -> error "Unexpected member access to a non object"))
    dereferencePrefix
      = Ex.Prefix (do
      current <- getState
      startPos <- getPosition
      _ <- reservedOp "*"
      return (\child -> case child of
        AccessObject obj ->
          let endPos = getStartPosition (getAnnotation child) in
          AccessObject (Dereference obj (Position current startPos endPos))
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
  ArrayInitializer value size . Position current startPos <$> getPosition

stringInitializerParser :: TerminaParser (Expression ParserAnn)
stringInitializerParser = do
  current <- getState
  startPos <- getPosition
  value <- stringLit
  StringInitializer value . Position current startPos <$> getPosition

arrayExprListInitializerParser :: TerminaParser (Expression ParserAnn)
arrayExprListInitializerParser = do
  current <- getState
  startPos <- getPosition
  exprs <- braces (sepBy expressionParser comma)
  ArrayExprListInitializer exprs . Position current startPos <$> getPosition

-- -- Task Definition

blockParser :: TerminaParser (Block ParserAnn)
blockParser = do
  current <- getState
  blkStartPos <- getPosition
  compound <- braces $ many blockItemParser
  Block compound . Position current blkStartPos <$> getPosition

returnStmtParser :: TerminaParser (Statement ParserAnn)
returnStmtParser = do
  current <- getState
  startPos <- getPosition
  _ <- reserved "return"
  ret <- optionMaybe expressionParser
  endPos <- getPosition
  _ <- semi
  return $ ReturnStmt ret (Position current startPos endPos)

continueStmtParser :: TerminaParser (Statement ParserAnn)
continueStmtParser = do
  current <- getState
  startPos <- getPosition
  _ <- reserved "continue"
  ret <- expressionParser
  endPos <- getPosition
  _ <- semi
  return $ ContinueStmt ret (Position current startPos endPos)

rebootStmtParser :: TerminaParser (Statement ParserAnn)
rebootStmtParser = do
  current <- getState
  startPos <- getPosition
  _ <- reserved "reboot"
  endPos <- getPosition
  _ <- semi
  return $ RebootStmt (Position current startPos endPos)

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
  Function name params typeSpec blockRet modifiers . Position current startPos <$> getPosition

constLiteralParser :: TerminaParser (Const ParserAnn)
constLiteralParser = parseLitInteger <|> parseLitBool <|> parseLitChar <|> parseLitNull
  where
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
  Constant literal . Position current startPos <$> getPosition

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
  _ <- reservedOp "="
  initializer <-  expressionParser
  endPos <- getPosition
  _ <- semi
  return $ Declaration name Mutable ty initializer (Position current startPos endPos)

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
  return $ Declaration name Immutable ty initializer (Position current startPos endPos)

singleExprStmtParser :: TerminaParser (Statement ParserAnn)
singleExprStmtParser = do
  current <- getState
  startPos <- getPosition
  expression <- expressionParser
  endPos <- getPosition
  _ <- semi
  return $ SingleExpStmt expression (Position current startPos endPos)

blockItemParser :: TerminaParser (Statement ParserAnn)
blockItemParser
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
  return $ AssignmentStmt lval rval (Position current startPos endPos)

matchCaseParser :: TerminaParser (MatchCase ParserAnn)
matchCaseParser = do
  current <- getState
  startPos <- getPosition
  reserved "case"
  caseId <- identifierParser
  args <- try (parens (sepBy identifierParser comma)) <|> return []
  reservedOp "=>"
  caseBlk <- blockParser
  MatchCase caseId args caseBlk . Position current startPos <$> getPosition

defaultCaseParser :: TerminaParser (DefaultCase ParserAnn)
defaultCaseParser = do
  current <- getState
  startPos <- getPosition
  reserved "case"
  reserved "_"
  reservedOp "=>"
  caseBlk <- blockParser
  DefaultCase caseBlk . Position current startPos <$> getPosition

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
  -- Check for default case first
  mDefault <- optionMaybe $ try $ lookAhead isDefaultCase >> defaultCaseParser
  case mDefault of
    Just defCase -> return (reverse acc, Just defCase)
    Nothing -> do
      -- Check for normal case
      canContinue <- optionMaybe $ try $ lookAhead isRegularCase
      case canContinue of
        Just _ -> do
          c <- matchCaseParser -- If this fails, it should propagate!
          parseCases (c : acc)
        Nothing -> return (reverse acc, Nothing)

matchStmtParser :: TerminaParser (Statement ParserAnn)
matchStmtParser = do
  current <- getState
  startPos <- getPosition
  reserved "match"
  matchExpression <- expressionParser
  (cases, defaultCase) <- braces (do
      c <- matchCaseParser
      (cs, dc) <- parseCases [c]
      return (cs, dc))
  MatchStmt matchExpression cases defaultCase . Position current startPos <$> getPosition

elseIfParser :: TerminaParser (ElseIf ParserAnn)
elseIfParser = do
  current <- getState
  startPos <- getPosition
  _ <- reserved "else"
  _ <- reserved "if"
  expression <- expressionParser
  elseIfBlk <- blockParser
  ElseIf expression elseIfBlk . Position current startPos <$> getPosition

ifElseIfStmtParser :: TerminaParser (Statement ParserAnn)
ifElseIfStmtParser = do
  current <- getState
  startPos <- getPosition
  _ <- reserved "if"
  expression <- expressionParser
  ifBlkStartPos <- getPosition
  ifCompound <- braces $ many blockItemParser
  ifBlk <- Block ifCompound . Position current ifBlkStartPos <$> getPosition
  elseIfs <- many $ try elseIfParser
  elseBlk <- option Nothing (do
    _ <- reserved "else"
    Just <$> blockParser)
  IfElseStmt expression ifBlk elseIfs elseBlk . Position current startPos <$> getPosition

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
  ForLoopStmt identifier ty start end breakCondition forBlk . Position current startPos <$> getPosition

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
  return $ Task identifier typeSpecifier initializer modifiers (Position current startPos endPos)

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
  return $ Emitter identifier typeSpecifier initializer modifiers (Position current startPos endPos)

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
  return $ Channel identifier typeSpecifier initializer modifiers (Position current startPos endPos)

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
  return $ Resource identifier typeSpecifier initializer modifiers (Position current startPos endPos)

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
  return $ Handler identifier typeSpecifier initializer modifiers (Position current startPos endPos)

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
  return $ ConstExpr identifier typeSpecifier initializer modifiers (Position current startPos endPos)

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
  return $ Const identifier typeSpecifier initializer modifiers (Position current startPos endPos)

globalDeclParser :: TerminaParser (AnnASTElement  ParserAnn)
globalDeclParser = do
  g <- try taskDeclParser
    <|> try resourceDeclParser
    <|> try handlerDeclParser
    <|> try emitterDeclParser
    <|> try channelDeclParser
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
  return $ FieldDefinition identifier typeSpecifier (Position current startPos endPos)

structDefinitionParser :: TerminaParser (AnnASTElement ParserAnn)
structDefinitionParser = do
  current <- getState
  modifiers <- many modifierParser
  startPos <- getPosition
  reserved "struct"
  identifier <- identifierParser
  fields <- braces (many1 $ try fieldDefinitionParser)
  endPos <- getPosition
  _ <- semi
  return $ TypeDefinition (Struct identifier fields modifiers) (Position current startPos endPos)

classFieldDefinitionParser :: TerminaParser (ClassMember ParserAnn)
classFieldDefinitionParser = do
  current <- getState
  startPos <- getPosition
  identifier <- identifierParser
  _ <- reservedOp ":"
  typeSpecifier <- typeSpecifierParser
  endPos <- getPosition
  _ <- semi
  return $ ClassField (FieldDefinition identifier typeSpecifier (Position current startPos endPos))

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
  accessKind <- parens selfAccessKindParser
  typeSpec <- optionMaybe (reservedOp "->" >> typeSpecifierParser)
  blockRet <- blockParser
  ClassMethod accessKind name typeSpec blockRet . Position current startPos <$> getPosition

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
  typeSpec <- reservedOp "->" >>  typeSpecifierParser
  blockRet <- blockParser
  ClassAction ak name param typeSpec blockRet . Position current startPos <$> getPosition

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
  ClassProcedure ak name params blockRet . Position current startPos <$> getPosition

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
  -- | TODO: See if we allow modifiers in interface procedures to be set
  -- by the programmers
  InterfaceProcedure ak name params [] . Position current startPos <$> getPosition

classViewerParser :: TerminaParser (ClassMember ParserAnn)
classViewerParser = do
  current <- getState
  startPos <- getPosition
  reserved "viewer"
  name <- identifierParser
  params <- parens viewerParamsParser
  typeSpec <- optionMaybe (reservedOp "->" >>  typeSpecifierParser)
  blockRet <- blockParser
  ClassViewer name params typeSpec blockRet . Position current startPos <$> getPosition
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
  procedures <- braces (many1 interfaceProcedureParser)
  endPos <- getPosition
  _ <- semi
  return $ TypeDefinition (Interface RegularInterface identifier extends procedures modifiers) (Position current startPos endPos)

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
    braces (many1 $ classMethodParser
      <|> classViewerParser
      <|> classProcedureParser
      <|> classActionParser
      <|> classFieldDefinitionParser)
  endPos <- getPosition
  _ <- semi
  return $ TypeDefinition (Class classKind identifier fields provides modifiers) (Position current startPos endPos)
  where
    classKindParser :: TerminaParser ClassKind
    classKindParser =
      (reserved "task" >> return TaskClass)
      <|> (reserved "resource" >> return ResourceClass)
      <|> (reserved "handler" >> return HandlerClass)

variantDefinitionParser :: TerminaParser (EnumVariant ParserAnn)
variantDefinitionParser = identifierParser >>= \identifier ->
  try (parens (sepBy1 typeSpecifierParser comma) <&> EnumVariant identifier)
  <|> return (EnumVariant identifier [])

enumDefinitionParser :: TerminaParser (AnnASTElement ParserAnn)
enumDefinitionParser = do
  current <- getState
  modifiers <- many modifierParser
  startPos <- getPosition
  reserved "enum"
  identifier <- identifierParser
  variants <- braces (sepBy1 (try variantDefinitionParser) comma)
  endPos <- getPosition
  _ <- semi
  return $ TypeDefinition (Enum identifier variants modifiers) (Position current startPos endPos)

-- | Top Level parser
topLevel :: TerminaParser (AnnotatedProgram ParserAnn)
topLevel = many $
  try functionParser <|> try globalDeclParser
  <|> try typeDefintionParser

moduleIdentifierParser :: TerminaParser [ String ]
moduleIdentifierParser = sepBy1 firstCapital dot
  where
    firstCapital = (:)
      <$> (lower <?> "Module paths begin with a lowercase letter.")
      <*> (many (lower <|> char '_' <|> digit) <?> "Module names only accept lowercase letters or underscores.")

moduleImportParser :: TerminaParser (ModuleImport ParserAnn)
moduleImportParser = do
  current <- getState
  startPos <- getPosition
  reserved "import"
  ident <- moduleIdentifierParser
  endPos <- getPosition
  _ <- semi
  return $ ModuleImport ident (Position current startPos endPos)

contents :: TerminaParser a -> TerminaParser a
contents p = wspcs *> p <* eof

terminaModuleParser :: TerminaParser (TerminaModule ParserAnn)
terminaModuleParser = wspcs *> (Termina <$> many moduleImportParser <*> contents topLevel)

-- | Simple function to test parsers
strParse :: String -> Either ParseError (AnnotatedProgram ParserAnn)
strParse = runP topLevel "" ""

getStartPosition :: ParserAnn -> SourcePos
getStartPosition (Position _ startPos _) = startPos
getStartPosition _ = error "Internal error: expected Position annotation (this should not happen)"

getEndPosition :: ParserAnn -> SourcePos
getEndPosition (Position _ _ endPos) = endPos
getEndPosition _ = error "Internal error: expected Position annotation (this should not happen)"