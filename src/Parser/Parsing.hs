{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
-- | Module dedicated to Parsing

module Parser.Parsing where

import           AST.Parser                  hiding (blockRet)
-- Importing position from Parsec
import           Text.Parsec.Pos
-- Importing parser combinators
import           Text.Parsec
import           Text.Parsec.String

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


----------------------------------------
-- Lexer
----------------------------------------

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef
  where
    reservedNames =
      -- Basic Types
      ["u8","u16","u32","u64"
      ,"i8","i16","i32","i64"
      ,"usize", "bool","char"]
      ++ -- Polymorphic Types
             ["MsgQueue", "Pool", "Option", "Allocator", "Atomic", 
              "AtomicArray", "AtomicAccess", "AtomicArrayAccess"]
      ++ -- Struct and enum types
             ["struct", "enum"]
      ++ -- Box Subtyping
             ["box"]
      ++ -- Fixed Location Subtyping
             ["loc"]
      ++ -- Ports Subtyping
             ["access", "sink", "in", "out"]
      ++ -- Global declarations
             ["task", "function", "handler", "resource", "const"]
      ++ -- Stmt
             ["var", "match", "for", "if", "else", "return", "while"]
      ++ -- Trigger
             ["triggers"]
      ++ -- Provide
             ["provides"]
      ++ -- Constants
             ["true", "false"]
      ++ -- Modules
             ["import"]
      ++ -- Class methods
             ["procedure", "viewer", "method", "action"]
      ++ -- Casting keyword
             ["as"]
      ++ -- option name
             ["option"]
      ++ -- is variant operator
             ["is"]

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
                      ,"[" -- Array init
                      ,"]" -- Array init
                      ,"{" -- Field values assignments
                      ,"}" -- Field values assignments
                      ,"(" -- Parens
                      ,")" -- Parens
                      ,".." -- Array slice and for loop range
                      ,"&mut" -- Mutable reference creation
                      ,"@" -- Field address assignment
                      ,"<->" -- Access port connection
                      ,"<-" -- Inbound/Sink connection
                    ]
                   -- | Is the language case sensitive? It should be
                   , Tok.caseSensitive = True
                   }

-- Lexemes

wspcs :: Parsec String () ()
wspcs = Tok.whiteSpace lexer

braces :: Parsec String () a -> Parsec String () a
braces = Tok.braces lexer

brackets :: Parsec String () a -> Parsec String () a
brackets = Tok.brackets lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

angles :: Parser a -> Parser a
angles = Tok.angles lexer

comma :: Parser String
comma = Tok.comma lexer

semi :: Parser String
semi = Tok.semi lexer

dot :: Parser String
dot = Tok.dot lexer

stringLit :: Parser String
stringLit = Tok.stringLiteral lexer

charLit :: Parser Char
charLit = Tok.charLiteral lexer

reserved :: String -> Parsec String () ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parsec String () ()
reservedOp = Tok.reservedOp lexer

identifierParser :: Parser String
identifierParser = Tok.identifier lexer

tails :: [a] -> [[a]]
tails = L.tails

number :: Integer -> Parser Char -> Parser Integer
number base baseDigit = do
    digits <- many1 baseDigit
    let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
    seq n (return n)

sign :: Parser (Integer -> Integer)
sign = (char '-' >> return negate)
  <|> (char '+' >> return id)
  <|> return id

-- | Parser for integer decimal numbers
-- This parser is used when defining regular integer literals
decimal :: Parser Integer
decimal =  Tok.lexeme lexer $ do
  f <- Tok.lexeme lexer sign
  n <- number 10 digit
  return (f n)

hexadecimal :: Parser Integer
hexadecimal = Tok.lexeme lexer $
  char '0' >> oneOf "xX" >> number 16 hexDigit

----------------------------------------
-- Parser
----------------------------------------

-- | Types
typeSpecifierParser :: Parser TypeSpecifier
typeSpecifierParser =
  msgQueueParser
  <|> poolParser
  <|> vectorParser
  <|> mutableReferenceParser
  <|> referenceParser
  <|> boxSubtypeParser
  <|> locationSubtypeParser
  <|> allocatorParser
  <|> atomicParser
  <|> atomicArrayParser
  <|> atomicAccessParser
  <|> atomicArrayAccessParser
  <|> sinkPortParser
  <|> inPortParser
  <|> outPortParser
  <|> accessPortParser
  <|> optionParser
  <|> (DefinedType <$> identifierParser)
  <|> (reserved "u8" >> return UInt8)
  <|> (reserved "u16" >> return UInt16)
  <|> (reserved "u32" >> return UInt32)
  <|> (reserved "u64" >> return UInt64)
  <|> (reserved "i8" >> return Int8)
  <|> (reserved "i16" >> return Int16)
  <|> (reserved "i32" >> return Int32)
  <|> (reserved "i64" >> return Int64)
  <|> (reserved "usize" >> return USize)
  <|> (reserved "bool" >> return Bool)
  <|> (reserved "char" >> return Char)

parameterIdentifierParser :: Parser Identifier
parameterIdentifierParser = try ((char '_' >> identifierParser) <&> ('_' :)) <|> identifierParser

parameterParser :: Parser Parameter
parameterParser = do
  identifier <- parameterIdentifierParser
  reservedOp ":"
  Parameter identifier <$> typeSpecifierParser

-- | Parser for a field value assignments expression
-- This expression is used to create annonymous structures to serve as right
-- hand side of an assignment expression.
-- Examples of this expression:
-- { field0 = 0 : u32, field1 = 0 : u16 } : StructIdentifier
structInitializerParser :: Parser (Expression ParserAnn)
structInitializerParser = do
    startpos <- getPosition
    assignments <- braces (sepBy (wspcs *> fieldAssignmentParser <* wspcs) comma)
    identifier <- optionMaybe (reservedOp ":" >> identifierParser)
    StructInitializer assignments identifier . Position startpos <$> getPosition
    where

      fieldAssignmentParser :: Parser (FieldAssignment ParserAnn)
      fieldAssignmentParser = do
        try fieldValueParser <|> try fieldAddressParser <|> try fieldAccessPortConnectionParser 
            <|> try fieldInboundPortConnectionParser <|> fieldOutboundPortConnectionParser 

      fieldValueParser :: Parser (FieldAssignment ParserAnn)
      fieldValueParser = do
        startPos <- getPosition
        identifier <- identifierParser
        _ <- reservedOp "="
        expr <- expressionParser
        FieldValueAssignment identifier expr . Position startPos <$> getPosition
      
      fieldAddressParser :: Parser (FieldAssignment ParserAnn)
      fieldAddressParser = do
        startPos <- getPosition
        identifier <- identifierParser
        _ <- reservedOp "@"
        addr <- integerParser
        FieldAddressAssignment identifier addr . Position startPos <$> getPosition
      
      fieldAccessPortConnectionParser :: Parser (FieldAssignment ParserAnn)
      fieldAccessPortConnectionParser = do
        startPos <- getPosition
        identifier <- identifierParser
        _ <- reservedOp "<->"
        port <- identifierParser
        FieldPortConnection AccessPortConnection identifier port . Position startPos <$> getPosition
      
      fieldInboundPortConnectionParser :: Parser (FieldAssignment ParserAnn)
      fieldInboundPortConnectionParser = do
        startPos <- getPosition
        identifier <- identifierParser
        _ <- reservedOp "<-"
        port <- identifierParser
        FieldPortConnection InboundPortConnection identifier port . Position startPos <$> getPosition

      fieldOutboundPortConnectionParser :: Parser (FieldAssignment ParserAnn)
      fieldOutboundPortConnectionParser = do
        startPos <- getPosition
        identifier <- identifierParser
        _ <- reservedOp "->"
        port <- identifierParser
        FieldPortConnection OutboundPortConnection identifier port . Position startPos <$> getPosition

-- | Parser for an element modifier
-- A modifier is of the form:
-- #[identifier(expression)]
-- where:
-- - identifier: is a mandatory identifier that names the attribute or modifier
-- - expresssion: is an optional expression between parenthesis.AST
-- Examples of a modifier:
-- #[priority(5)] for a task
-- #[packed] for a struct or union

modifierParser :: Parser Modifier
modifierParser = do
  _ <- reservedOp "#"
  _ <- reservedOp "["
  identifier <- identifierParser
  initializer <- optionMaybe (parens constLiteralParser)
  _ <- reservedOp "]"
  return $ Modifier identifier initializer

msgQueueParser :: Parser TypeSpecifier
msgQueueParser = do
  reserved "MsgQueue"
  _ <- reservedOp "<"
  typeSpecifier <- typeSpecifierParser
  _ <- semi
  size <- sizeParser
  _ <- reservedOp ">"
  return $ MsgQueue typeSpecifier size

poolParser :: Parser TypeSpecifier
poolParser = do
  reserved "Pool"
  _ <- reservedOp "<"
  typeSpecifier <- typeSpecifierParser
  _ <- semi
  size <- sizeParser
  _ <- reserved ">"
  return $ Pool typeSpecifier size

allocatorParser :: Parser TypeSpecifier
allocatorParser = do
  reserved "Allocator"
  _ <- reservedOp "<"
  typeSpecifier <- typeSpecifierParser
  _ <- reserved ">"
  return $ Allocator typeSpecifier

atomicParser :: Parser TypeSpecifier
atomicParser = do
  reserved "Atomic"
  _ <- reservedOp "<"
  typeSpecifier <- typeSpecifierParser
  _ <- reserved ">"
  return $ Atomic typeSpecifier

atomicAccessParser :: Parser TypeSpecifier
atomicAccessParser = do
  reserved "AtomicAccess"
  _ <- reservedOp "<"
  typeSpecifier <- typeSpecifierParser
  _ <- reserved ">"
  return $ AtomicAccess typeSpecifier

atomicArrayParser :: Parser TypeSpecifier
atomicArrayParser = do
  reserved "AtomicArray"
  _ <- reservedOp "<"
  typeSpecifier <- typeSpecifierParser
  _ <- semi
  size <- sizeParser
  _ <- reserved ">"
  return $ AtomicArray typeSpecifier size

atomicArrayAccessParser :: Parser TypeSpecifier
atomicArrayAccessParser = do
  reserved "AtomicArrayAccess"
  _ <- reservedOp "<"
  typeSpecifier <- typeSpecifierParser
  _ <- semi
  size <- sizeParser
  _ <- reserved ">"
  return $ AtomicArrayAccess typeSpecifier size

vectorParser :: Parser TypeSpecifier
vectorParser = do
  _ <- reservedOp "["
  typeSpecifier <- typeSpecifierParser
  _ <- semi
  size <- sizeParser
  _ <- reserved "]"
  return $ Array typeSpecifier size

referenceParser :: Parser TypeSpecifier
referenceParser = reservedOp "&" >> Reference Immutable <$> typeSpecifierParser

mutableReferenceParser :: Parser TypeSpecifier
mutableReferenceParser = reservedOp "&mut" >> Reference Mutable <$> typeSpecifierParser

boxSubtypeParser :: Parser TypeSpecifier
boxSubtypeParser = reserved "box" >> BoxSubtype <$> typeSpecifierParser

locationSubtypeParser :: Parser TypeSpecifier
locationSubtypeParser = reservedOp "loc" >> Location <$> typeSpecifierParser

sinkPortParser :: Parser TypeSpecifier
sinkPortParser = do
  _ <- reserved "sink"
  ts <- typeSpecifierParser
  _ <- reserved "triggers"
  SinkPort ts <$> identifierParser

accessPortParser :: Parser TypeSpecifier
accessPortParser = reservedOp "access" >> AccessPort <$> typeSpecifierParser

outPortParser :: Parser TypeSpecifier
outPortParser = reservedOp "out" >> OutPort <$> typeSpecifierParser

inPortParser :: Parser TypeSpecifier
inPortParser = do
  _ <- reserved "in"
  ts <- typeSpecifierParser
  _ <- reserved "triggers"
  InPort ts <$> identifierParser

optionParser :: Parser TypeSpecifier
optionParser = do
  _ <- reserved "Option"
  _ <- reservedOp "<"
  ts <- typeSpecifierParser
  _ <- reservedOp ">"
  return $ Option ts

-- Expression Parser
expressionParser' :: Parser (Expression ParserAnn)
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
          _ <- reservedOp s
          return $ \l r -> BinOp f l r (Position (getStartPosition (getAnnotation l)) (getEndPosition (getAnnotation r))))
    castingPostfix = Ex.Postfix (do
          _ <- reserved "as"
          typeSpecificer <- typeSpecifierParser
          endPos <- getPosition
          return $ \parent -> Casting parent typeSpecificer (Position (getStartPosition (getAnnotation parent)) endPos))

functionCallParser :: Parser (Expression ParserAnn)
functionCallParser = do
  startPos <- getPosition
  ident <- identifierParser
  params <- parens (sepBy (try expressionParser) comma)
  FunctionCall ident params . Position startPos <$> getPosition

optionVariantExprParser :: Parser (Expression ParserAnn)
optionVariantExprParser =
  (do
    startPos <- getPosition
    _ <- reserved "None"
    OptionVariantInitializer None . Position startPos <$> getPosition) <|>
  (do
    startPos <- getPosition
    _ <- reserved "Some"
    someExpr <- parens expressionParser
    OptionVariantInitializer (Some someExpr) . Position startPos <$> getPosition)

enumVariantExprParser :: Parser (Expression ParserAnn)
enumVariantExprParser = do
  startPos <- getPosition
  enum <- identifierParser
  _ <- reservedOp "::"
  variant <- identifierParser
  parameterList <-
    option [] (parens (sepBy (try expressionParser) comma))
  EnumVariantInitializer enum variant parameterList . Position startPos <$> getPosition

isEnumVariantExprParser :: Parser (Expression ParserAnn)
isEnumVariantExprParser = do
  startPos <- getPosition
  object <- objectParser
  _ <- reserved "is"
  enum <- identifierParser
  _ <- reservedOp "::"
  variant <- identifierParser
  IsEnumVariantExpression object enum variant . Position startPos <$> getPosition

isOptionVariantExprParser :: Parser (Expression ParserAnn)
isOptionVariantExprParser =
  (do
    startPos <- getPosition
    object <- objectParser
    _ <- reserved "is"
    _ <- reserved "None"
    IsOptionVariantExpression object NoneLabel . Position startPos <$> getPosition) <|>
  (do
    startPos <- getPosition
    object <- objectParser
    _ <- reserved "is"
    _ <- reserved "Some"
    IsOptionVariantExpression object SomeLabel . Position startPos <$> getPosition)

expressionParser :: Parser (Expression ParserAnn)
expressionParser = try optionVariantExprParser
  <|> try enumVariantExprParser
  <|> try mutableReferenceExprParser
  <|> try isOptionVariantExprParser
  <|> try arrayExprListInitializerParser
  <|> arrayInitializerParser
  <|> referenceExprParser
  <|> structInitializerParser
  <|> expressionParser'

mutableReferenceExprParser :: Parser (Expression ParserAnn)
mutableReferenceExprParser = do
  startPos <- getPosition
  _ <- reservedOp "&mut"
  object <- objectParser
  ReferenceExpression Mutable object . Position startPos <$> getPosition

referenceExprParser :: Parser (Expression ParserAnn)
referenceExprParser = do
  startPos <- getPosition
  _ <- reservedOp "&"
  object <- objectParser
  ReferenceExpression Immutable object . Position startPos <$> getPosition

expressionTermParser :: Parser (Expression ParserAnn)
expressionTermParser = try isEnumVariantExprParser
  <|> try constantParser
  <|> try functionCallParser
  <|> try accessObjectParser
  <|> parensExprParser

parensExprParser :: Parser (Expression ParserAnn)
parensExprParser = parens expressionParser

parensObjectParser :: Parser (Object  ParserAnn)
parensObjectParser = parens objectParser

----------------------------------------
-- Object Parsing

-- Object term parser.
objectTermParser :: Parser (Object ParserAnn)
objectTermParser = (do
    startPos <- getPosition
    ident <- identifierParser
    Variable ident . Position startPos <$> getPosition)
  <|> parensObjectParser

-- Expression parser
-- This parser is a variation of the original parser that allows us to chain
-- two or more unary expressions together. This is useful when parsing expressions
-- such as vector[0][1], where the postfix operator [] is used twice. This code
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

objectParser :: Parser (Object ParserAnn)
objectParser = objectParser' objectTermParser
  where
    objectParser'
      = buildPrattParser -- New parser
      [[dereferenceMemberAccessPostfix, memberAccessPostfix, vectorOpPostfix]
      ,[dereferencePrefix]]
    vectorOpPostfix
      = Ex.Postfix (try (do
            _ <- reservedOp "["
            low <- expressionParser 
            _ <- reservedOp ".."
            up <- expressionParser
            _ <- reservedOp "]"
            endPos <- getPosition
            return $ \parent ->  ArraySlice parent low up (Position (getStartPosition (getAnnotation parent)) endPos)
          ) <|> (do
            index <- brackets expressionParser
            endPos <- getPosition
            return $ \parent ->  ArrayIndexExpression parent index (Position (getStartPosition (getAnnotation parent)) endPos)
          ))
    dereferenceMemberAccessPostfix
      = Ex.Postfix (do
        _ <- reservedOp "->"
        member <- identifierParser
        endPos <- getPosition
        return $ \parent -> DereferenceMemberAccess parent member (Position (getStartPosition (getAnnotation parent)) endPos))
    memberAccessPostfix
      = Ex.Postfix (do
        _ <- reservedOp "."
        member <- identifierParser
        endPos <- getPosition
        return $ \parent ->  MemberAccess parent member (Position (getStartPosition (getAnnotation parent)) endPos))
    dereferencePrefix
      = Ex.Prefix (do
        startPos <- getPosition
        _ <- reservedOp "*"
        return $ \child -> Dereference child (Position startPos (getEndPosition (getAnnotation child))))
----------------------------------------

accessObjectParser :: Parser (Expression ParserAnn)
accessObjectParser = accessObjectParser' (AccessObject <$> objectTermParser)
  where
    accessObjectParser'
      = buildPrattParser -- New parser
      [[dereferenceMemberAccessPostfix, memberAccessPostfix, vectorOpPostfix]
      ,[dereferencePrefix]]
    vectorOpPostfix
      = Ex.Postfix (try (do
            _ <- reservedOp "["
            low <- expressionParser
            _ <- reservedOp ".."
            up <- expressionParser
            _ <- reservedOp "]"
            endPos <- getPosition
            return $ \parent -> case parent of
              AccessObject obj -> AccessObject (ArraySlice obj low up (Position (getStartPosition (getAnnotation parent)) endPos))  
              _ -> error "Unexpected member access to a non object"
          ) <|> (do
            index <- brackets expressionParser
            endPos <- getPosition
            return $ \parent -> case parent of
              AccessObject obj -> AccessObject (ArrayIndexExpression obj index (Position (getStartPosition (getAnnotation parent)) endPos))  
              _ -> error "Unexpected member access to a non object"
          ))
    dereferenceMemberAccessPostfix
      = Ex.Postfix (do
      _ <- reservedOp "->"
      member <- identifierParser
      params <- optionMaybe (parens (sepBy (try expressionParser) comma))
      endPos <- getPosition
      return (\parent -> case parent of
        AccessObject obj ->
          let startPos = getStartPosition (getAnnotation parent) in
          maybe (AccessObject (DereferenceMemberAccess obj member (Position startPos endPos))) (flip (DerefMemberFunctionCall obj member) (Position startPos endPos)) params
        _ -> error "Unexpected member access to a non object"))
    memberAccessPostfix
      = Ex.Postfix (do
      _ <- reservedOp "."
      member <- identifierParser
      params <- optionMaybe (parens (sepBy (try expressionParser) comma))
      endPos <- getPosition
      return (\parent -> case parent of
        AccessObject obj ->
          let startPos = getStartPosition (getAnnotation parent) in
          maybe (AccessObject (MemberAccess obj member (Position startPos endPos))) (flip (MemberFunctionCall obj member) (Position startPos endPos)) params
        _ -> error "Unexpected member access to a non object"))
    dereferencePrefix
      = Ex.Prefix (do
      startPos <- getPosition
      _ <- reservedOp "*"
      return (\child -> case child of
        AccessObject obj -> 
          let endPos = getStartPosition (getAnnotation child) in
          AccessObject (Dereference obj (Position startPos endPos))
        _ -> error "Unexpected member access to a non object"))

arrayInitializerParser :: Parser (Expression ParserAnn)
arrayInitializerParser = do
  startPos <- getPosition
  _ <- reservedOp "["
  value <- expressionParser
  _ <- semi
  size <- sizeParser
  _ <- reservedOp "]"
  ArrayInitializer value size . Position startPos <$> getPosition

arrayExprListInitializerParser :: Parser (Expression ParserAnn)
arrayExprListInitializerParser = do
  startPos <- getPosition
  exprs <- braces (sepBy expressionParser comma)
  ArrayExprListInitializer exprs . Position startPos <$> getPosition

-- -- Task Definition

blockParser :: Parser (BlockRet ParserAnn)
blockParser = BlockRet  <$> many blockItemParser <*> returnStmtParser

returnStmtParser :: Parser (ReturnStmt ParserAnn)
returnStmtParser = do
  startPos <- getPosition
  _ <- reserved "return"
  ret <- optionMaybe expressionParser
  _ <- semi
  ReturnStmt ret . Position startPos <$> getPosition

emptyReturn :: Parser ()
emptyReturn = returnStmtParser >>= maybe mempty (const (fail "Expected Empty return")) . returnExpression

functionParser :: Parser (AnnASTElement  ParserAnn)
functionParser = do
  modifiers <- many modifierParser
  reserved "function"
  startPos <- getPosition
  name <- identifierParser
  params <- parens (sepBy parameterParser comma)
  typeSpec <- optionMaybe (do
    reservedOp "->"
    typeSpecifierParser)
  endPos <- getPosition
  blockRet <- braces blockParser
  return $ Function name params typeSpec blockRet modifiers (Position startPos endPos)

constLiteralParser :: Parser Const
constLiteralParser = parseLitInteger <|> parseLitBool <|> parseLitChar
  where
    parseLitInteger =
      do
        num <- integerParser
        ty <- optionMaybe (reservedOp ":" >> typeSpecifierParser)
        return (I num ty)
    parseLitBool = (reserved "true" >> return (B True)) <|> (reserved "false" >> return (B False))
    parseLitChar = C <$> charLit

constantParser :: Parser (Expression ParserAnn)
constantParser = do
  startPos <- getPosition
  literal <- constLiteralParser
  Constant literal . Position startPos <$> getPosition

constExprParser :: Parser (ConstExpression ParserAnn)
constExprParser = 
  constSymbolParser <|> constExprLiteral
  where
    constSymbolParser = do
      startPos <- getPosition
      ident <- identifierParser
      KV ident . Position startPos <$> getPosition
    constExprLiteral = do
      startPos <- getPosition
      literal <- constLiteralParser
      KC literal . Position startPos <$> getPosition

integerParser :: Parser TInteger
integerParser = try hexParser <|> decParser
  where
    hexParser = flip TInteger HexRepr <$> hexadecimal
    decParser = flip TInteger DecRepr <$> decimal

sizeParser :: Parser Size
sizeParser = constValueSizeParser <|> constSizeParser
  where
    constValueSizeParser = do
      ks <- integerParser
      optional (reservedOp ":" >> reserved "usize")
      return $ K ks 
    constSizeParser = V <$> identifierParser

mutableObjDeclarationParser :: Parser (Statement ParserAnn)
mutableObjDeclarationParser = do
  reserved "var"
  startPos <- getPosition
  name <- identifierParser
  reservedOp ":"
  ty <- typeSpecifierParser
  _ <- reservedOp "="
  initializer <-  expressionParser
  _ <- semi
  Declaration name Mutable ty initializer . Position startPos <$> getPosition

immutableObjDeclarationParser :: Parser (Statement ParserAnn)
immutableObjDeclarationParser = do
  reserved "let"
  startPos <- getPosition
  name <- identifierParser
  reservedOp ":"
  ty <- typeSpecifierParser
  _ <- reservedOp "="
  initializer <-  expressionParser
  _ <- semi
  Declaration name Immutable ty initializer . Position startPos <$> getPosition

singleExprStmtParser :: Parser (Statement ParserAnn)
singleExprStmtParser = do
  startPos <- getPosition
  expression <- expressionParser
  _ <- semi
  SingleExpStmt expression . Position startPos <$> getPosition

blockItemParser :: Parser (Statement ParserAnn)
blockItemParser
  =   try ifElseIfStmtParser
  <|> try mutableObjDeclarationParser
  <|> try immutableObjDeclarationParser
  <|> try assignmentStmtPaser
  <|> try forLoopStmtParser
  <|> try matchStmtParser
  <|> singleExprStmtParser

assignmentStmtPaser :: Parser (Statement ParserAnn)
assignmentStmtPaser = do
  startPos <- getPosition
  lval <- objectParser
  _ <- reservedOp "="
  rval <- expressionParser
  _ <- semi
  AssignmentStmt lval rval . Position startPos <$> getPosition

matchCaseParser :: Parser (MatchCase ParserAnn)
matchCaseParser = do
  reserved "case"
  startPos <- getPosition
  caseId <- identifierParser
  args <- try (parens (sepBy identifierParser comma)) <|> return []
  reservedOp "=>"
  compound <- braces $ many blockItemParser
  MatchCase caseId args compound . Position startPos <$> getPosition

matchStmtParser :: Parser (Statement ParserAnn)
matchStmtParser = do
  startPos <- getPosition
  reserved "match"
  matchExpression <- expressionParser
  cases <- braces (many1 $ try matchCaseParser)
  MatchStmt matchExpression cases . Position startPos <$> getPosition

elseIfParser :: Parser (ElseIf ParserAnn)
elseIfParser = do
  startPos <- getPosition
  _ <- reserved "else"
  _ <- reserved "if"
  expression <- expressionParser
  compound <- braces $ many blockItemParser
  ElseIf expression compound . Position startPos <$> getPosition

ifElseIfStmtParser :: Parser (Statement ParserAnn)
ifElseIfStmtParser = do
  startPos <- getPosition
  _ <- reserved "if"
  expression <- expressionParser
  ifCompound <- braces $ many blockItemParser
  elseIfs <- many $ try elseIfParser
  elseCompound <- option Nothing (do
    _ <- reserved "else"
    stmts <- braces $ many $ try blockItemParser
    return $ Just stmts)
  IfElseStmt expression ifCompound elseIfs elseCompound . Position startPos <$> getPosition

forLoopStmtParser :: Parser (Statement ParserAnn)
forLoopStmtParser = do
  startPos <- getPosition
  _ <- reserved "for"
  identifier <- identifierParser
  reservedOp ":"
  ty <- typeSpecifierParser
  _ <- reserved "in"
  start <- constExprParser
  _ <- reservedOp ".."
  end <- constExprParser
  breakCondition <- optionMaybe (do
    reserved "while"
    expressionParser)
  compound <- braces $ many blockItemParser
  ForLoopStmt identifier ty start end breakCondition compound . Position startPos <$> getPosition

taskDeclParser :: Parser (Global ParserAnn)
taskDeclParser = do
  modifiers <- many modifierParser
  reserved "task"
  startPos <- getPosition
  identifier <- identifierParser
  reservedOp ":"
  typeSpecifier <- typeSpecifierParser
  initializer <- optionMaybe (do
    reservedOp "="
    expressionParser)
  _ <- semi
  Task identifier typeSpecifier initializer modifiers . Position startPos <$> getPosition

emitterDeclParser :: Parser (Global ParserAnn)
emitterDeclParser = do
  modifiers <- many modifierParser
  reserved "emitter"
  startPos <- getPosition
  identifier <- identifierParser
  reservedOp ":"
  typeSpecifier <- typeSpecifierParser
  initializer <- optionMaybe (do
    reservedOp "="
    expressionParser)
  _ <- semi
  Emitter identifier typeSpecifier initializer modifiers . Position startPos <$> getPosition

channelDeclParser :: Parser (Global ParserAnn)
channelDeclParser = do
  modifiers <- many modifierParser
  reserved "channel"
  startPos <- getPosition
  identifier <- identifierParser
  reservedOp ":"
  typeSpecifier <- typeSpecifierParser
  initializer <- optionMaybe (do
    reservedOp "="
    expressionParser)
  _ <- semi
  Channel identifier typeSpecifier initializer modifiers . Position startPos <$> getPosition

resourceDeclParser :: Parser (Global ParserAnn)
resourceDeclParser = do
  modifiers <- many modifierParser
  reserved "resource"
  startPos <- getPosition
  identifier <- identifierParser
  reservedOp ":"
  typeSpecifier <- typeSpecifierParser
  initializer <- optionMaybe (do
    reservedOp "="
    expressionParser)
  _ <- semi
  Resource identifier typeSpecifier initializer modifiers . Position startPos <$> getPosition

handlerDeclParser :: Parser (Global ParserAnn)
handlerDeclParser = do
  modifiers <- many modifierParser
  reserved "handler"
  startPos <- getPosition
  identifier <- identifierParser
  reservedOp ":"
  typeSpecifier <- typeSpecifierParser
  initializer <- optionMaybe (do
    reservedOp "="
    expressionParser)
  _ <- semi
  Handler identifier typeSpecifier initializer modifiers . Position startPos <$> getPosition

constDeclParser :: Parser (Global ParserAnn)
constDeclParser = do
  modifiers <- many modifierParser
  reserved "const"
  startPos <- getPosition
  identifier <- identifierParser
  reservedOp ":"
  typeSpecifier <- typeSpecifierParser
  _ <- reservedOp "="
  initializer <- constExprParser
  _ <- semi
  Const identifier typeSpecifier initializer modifiers . Position startPos <$> getPosition

globalDeclParser :: Parser (AnnASTElement  ParserAnn)
globalDeclParser = do
  g <- try taskDeclParser
    <|> try resourceDeclParser
    <|> try handlerDeclParser
    <|> try emitterDeclParser
    <|> try channelDeclParser
    <|> constDeclParser
  return $ GlobalDeclaration g

typeDefintionParser :: Parser (AnnASTElement ParserAnn)
typeDefintionParser = structDefinitionParser <|> enumDefinitionParser <|> classDefinitionParser <|> interfaceDefinitionParser

fieldDefinitionParser :: Parser FieldDefinition
fieldDefinitionParser = do
  identifier <- identifierParser
  _ <- reservedOp ":"
  typeSpecifier <- typeSpecifierParser
  _ <- semi
  return $ FieldDefinition identifier typeSpecifier

structDefinitionParser :: Parser (AnnASTElement ParserAnn)
structDefinitionParser = do
  modifiers <- many modifierParser
  reserved "struct"
  startPos <- getPosition
  identifier <- identifierParser
  fields <- braces (many1 $ try fieldDefinitionParser)
  _ <- semi
  TypeDefinition (Struct identifier fields modifiers) . Position startPos <$> getPosition

classFieldDefinitionParser :: Parser (ClassMember ParserAnn)
classFieldDefinitionParser = do
  startPos <- getPosition
  field <- fieldDefinitionParser
  ClassField field . Position startPos <$> getPosition

classMethodParser :: Parser (ClassMember ParserAnn)
classMethodParser = do
  reserved "method"
  startPos <- getPosition
  name <- identifierParser
  parens (reserved "&mut" >> reserved "self")
  typeSpec <- optionMaybe (reservedOp "->" >> typeSpecifierParser)
  blockRet <- braces blockParser
  ClassMethod name typeSpec blockRet . Position startPos <$> getPosition

classActionParser :: Parser (ClassMember ParserAnn)
classActionParser = do
  reserved "action"
  startPos <- getPosition
  name <- identifierParser
  param <- parens (reserved "&mut" >> reserved "self" >> comma >> parameterParser)
  typeSpec <- reservedOp "->" >>  typeSpecifierParser
  blockRet <- braces blockParser
  ClassAction name param typeSpec blockRet . Position startPos <$> getPosition

classProcedureParser :: Parser (ClassMember ParserAnn)
classProcedureParser = do
  reserved "procedure"
  startPos <- getPosition
  name <- identifierParser
  params <- parens procedureParamsParser
  blockRet <- braces blockParser
  ClassProcedure name params blockRet . Position startPos <$> getPosition
  where
    procedureParamsParser :: Parser [Parameter]
    procedureParamsParser =
      reserved "&mut" >> reserved "self" >> option [] (comma >> sepBy parameterParser comma)

interfaceProcedureParser :: Parser (InterfaceMember ParserAnn)
interfaceProcedureParser = do
  reserved "procedure"
  startPos <- getPosition
  name <- identifierParser
  params <- parens procedureParamsParser
  reservedOp ";"
  InterfaceProcedure name params . Position startPos <$> getPosition
  where
    procedureParamsParser :: Parser [Parameter]
    procedureParamsParser =
      reserved "&mut" >> reserved "self" >> option [] (comma >> sepBy parameterParser comma)

classViewerParser :: Parser (ClassMember ParserAnn)
classViewerParser = do
  reserved "viewer"
  startPos <- getPosition
  name <- identifierParser
  params <- parens viewerParamsParser
  typeSpec <- optionMaybe (reservedOp "->" >>  typeSpecifierParser)
  blockRet <- braces blockParser
  ClassViewer name params typeSpec blockRet . Position startPos <$> getPosition
  where
    viewerParamsParser :: Parser [Parameter]
    viewerParamsParser =
      reserved "&self" >> option [] (comma >> sepBy parameterParser comma)

interfaceDefinitionParser :: Parser (AnnASTElement ParserAnn)
interfaceDefinitionParser = do
  modifiers <- many modifierParser
  reserved "interface"
  startPos <- getPosition
  identifier <- identifierParser
  procedures <- braces (many1 interfaceProcedureParser)
  _ <- semi
  TypeDefinition (Interface identifier procedures modifiers) . Position startPos <$> getPosition

classDefinitionParser :: Parser (AnnASTElement ParserAnn)
classDefinitionParser = do
  modifiers <- many modifierParser
  classKind <- classKindParser
  reserved "class"
  startPos <- getPosition
  identifier <- identifierParser
  provides <- option [] (reserved "provides" >> sepBy identifierParser comma)
  fields <-
    braces (many1 $ classMethodParser
      <|> classViewerParser
      <|> classProcedureParser
      <|> classActionParser
      <|> classFieldDefinitionParser)
  _ <- semi
  TypeDefinition (Class classKind identifier fields provides modifiers) . Position startPos <$> getPosition
  where
    classKindParser :: Parser ClassKind
    classKindParser =
      (reserved "task" >> return TaskClass)
      <|> (reserved "resource" >> return ResourceClass)
      <|> (reserved "handler" >> return HandlerClass)

variantDefinitionParser :: Parser EnumVariant
variantDefinitionParser = identifierParser >>= \identifier ->
  try (parens (sepBy1 typeSpecifierParser comma) <&> EnumVariant identifier)
  <|> return (EnumVariant identifier [])

enumDefinitionParser :: Parser (AnnASTElement ParserAnn)
enumDefinitionParser = do
  modifiers <- many modifierParser
  reserved "enum"
  startPos <- getPosition
  identifier <- identifierParser
  variants <- braces (sepBy1 (try variantDefinitionParser) comma)
  _ <- semi
  TypeDefinition (Enum identifier variants modifiers) . Position startPos <$> getPosition

-- | Top Level parser
topLevel :: Parser AnnotatedProgram
topLevel = many1 $
  try functionParser <|> try globalDeclParser
  <|> try typeDefintionParser

moduleIdentifierParser :: Parser [ String ]
moduleIdentifierParser = sepBy1 firstCapital dot
  where
    firstCapital = (:)
      <$> (lower <?> "Module paths begin with a lowercase letter.")
      <*> (many (lower <|> char '_' <|> digit) <?> "Module names only accept lowercase letters or underscores.")

singleModule :: Parser ([ Modifier ], [String])
singleModule = do
  modifiers <- many modifierParser
  moduleIdent <- moduleIdentifierParser
  return (modifiers, moduleIdent)

moduleInclusionParser :: Parser Module
moduleInclusionParser = do
  reserved "import" 
  (m, ident) <- singleModule
  _ <- semi
  return $ ModInclusion ident m

contents :: Parser a -> Parser a
contents p = wspcs *> p <* eof

terminaModuleParser :: Parser (TerminaModule ParserAnn)
terminaModuleParser = wspcs *> (Termina <$> many moduleInclusionParser <*> contents topLevel)

-- | Simple function to test parsers
strParse :: String -> Either ParseError AnnotatedProgram
strParse = parse topLevel ""

getStartPosition :: ParserAnn -> SourcePos
getStartPosition (Position startPos _) = startPos
getStartPosition _ = error "Internal error: expected Position annotation (this should not happen)"

getEndPosition :: ParserAnn -> SourcePos
getEndPosition (Position _ endPos) = endPos
getEndPosition _ = error "Internal error: expected Position annotation (this should not happen)"