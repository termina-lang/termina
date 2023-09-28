{-# LANGUAGE TupleSections #-}
-- | Module dedicated to Parsing

module Parsing where

import           AST                  hiding (blockRet)
-- Importing position from Parsec
import           Text.Parsec.Pos
-- Importing parser combinators
import           Text.Parsec
import           Text.Parsec.String
-- Importing tokenizer
import qualified Text.Parsec.Expr     as Ex
import qualified Text.Parsec.Language as Lang
import qualified Text.Parsec.Token    as Tok

import           Data.Functor
import Text.Parsec.Expr
import qualified Data.List as L
import Control.Monad

{- | Type of the parsing annotations

This type is used to identify the annotations made on the different
elements of the AST. In this case, the annotations will only include
the position in the source file where the element is located.

-}

data Annotation =
  Position SourcePos -- ^ Source code position
  | Internal
  -- ^ Internal error position. Used for debugging, internals shoulnd't happen
  deriving Show

----------------------------------------
-- Lexer
----------------------------------------

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef
  where
    reservedNames =
      -- Globals
      ["volatile","static","protected", "const"]
      ++ -- Basic Types
      ["u8","u16","u32","u64"
      ,"i8","i16","i32","i64"
      ,"bool","char"]
      ++ -- Polymorphic Types
      ["MsgQueue","Pool", "Option"]
      ++ -- Struct and Union Types
      ["struct","union"]
      ++ -- Dynamic Subtyping
      ["'dyn"]
      ++ -- Declarations
      ["task","function","handler", "at"]
      ++ -- Stmt
      ["var", "match", "for", "if", "else", "return", "while"]
      ++ -- Free
      ["free"]
      ++ -- Constants
      ["true","false"]
      ++ -- Modules
      ["mod"]
      ++ -- Class methods
      ["self"]

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
                      ,"&" -- BitwiseAnd
                      ,"|" -- BitwiseOr
                      ,"^" -- BitwiseXor
                      ,"&&" -- LogicalAnd
                      ,"||" -- LogicalOr
                      ,"#" -- Attribute
                      ,":" -- Type annotation
                      ,"::" -- Enum variant
                      ,"=" -- Assignment
                      ,"->" -- Function return type
                      ,"=>" -- Match case
                      ,"[" -- Vector init
                      ,"]" -- Vector init
                      ,"{" -- Field values assignments
                      ,"}" -- Field values assignments
                      ,"(" -- Parens
                      ,")" -- Parens
                      ,".." -- Vector slice and for loop range
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

hexa :: Parser Integer
hexa = char '0' >> Tok.hexadecimal lexer

tails :: [a] -> [[a]]
tails = L.tails

-- | Parser for natural numbers (positive integers + 0)
-- This parser is used solely when declaring a new vector to specify
-- its dimension, i.e. the parser checks that we specify the dimension
-- as a natural number.
natural :: Parser Integer
natural = Tok.natural lexer

-- | Parser for integer numbers
-- This parser is used when defining regular integer literals
-- TODO: Change it to support Termina's hex format.
integer :: Parser Integer
integer = Tok.integer lexer

----------------------------------------
-- Parser
----------------------------------------

-- | Types
typeSpecifierParser :: Parser TypeSpecifier
typeSpecifierParser =
  msgQueueParser
  <|> poolParser
  <|> vectorParser
  <|> referenceParser
  <|> dynamicSubtypeParser
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
  <|> (reserved "bool" >> return Bool)
  <|> (reserved "char" >> return Char)

parameterParser :: Parser Parameter
parameterParser = do
  identifier <- identifierParser
  reservedOp ":"
  Parameter identifier <$> typeSpecifierParser

-- | Parser for a field value assignments expression
-- This expression is used to create annonymous structures to serve as right
-- hand side of an assignment expression.
-- Examples of this expression:
-- { field0 = 0 : u32, field1 = 0 : u16 } : StructIdentifier
fieldValuesAssignExpressionParser :: Parser (Expression Annotation)
fieldValuesAssignExpressionParser = do
    assignments <- braces (sepBy flValues comma)
    p <- getPosition
    _ <- reservedOp ":"
    identifier <- identifierParser
    return $ FieldValuesAssignmentsExpression identifier assignments (Position p)
    where
      flValues = do
            identifier <- identifierParser
            _ <- reservedOp "="
            FieldValueAssignment identifier <$> expressionParser

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
  initializer <- optionMaybe (parens constExprParser')
  _ <- reservedOp "]"
  return $ Modifier identifier (KC <$> initializer)

msgQueueParser :: Parser TypeSpecifier
msgQueueParser = do
  reserved "MsgQueue"
  _ <- reservedOp "<"
  typeSpecifier <- typeSpecifierParser
  _ <- semi
  size <- K <$> natural
  _ <- reservedOp ">"
  return $ MsgQueue typeSpecifier size

poolParser :: Parser TypeSpecifier
poolParser = do
  reserved "Pool"
  _ <- reservedOp "<"
  typeSpecifier <- typeSpecifierParser
  _ <- semi
  size <- K <$> natural
  _ <- reservedOp ">"
  return $ Pool typeSpecifier size

vectorParser :: Parser TypeSpecifier
vectorParser = do
  _ <- reservedOp "["
  typeSpecifier <- typeSpecifierParser
  _ <- semi
  size <- KC <$> constExprParser'
  _ <- reservedOp "]"
  return $ Vector typeSpecifier size

referenceParser :: Parser TypeSpecifier
referenceParser = reservedOp "&" >> Reference <$> typeSpecifierParser

dynamicSubtypeParser :: Parser TypeSpecifier
dynamicSubtypeParser = reservedOp "'dyn" >> DynamicSubtype <$> typeSpecifierParser

optionParser :: Parser TypeSpecifier
optionParser = reserved "Option" >> Option <$> angles typeSpecifierParser

-- Expression Parser
expressionParser' :: Parser (Expression Annotation)
expressionParser' = buildPrattParser -- New parser
    [[castingPostfix]
    ,[binaryInfix "*" Multiplication Ex.AssocLeft,
      binaryInfix "/" Division Ex.AssocLeft]
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
  where binaryInfix s f = Ex.Infix (do
          _ <- reservedOp s
          p <- getPosition
          return $ \l r -> BinOp f l r (Position p))
        castingPostfix = Ex.Postfix (do
          _ <- reserved "as"
          p <- getPosition
          typeSpecificer <- typeSpecifierParser
          return $ \parent -> Casting parent typeSpecificer (Position p))

functionCallParser :: Parser (Expression Annotation)
functionCallParser =
  FunctionExpression
  <$> identifierParser
  <*> parens (sepBy (try expressionParser) comma)
  <*> (Position <$> getPosition)


optionVariantExprParser :: Parser (Expression Annotation)
optionVariantExprParser =
  (do
    p <- getPosition
    _ <- reserved "None"
    return $ OptionVariantExpression None (Position p)) <|>
  (do
    p <- getPosition
    _ <- reserved "Some"
    someExpr <- parens expressionParser
    return $ OptionVariantExpression (Some someExpr) (Position p))

enumVariantExprParser :: Parser (Expression Annotation)
enumVariantExprParser = do
  p <- getPosition
  enum <- identifierParser
  _ <- reservedOp "::"
  variant <- identifierParser
  parameterList <-
    option [] (parens (sepBy (try expressionParser) comma))
  return $ EnumVariantExpression enum variant parameterList (Position p)

expressionParser :: Parser (Expression Annotation)
expressionParser = try optionVariantExprParser
  <|> try enumVariantExprParser
  <|> referenceExprParser
  <|> vectorInitParser
  <|> fieldValuesAssignExpressionParser
  <|> expressionParser'

referenceExprParser :: Parser (Expression Annotation)
referenceExprParser = do
  p <- getPosition
  _ <- reservedOp "&"
  object <- objectParser
  return $ ReferenceExpression object (Position p)

expressionTermParser :: Parser (Expression Annotation)
expressionTermParser = constExprParser
  <|> try memberMethodAccessParser
  <|> try functionCallParser
  <|> try (AccessObject <$> objectParser)
  <|> parensExprParser

memberMethodAccessParser :: Parser (Expression Annotation)
memberMethodAccessParser = do
  p <- getPosition
  object <- identifierParser
  _ <- reservedOp "."
  -- method <- identifierParser
  method <- accessOpParser
  params <- parens (sepBy (try expressionParser) comma)
  return $ MemberMethodAccess (Variable object (Position p)) method params (Position p)

accessOpParser :: Parser AccessOp
accessOpParser
  = try (reserved "alloc" >> pure Alloc)
  <|> try (reserved "send" >> pure Send)
  <|> try (reserved "receive" >> pure Receive)
  <|> (UserDef <$> identifierParser)


parensExprParser :: Parser (Expression Annotation)
parensExprParser = parens $ ParensExpression <$> expressionParser <*> (Position <$> getPosition)

parensObjectParser :: Parser (Object  Annotation)
parensObjectParser = parens $ ParensObject <$> objectParser <*> (Position <$> getPosition)

----------------------------------------
-- Object Parsing

-- Object term parser.
objectTermParser :: Parser (Object Annotation)
objectTermParser = Variable <$> identifierParser <*> (Position <$> getPosition)
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

objectParser :: Parser (Object  Annotation)
objectParser = objectParser' objectTermParser
  where
    objectParser'
      = buildPrattParser -- New parser
      [[memberAccessPostfix, vectorOpPostfix]
      ,[dereferencePrefix]]
    vectorOpPostfix 
      = Ex.Postfix (try (do 
            _ <- reservedOp "["
            low <- KC <$> constExprParser'
            _ <- reservedOp ".."
            up <- KC <$> constExprParser'
            _ <- reservedOp "]"
            p <- getPosition
            return $ \parent ->  VectorSliceExpression parent low up (Position p)
          ) <|> (do
            index <- brackets expressionParser
            p <- getPosition
            return $ \parent ->  VectorIndexExpression parent index (Position p)
          ))
    memberAccessPostfix
      = Ex.Postfix (do
      _ <- reservedOp "."
      p <- getPosition
      member <- identifierParser
      -- member <- accessOpParser
      return $ \parent ->  MemberAccess parent member (Position p))
    dereferencePrefix
      = Ex.Prefix (do
      p <- getPosition
      _ <- reservedOp "*"
      return $ flip Dereference (Position p))
----------------------------------------


vectorInitParser :: Parser (Expression Annotation)
vectorInitParser = do
  _ <- reservedOp "["
  p <- getPosition
  value <- expressionParser
  _ <- semi
  size <- KC <$> constExprParser'
  _ <- reservedOp "]"
  return $ VectorInitExpression value size (Position p)

-- -- Task Definition

blockParser :: Parser (BlockRet Annotation)
blockParser = BlockRet  <$> many blockItemParser <*> returnStmtParser

-- <task-definition> ::= 'task' <identifier> '(' <input-parameter> ')' <compound-statement>
taskParser :: Parser (AnnASTElement  Annotation)
taskParser = do
  modifiers <- many modifierParser
  p <- getPosition
  reserved "task"
  name <- identifierParser
  params <- parens (sepBy parameterParser comma)
  reservedOp "->"
  typeSpec <- typeSpecifierParser
  blockRet <- braces blockParser
  return $ Task name params typeSpec blockRet modifiers (Position p)

handlerParser :: Parser (AnnASTElement  Annotation)
handlerParser = do
  modifiers <- many modifierParser
  p <- getPosition
  reserved "handler"
  name <- identifierParser
  params <- parens (sepBy parameterParser comma)
  reservedOp "->"
  typeSpec <- typeSpecifierParser
  blockRet <- braces blockParser
  return $ Handler name params typeSpec blockRet modifiers (Position p)

returnStmtParser :: Parser (ReturnStmt Annotation)
returnStmtParser = do
  p <- getPosition
  _ <- reserved "return"
  ret <- optionMaybe expressionParser
  _ <- semi
  return $ ReturnStmt ret (Position p)

emptyReturn :: Parser ()
emptyReturn = returnStmtParser >>= maybe mempty (const (fail "Expected Empty return")) . returnExpression

functionParser :: Parser (AnnASTElement  Annotation)
functionParser = do
  modifiers <- many modifierParser
  p <- getPosition
  reserved "fn"
  name <- identifierParser
  params <- parens (sepBy parameterParser comma)
  typeSpec <- optionMaybe (do
    reservedOp "->"
    typeSpecifierParser)
  blockRet <- braces blockParser
  return $ Function name params typeSpec blockRet modifiers (Position p)

moduleInclusionParser :: Parser (AnnASTElement  Annotation)
moduleInclusionParser = do
  modifiers <- many modifierParser
  p <- getPosition
  reserved "mod"
  name <- identifierParser
  _ <- semi
  return $ ModuleInclusion name modifiers (Position p)

constExprParser' :: Parser Const
constExprParser' = parseLitInteger <|> parseLitBool <|> parseLitChar
  where
    parseLitInteger =
      do
        num <- integer
        reservedOp ":"
        ty <- typeSpecifierParser
        return (I ty num)
    parseLitBool = (reserved "true" >> return (B True)) <|> (reserved "false" >> return (B False))
    parseLitChar = C <$> charLit

constExprParser :: Parser (Expression Annotation)
constExprParser = flip Constant . Position  <$> getPosition <*> constExprParser'
    -- parseLitString = S <$> stringLit

declarationParser :: Parser (Statement Annotation)
declarationParser = do
  p <- getPosition
  reserved "var"
  name <- identifierParser
  reservedOp ":"
  ty <- typeSpecifierParser
  _ <- reservedOp "="
  initializer <-  expressionParser
  _ <- semi
  return $ Declaration name ty initializer (Position p)

singleExprStmtParser :: Parser (Statement Annotation)
singleExprStmtParser = do
  p <- getPosition
  expression <- expressionParser
  _ <- semi
  return $ SingleExpStmt expression (Position p)

freeStmtParser :: Parser (Statement Annotation)
freeStmtParser = reserved "free" >>
 Free <$> parens objectParser <*> (Position <$> getPosition)
 >>= \t -> semi >> return t


blockItemParser :: Parser (Statement Annotation)
blockItemParser
  =   try ifElseIfStmtParser
  <|> try declarationParser
  <|> try assignmentStmtPaser
  <|> try forLoopStmtParser
  <|> try matchStmtParser
  <|> try freeStmtParser
  <|> singleExprStmtParser

assignmentStmtPaser :: Parser (Statement Annotation)
assignmentStmtPaser = do
  p <- getPosition
  lval <- objectParser
  _ <- reservedOp "="
  rval <- expressionParser
  _ <- semi
  return $ AssignmentStmt lval rval (Position p)

matchCaseParser :: Parser (MatchCase Annotation)
matchCaseParser = do
  reserved "case"
  p <- getPosition
  cons <- identifierParser
  args <- try (parens (sepBy identifierParser comma)) <|> return []
  reservedOp "=>"
  compound <- braces $ many blockItemParser
  return $ MatchCase cons args compound (Position p)

matchStmtParser :: Parser (Statement Annotation)
matchStmtParser = do
  reserved "match"
  p <- getPosition
  matchExpression <- expressionParser
  cases <- braces (many1 $ try matchCaseParser)
  return $ MatchStmt matchExpression cases (Position p)

elseIfParser :: Parser (ElseIf Annotation)
elseIfParser = do
  p <- getPosition
  _ <- reserved "else"
  _ <- reserved "if"
  expression <- expressionParser
  compound <- braces $ many blockItemParser
  return $ ElseIf expression compound (Position p)

ifElseIfStmtParser :: Parser (Statement Annotation)
ifElseIfStmtParser = do
  p <- getPosition
  _ <- reserved "if"
  expression <- expressionParser
  ifCompound <- braces $ many blockItemParser
  elseIfs <- many $ try elseIfParser
  elseCompound <- option [] (do
    _ <- reserved "else"
    braces $ many $ try blockItemParser)
  return $ IfElseStmt expression ifCompound elseIfs elseCompound (Position p)

forLoopStmtParser :: Parser (Statement Annotation)
forLoopStmtParser = do
  p <- getPosition
  _ <- reserved "for"
  identifier <- identifierParser
  _ <- reserved "in"
  start <- expressionParser
  _ <- reservedOp ".."
  end <- expressionParser
  breakCondition <- optionMaybe (do
    reserved "while"
    expressionParser)
  compound <- braces $ many blockItemParser
  return $ ForLoopStmt identifier start end breakCondition compound (Position p)

volatileDeclParser :: Parser (Global Annotation)
volatileDeclParser = do
  modifiers <- many modifierParser
  p <- getPosition
  reserved "volatile"
  identifier <- identifierParser
  reservedOp ":"
  typeSpecifier <- typeSpecifierParser
  reserved "at"
  -- From doc: Parses a non-negative whole number in the hexadecimal system.
  -- https://hackage.haskell.org/package/parsec-3.1.15.1/docs/Text-Parsec-Token.html
  addr <- hexa
  _ <- semi
  return $ Volatile identifier typeSpecifier addr modifiers (Position p)

staticDeclParser :: Parser (Global Annotation)
staticDeclParser = do
  modifiers <- many modifierParser
  p <- getPosition
  reserved "static"
  identifier <- identifierParser
  reservedOp ":"
  typeSpecifier <- typeSpecifierParser
  initializer <- optionMaybe (do
    reservedOp "="
    expressionParser)
  _ <- semi
  return $ Static identifier typeSpecifier initializer modifiers (Position p)

protectedDeclParser :: Parser (Global Annotation)
protectedDeclParser = do
  modifiers <- many modifierParser
  p <- getPosition
  reserved "protected"
  identifier <- identifierParser
  reservedOp ":"
  typeSpecifier <- typeSpecifierParser
  initializer <- optionMaybe (do
    reservedOp "="
    expressionParser)
  _ <- semi
  return $ Shared identifier typeSpecifier initializer modifiers (Position p)

constDeclParser :: Parser (Global Annotation)
constDeclParser = do
  modifiers <- many modifierParser
  p <- getPosition
  reserved "const"
  identifier <- identifierParser
  reservedOp ":"
  typeSpecifier <- typeSpecifierParser
  _ <- reservedOp "="
  initializer <- expressionParser
  _ <- semi
  return $ Const identifier typeSpecifier initializer modifiers (Position p)

globalDeclParser :: Parser (AnnASTElement  Annotation)
globalDeclParser = do
  g <- volatileDeclParser <|> staticDeclParser <|> protectedDeclParser <|> constDeclParser
  return $ GlobalDeclaration g

typeDefintionParser :: Parser (AnnASTElement  Annotation)
typeDefintionParser = flip TypeDefinition
  -- First we get the position, at the beginning of the definition
  <$> (Position <$> getPosition)
  <*> (structDefinitionParser <|> unionDefinitionParser <|> enumDefinitionParser <|> classDefinitionParser)

fieldDefinitionParser :: Parser FieldDefinition
fieldDefinitionParser = do
  identifier <- identifierParser
  _ <- reservedOp ":"
  typeSpecifier <- typeSpecifierParser
  _ <- semi
  return $ FieldDefinition identifier typeSpecifier

structDefinitionParser :: Parser (TypeDef Annotation)
structDefinitionParser = do
  modifiers <- many modifierParser
  -- p <- getPosition
  reserved "struct"
  identifier <- identifierParser
  fields <- braces (many1 $ try fieldDefinitionParser)
  _ <- semi
  return $ Struct identifier fields modifiers -- (Position p)

unionDefinitionParser :: Parser (TypeDef Annotation)
unionDefinitionParser = do
  modifiers <- many modifierParser
  -- p <- getPosition
  reserved "union"
  identifier <- identifierParser
  fields <- braces (many1 $ try fieldDefinitionParser)
  _ <- semi
  return $ Union identifier fields modifiers -- (Position p)

classFieldDefinitionParser :: Parser (ClassMember Annotation)
classFieldDefinitionParser = do
  identifier <- identifierParser
  p <- getPosition
  _ <- reservedOp ":"
  typeSpecifier <- typeSpecifierParser
  _ <- semi
  return $ ClassField identifier typeSpecifier (Position p)

classMethodParser :: Parser (ClassMember Annotation)
classMethodParser = do
  p <- getPosition
  reserved "fn"
  name <- identifierParser
  (self, params) <- parens (withSelf <|> withOutSelf)
  -- All methods are procedure.
  -- typeSpec <- optionMaybe (do
  --   reservedOp "->"
  --   typeSpecifierParser)
  reservedOp "{"
  body <- many blockItemParser
  -- ret <- returnStmtParser
  emptyReturn
  reservedOp "}"
  return $ ClassMethod name params self body (Position p)
  where
    withSelf :: Parser (SelfMethod, [Parameter])
    withSelf =
      reserved "self" >> comma >> sepBy parameterParser comma >>= return .(Self,)
    withOutSelf :: Parser (SelfMethod, [Parameter])
    withOutSelf = (NoSelf,) <$> sepBy parameterParser comma



classDefinitionParser :: Parser (TypeDef Annotation)
classDefinitionParser = do
  modifiers <- many modifierParser
  -- p <- getPosition
  reserved "class"
  identifier <- identifierParser
  fields <- braces (many1 $ try classFieldDefinitionParser <|> classMethodParser)
  _ <- semi
  return $ Class identifier fields modifiers

variantDefinitionParser :: Parser EnumVariant
variantDefinitionParser = identifierParser >>= \identifier ->
  try (parens (sepBy1 typeSpecifierParser comma) <&> EnumVariant identifier)
  <|> return (EnumVariant identifier [])

enumDefinitionParser :: Parser (TypeDef Annotation)
enumDefinitionParser = do
  modifiers <- many modifierParser
  reserved "enum"
  identifier <- identifierParser
  variants <- braces (sepBy1 (try variantDefinitionParser) comma)
  _ <- semi
  return $ Enum identifier variants modifiers

-- | Top Level parser
topLevel :: Parser (AnnotatedProgram Annotation)
topLevel = many1 $
  try taskParser <|> try handlerParser
  <|> try functionParser <|> try globalDeclParser
  <|> try typeDefintionParser
  <|> moduleInclusionParser

contents :: Parser a -> Parser a
contents p = wspcs *> p <* eof

-- | Simple function to test parsers
strParse :: String -> Either ParseError (AnnotatedProgram Annotation)
strParse = parse topLevel ""
