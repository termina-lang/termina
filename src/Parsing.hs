-- | Module dedicated to Parsing

module Parsing where

import AST
-- Importing position from Parsec
import Text.Parsec.Pos
-- Importing parser combinators
import Text.Parsec
import Text.Parsec.String
-- Importing tokenizer
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Language as Lang
import qualified Text.Parsec.Expr as Ex

-- | This is what we will build in this section
data Annotation =
    Position SourcePos
  | Attribute Identifier Expression
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
      ["MsgQueue","Pool"]
      ++ -- Struct and Union Types
      ["struct","union"]
      ++ -- Declarations
      ["task","function","handler", "at"]
      ++ -- Stmt
      ["var", "match", "for", "if", "else", "return"]
      ++ -- Constants
      ["true","false"]
      ++ -- Modules
      ["mod"]

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

number :: Parser Int
number = fromInteger <$> Tok.natural lexer

----------------------------------------
-- Parser
----------------------------------------

-- | Types
typeSpecifierParser :: Parser TypeSpecifier
typeSpecifierParser =
  msgQueueParser
  <|> poolParser
  <|> vectorParser
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

fieldValuesAssignExpressionParser :: Parser Expression
fieldValuesAssignExpressionParser = 
  braces (FieldValuesAssignmentsExpression <$> sepBy (
    do
      identifier <- identifierParser
      reservedOp "="
      FieldValueAssignment identifier <$> expressionParser
    )
  comma)

attributeParser :: Parser Annotation
attributeParser = do
  char '#' >> brackets (Attribute <$> identifierParser <*> parens expressionParser)

msgQueueParser :: Parser TypeSpecifier
msgQueueParser = do
  reserved "MsgQueue"
  _ <- reservedOp "<"
  typeSpecifier <- typeSpecifierParser
  _ <- semi
  quantity <- variableParser <|> constExprParser
  _ <- reservedOp ">"
  return $ MsgQueue typeSpecifier quantity

poolParser :: Parser TypeSpecifier
poolParser = do
  reserved "Pool"
  _ <- reservedOp "<"
  typeSpecifier <- typeSpecifierParser
  _ <- semi
  quantity <- variableParser <|> constExprParser
  _ <- reservedOp ">"
  return $ Pool typeSpecifier quantity

vectorParser :: Parser TypeSpecifier
vectorParser = do
  _ <- reservedOp "["
  typeSpecifier <- typeSpecifierParser
  _ <- semi
  quantity <- variableParser <|> constExprParser
  _ <- reservedOp "]"
  return $ Vector typeSpecifier quantity

-- Expression Parser

expressionParser :: Parser Expression
expressionParser = Ex.buildExpressionParser
    [[referencePrefix]
    ,[functionPostfix]
    ,[vectorIndexPostfix]
    ,[binaryInfix "." MemberAccess Ex.AssocRight]
    ,[binaryInfix "*" Multiplication Ex.AssocLeft,
      binaryInfix "/" Division Ex.AssocLeft]
    ,[binaryInfix "+" Addition Ex.AssocLeft,
      binaryInfix "-" Substraction Ex.AssocLeft]
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
    termParser
  where binaryInfix s f = Ex.Infix (do
          _ <- reservedOp s
          return (BinOp f))
        referencePrefix = Ex.Prefix (do
          _ <- reservedOp "&"
          return ReferenceExpression)
        functionPostfix = Ex.Postfix (do
          arguments <- parens $ sepBy (try expressionParser) comma
          return $ \parent -> FunctionExpression parent arguments)
        vectorIndexPostfix = Ex.Postfix (do
          index <- brackets expressionParser
          return $ \parent ->  VectorIndexExpression parent index)

termParser :: Parser Expression
termParser = vectorInitParser <|> variableParser
  <|> constExprParser
  <|> fieldValuesAssignExpressionParser
  <|> parens expressionParser

variableParser :: Parser Expression
variableParser = Variable <$> identifierParser

vectorInitParser :: Parser Expression
vectorInitParser = do
  _ <- reservedOp "["
  value <- expressionParser
  _ <- semi
  amount <- expressionParser
  _ <- reservedOp "]"
  return $ VectorInitExpression value amount

-- -- Task Definition

-- <task-definition> ::= 'task' <identifier> '(' <input-parameter> ')' <compound-statement>
taskParser :: Parser (AnnASTElement Annotation)
taskParser = do
  attributes <- many attributeParser
  p <- getPosition
  reserved "task"
  name <- identifierParser
  params <- parens (sepBy parameterParser comma)
  reservedOp "->"
  typeSpec <- typeSpecifierParser
  reservedOp "{"
  body <- many blockItemParser
  ret <- returnStmtParser
  reservedOp "}"
  return $ Task name params typeSpec body ret (Position p : attributes)

handlerParser :: Parser (AnnASTElement Annotation)
handlerParser = do
  attributes <- many attributeParser
  p <- getPosition
  reserved "handler"
  name <- identifierParser
  params <- parens (sepBy parameterParser comma)
  reservedOp "{"
  body <- many blockItemParser
  ret <- returnStmtParser
  reservedOp "}"
  return $ Handler name params body ret (Position p : attributes)

returnStmtParser :: Parser (Statement Annotation)
returnStmtParser = do
  attributes <- many attributeParser
  p <- getPosition
  _ <- reserved "return"
  ret <- optionMaybe expressionParser
  _ <- semi
  return $ ReturnStmt ret (Position p : attributes)

functionParser :: Parser (AnnASTElement Annotation)
functionParser = do
  attributes <- many attributeParser
  p <- getPosition
  reserved "fn"
  name <- identifierParser
  params <- parens (sepBy parameterParser comma)
  reservedOp "->"
  typeSpec <- typeSpecifierParser
  reservedOp "{"
  body <- many blockItemParser
  ret <- returnStmtParser
  reservedOp "}"
  return $ Function name params typeSpec body ret (Position p : attributes)

moduleInclusionParser :: Parser (AnnASTElement Annotation)
moduleInclusionParser = do
  attributes <- many attributeParser
  p <- getPosition
  reserved "mod"
  name <- identifierParser
  _ <- semi
  return $ ModuleInclusion name (Position p : attributes)

constExprParser :: Parser Expression
constExprParser = Constant <$> (parseLitInt <|> parseLitBool <|> parseLitChar <|> parseLitString)
  where
    parseLitInt = I <$> number
    parseLitBool = (reserved "true" >> return (B True)) <|> (reserved "false" >> return (B False))
    parseLitChar = C <$> charLit
    parseLitString = S <$> stringLit

declarationParser :: Parser (Statement Annotation)
declarationParser = do
  attributes <- many attributeParser
  p <- getPosition
  reserved "var"
  name <- identifierParser
  reservedOp ":"
  ty <- typeSpecifierParser
  initializer <- optionMaybe (do
    reservedOp "="
    expressionParser)
  _ <- semi
  return $ Declaration name ty initializer (Position p : attributes)

singleExprStmtParser :: Parser (Statement Annotation)
singleExprStmtParser = do
  attributes <- many attributeParser
  p <- getPosition
  expression <- expressionParser
  _ <- semi
  return $ SingleExpStmt expression (Position p : attributes)

blockItemParser :: Parser (Statement Annotation)
blockItemParser = try matchStmtParser
  <|> try ifElseIfStmtParser
  <|> try declarationParser
  <|> try assignmentStmtPaser
  <|> singleExprStmtParser

assignmentStmtPaser :: Parser (Statement Annotation)
assignmentStmtPaser = do
  attributes <- many attributeParser
  p <- getPosition
  lval <- expressionParser
  reservedOp "="
  rval <- expressionParser
  _ <- semi
  return $ AssignmentStmt lval rval (Position p : attributes)

matchCaseParser :: Parser (MatchCase Annotation)
matchCaseParser = do
  attributes <- many attributeParser
  p <- getPosition
  caseExpression <- expressionParser
  reservedOp "=>"
  compound <- braces $ many blockItemParser
  return $ MatchCase caseExpression compound (Position p : attributes)

matchStmtParser :: Parser (Statement Annotation)
matchStmtParser = do
  attributes <- many attributeParser
  p <- getPosition
  reserved "match"
  matchExpression <- expressionParser
  cases <- braces (many1 $ try matchCaseParser)
  return $ MatchStatement matchExpression cases (Position p : attributes)

elseIfParser :: Parser (ElseIf Annotation)
elseIfParser = do
  attributes <- many attributeParser
  p <- getPosition
  _ <- reserved "else"
  _ <- reserved "if"
  expression <- expressionParser
  compound <- braces $ many blockItemParser
  return $ ElseIf expression compound (Position p : attributes)

ifElseIfStmtParser :: Parser (Statement Annotation)
ifElseIfStmtParser = do
  attributes <- many attributeParser
  p <- getPosition
  reserved "if"
  expression <- expressionParser
  ifCompound <- braces $ many blockItemParser
  elseIfs <- many $ try elseIfParser
  _ <- reserved "else"
  elseCompound <- braces $ many blockItemParser
  return $ IfElseStmt expression ifCompound elseIfs elseCompound  (Position p : attributes)

volatileDeclParser :: Parser (Global Annotation)
volatileDeclParser = do
  attributes <- many attributeParser
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
  return $ Volatile identifier typeSpecifier addr (Position p : attributes)

staticDeclParser :: Parser (Global Annotation)
staticDeclParser = do
  attributes <- many attributeParser
  p <- getPosition
  reserved "static"
  identifier <- identifierParser
  reservedOp ":"
  typeSpecifier <- typeSpecifierParser
  initializer <- optionMaybe (do
    reservedOp "="
    expressionParser)
  _ <- semi
  return $ Static identifier typeSpecifier initializer (Position p : attributes)

protectedDeclParser :: Parser (Global Annotation)
protectedDeclParser = do
  attributes <- many attributeParser
  p <- getPosition
  reserved "protected"
  identifier <- identifierParser
  reservedOp ":"
  typeSpecifier <- typeSpecifierParser
  initializer <- optionMaybe (do
    reservedOp "="
    expressionParser)
  _ <- semi
  return $ Protected identifier typeSpecifier initializer (Position p : attributes)

constDeclParser :: Parser (Global Annotation)
constDeclParser = do
  attributes <- many attributeParser
  p <- getPosition
  reserved "const"
  identifier <- identifierParser
  reservedOp ":"
  typeSpecifier <- typeSpecifierParser
  _ <- reservedOp "="
  initializer <- expressionParser
  _ <- semi
  return $ Const identifier typeSpecifier initializer (Position p : attributes)

globalDeclParser :: Parser (AnnASTElement Annotation)
globalDeclParser = do
  g <- volatileDeclParser <|> staticDeclParser <|> protectedDeclParser <|> constDeclParser
  return $ GlobalDeclaration g

-- | Top Level parser
topLevel :: Parser (AnnotatedProgram Annotation)
topLevel = many $
  taskParser <|> handlerParser <|> functionParser <|> globalDeclParser
  <|> moduleInclusionParser

contents :: Parser a -> Parser a
contents p = wspcs *> p <* eof
