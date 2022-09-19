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
import qualified Text.Parsec.Token as Tok

-- | This is what we will build in this section
type PositionAST = AASTElem SourcePos

----------------------------------------
-- Lexer
----------------------------------------

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef
  where
    reservedNames =
      -- Globals
      ["Atomic","Volatile","MsgQueue","Pool"]
      ++ -- Basic Types
      ["u8","u16","u32","u64"
      ,"i8","i16","i32","i64"
      ,"bool","char"]
      ++ -- Declarations
      ["Task","Procedure","Handler", "at"]
      ++ -- Stmt
      ["let", "skip"]
      ++ -- Constants
      ["true","false"]

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
                   , Tok.opStart = oneOf ":,;=&|<=>+-*/."
                   , Tok.reservedNames = reservedNames
                   , Tok.reservedOpNames = [":","=",";",","]
                   -- | Is the language case sensitive? It should be
                   , Tok.caseSensitive = True
                   }

-- Lexemes

wspcs :: Parsec String () ()
wspcs = Tok.whiteSpace lexer

braces :: Parsec String () a -> Parsec String () a
braces = Tok.braces lexer

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

reserved :: String -> Parsec String () ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parsec String () ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Tok.identifier lexer

hexa :: Parser Integer
hexa = Tok.hexadecimal lexer

number :: Parser Int
number = fromInteger <$> Tok.natural lexer

----------------------------------------
-- Parser
----------------------------------------

-- | Types
basicTypesParse :: Parser BType
basicTypesParse =
  (reserved "u8" >> return UInt8)
  <|> (reserved "u16" >> return UInt16)
  <|> (reserved "u32" >> return UInt32)
  <|> (reserved "u64" >> return UInt64)
  <|> (reserved "i8" >> return Int8)
  <|> (reserved "i16" >> return Int16)
  <|> (reserved "i32" >> return Int32)
  <|> (reserved "i64" >> return Int64)
  <|> (reserved "bool" >> return Bool)
  <|> (reserved "char" >> return Char)

multLines :: Parser [String]
multLines = sepBy (many (satisfy (\c -> (c /= '}') && (c /= '{')) )) newline

withPos :: Parser a -> Parser (a, SourcePos)
withPos p = do
  pos <- getPosition
  res <- p
  return (res,pos)
--

paramParser :: Parser Param
paramParser = curry Param <$> identifier <*> basicTypesParse

paramParse1 :: Parser (Param, [Param])
paramParse1 = sepBy1 paramParser comma
  >>= \case
   (x : xs) -> return (x,xs)
   _ -> parserFail "Expecting at least one argument"

-- -- Task Definition

-- <task-definition> ::= 'task' <identifier> '(' <input-parameter> ')' <compound-statement>
taskParser :: Parser PositionAST
taskParser = do
  p <- getPosition
  reserved "Task"
  name <- identifier
  param <- parens paramParser
  body <- compoundParser
  return $ Task name param body p

handlerParser :: Parser PositionAST
handlerParser = do
  reserved "Handler"
  p <- getPosition
  name <- identifier
  params <- parens (sepEndBy paramParser comma)
  compound <- compoundParser
  return $ Handler name params compound p

procedureParser :: Parser PositionAST
procedureParser = do
  p <- getPosition
  reserved "Procedure"
  name <- identifier
  params <- parens paramParse1
  reservedOp ":"
  btype <- basicTypesParse
  body <- compoundParser
  return $ Proc name params btype body p

constantParser :: Parser Const
constantParser = parseLitInt <|> parseLitBool <|> parseLitChar
  where
    parseLitInt = I <$> number
    parseLitBool = (reserved "true" >> return (B True)) <|> (reserved "false" >> return (B False))
    parseLitChar = C <$> anyChar

localDeclarations :: Parser (LocalDecl SourcePos)
localDeclarations = do
  p <- getPosition
  reserved "let"
  name <- identifier
  reservedOp ":"
  ty <- basicTypesParse
  reservedOp "="
  val <- constantParser
  _ <- semi
  return $ LDecl (name, ty, val, p )

stmtParser :: Parser (Stmt SourcePos)
stmtParser = getPosition >>= \p ->
  (parseSkip p)
  <|> (parseComment p)
  where
    parseSkip p = reserved "skip" >> semi >>= \_ -> return (Skip p)
    parseComment p = reserved "ccmt" >> stringLit >>= \str -> return (CComment str p)

compoundParser :: Parser (CompoundStmt SourcePos)
compoundParser = braces $
  Compound <$> (many localDeclarations) <*> (many stmtParser)

atomDeclParser :: Parser Global
atomDeclParser = do
  reserved "Atomic"
  ty <- angles basicTypesParse
  nm <- identifier
  _ <- semi
  return $ Atom ty nm

volatileDeclParser :: Parser Global
volatileDeclParser = do
  reserved "Volatile"
  ty <- angles basicTypesParse
  nm <- identifier
  reserved "at"
  -- From doc: Parses a non-negative whole number in the hexadecimal system.
  -- https://hackage.haskell.org/package/parsec-3.1.15.1/docs/Text-Parsec-Token.html
  addr <- hexa
  _ <- semi
  return $ Volatile ty nm addr

pairTypeInt :: Parser (BType, Int)
pairTypeInt = do
      ty <- basicTypesParse
      _ <- comma
      gint <- number
      return (ty,gint)

msgQueueDeclParser :: Parser Global
msgQueueDeclParser = do
  reserved "MsgQueue"
  (ty, gint) <- angles pairTypeInt
  nm <- identifier
  _ <- semi
  return $ MsgQueue ty gint nm

poolDeclParser :: Parser Global
poolDeclParser = do
  reserved "Pool"
  (ty,gint) <- angles pairTypeInt
  nm <- identifier
  _ <- semi
  return $ Pool ty gint nm

glblDecl :: Parser Global
glblDecl = poolDeclParser <|> msgQueueDeclParser
  <|> atomDeclParser  <|> volatileDeclParser

-- | Top Level parser
topLevel :: Parser [AASTElem SourcePos]
topLevel = many $
  taskParser <|> handlerParser <|> procedureParser
  <|> (getPosition >>= \p -> GlbDec <$> glblDecl <*> pure p )


contents :: Parser a -> Parser a
contents p = wspcs *> p <* eof
