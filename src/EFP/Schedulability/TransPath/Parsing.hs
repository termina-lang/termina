module EFP.Schedulability.TransPath.Parsing where

-- Importing parser combinators
import           Text.Parsec hiding (Error, Ok)

-- Importing tokenizer
import qualified Text.Parsec.Language as Lang
import qualified Text.Parsec.Token    as Tok

import EFP.Schedulability.TransPath.AST
import qualified Text.Parsec.Expr as Ex
import Data.Char

type PathParser = Parsec String FilePath

lexer :: Tok.TokenParser FilePath
lexer = Tok.makeTokenParser langDef
  where
    reservedNames =
      ["import"
       ,"twcep"]
      ++ 
      -- | Control flow keywords
      ["if", "elif", "else", "case", "for"]
      ++
      ["block", "send", "invoke", "call",
       "alloc", "free", "return", "continue", "reboot", "syscall"]

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
                   , Tok.opStart = oneOf "="
                   , Tok.reservedNames = reservedNames
                   , Tok.reservedOpNames = [
                      "::" -- Scoping
                      ,"@" -- Location specifier
                      ,"=" -- Assignment
                    ]
                   -- | Is the language case sensitive? It should be
                   , Tok.caseSensitive = True
                   }

reserved :: String -> PathParser ()
reserved = Tok.reserved lexer

reservedOp :: String -> PathParser ()
reservedOp = Tok.reservedOp lexer

identifierParser :: PathParser String
identifierParser = Tok.identifier lexer

brackets :: PathParser a -> PathParser a
brackets = Tok.brackets lexer

braces :: PathParser a -> PathParser a
braces = Tok.braces lexer

parens :: PathParser a -> PathParser a
parens = Tok.parens lexer

wspcs :: PathParser ()
wspcs = Tok.whiteSpace lexer

comma :: PathParser String
comma = Tok.comma lexer

number :: Integer -> PathParser Char -> PathParser Integer
number base baseDigit = do
    digits <- many1 baseDigit
    let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
    seq n (return n)

sign :: PathParser (Integer -> Integer)
sign = (char '-' >> return negate)
  <|> (char '+' >> return id)
  <|> return id

-- | TerminaParser for integer decimal numbers
-- This parser is used when defining regular integer literals
decimal :: PathParser Integer
decimal =  Tok.lexeme lexer $ do
  f <- Tok.lexeme lexer sign
  n <- number 10 digit
  return (f n)

hexadecimal :: PathParser Integer
hexadecimal = Tok.lexeme lexer $
  char '0' >> oneOf "xX" >> number 16 hexDigit

integerParser :: PathParser TInteger
integerParser = try hexParser <|> decParser
  where
    hexParser = flip TInteger HexRepr <$> hexadecimal
    decParser = flip TInteger DecRepr <$> decimal

blockPositionParser :: PathParser BlockPosition
blockPositionParser = do
    _ <- reservedOp "@("
    startLine <- decimal
    _ <- reservedOp ":"
    startCol <- decimal
    _ <- reservedOp ","
    endLine <- decimal
    _ <- reservedOp ":"
    endCol <- decimal
    _ <- reservedOp ")"
    return $ BlockPosition startLine startCol endLine endCol

condIfParser :: PathParser WCEPathBlock
condIfParser = do
    _ <- reserved "if"
    loc <- blockPositionParser
    blocks <- braces (sepBy wcepPathBlockParser comma)
    return $ WCEPathCondIf blocks loc

condElseIfParser :: PathParser WCEPathBlock
condElseIfParser = do
    _ <- reserved "elif"
    loc <- blockPositionParser
    blocks <- braces (sepBy wcepPathBlockParser comma)
    return $ WCEPathCondElseIf blocks loc

condElseParser :: PathParser WCEPathBlock
condElseParser = do
    _ <- reserved "else"
    loc <- blockPositionParser
    blocks <- braces (sepBy wcepPathBlockParser comma)
    return $ WCEPathCondElse blocks loc

parensConstExprParser :: PathParser ConstExpression
parensConstExprParser = parens constExpressionParser

constIntParser :: PathParser ConstExpression
constIntParser = ConstInt <$> integerParser

constObjectParser :: PathParser ConstExpression
constObjectParser = ConstObject <$> identifierParser

constExpressionTermParser :: PathParser ConstExpression
constExpressionTermParser =
  try constIntParser
  <|> try constObjectParser
  <|> parensConstExprParser

-- Expression TerminaParser
constExpressionParser :: PathParser ConstExpression
constExpressionParser = Ex.buildExpressionParser  -- New parser
    [[binaryInfix "*" Multiplication Ex.AssocLeft,
      binaryInfix "/" Division Ex.AssocLeft,
      binaryInfix "%" Modulo Ex.AssocLeft]
    ,[binaryInfix "+" Addition Ex.AssocLeft,
      binaryInfix "-" Subtraction Ex.AssocLeft]
    ,[binaryInfix "<<" BitwiseLeftShift Ex.AssocLeft,
      binaryInfix ">>" BitwiseRightShift Ex.AssocLeft]
    ,[binaryInfix "&" BitwiseAnd Ex.AssocLeft]
    ,[binaryInfix "|" BitwiseOr Ex.AssocLeft]
    ,[binaryInfix "^" BitwiseXor Ex.AssocLeft]
    ]
    constExpressionTermParser
  where
    binaryInfix s f = Ex.Infix (do
          _ <- reservedOp s
          return $ \l r -> ConstBinOp f l r)


forLoopParser :: PathParser WCEPathBlock
forLoopParser = do
    _ <- reserved "for"
    (initExpr, finalExpr) <- parens $ do
        initE <- constExpressionParser
        _ <- reservedOp ".."
        finalE <- constExpressionParser
        return (initE, finalE)
    loc <- blockPositionParser
    blocks <- braces (sepBy wcepPathBlockParser comma)
    return $ WCEPathForLoop initExpr finalExpr blocks loc

matchCaseParser :: PathParser WCEPathBlock
matchCaseParser = do
    _ <- reserved "case"
    loc <- blockPositionParser
    blocks <- braces (sepBy wcepPathBlockParser comma)
    return $ WCEPathMatchCase blocks loc

sendMessageParser :: PathParser WCEPathBlock
sendMessageParser = do
    _ <- reserved "send"
    portName <- parens identifierParser
    WCEPSendMessage portName <$> blockPositionParser

memberFunctionCallParser :: PathParser WCEPathBlock
memberFunctionCallParser = do
    _ <- reserved "call"
    funcName <- identifierParser
    argExprs <- parens (sepBy constExpressionParser comma)
    WCEPathMemberFunctionCall funcName argExprs <$> blockPositionParser

procedureInvokeParser :: PathParser WCEPathBlock
procedureInvokeParser = do
    _ <- reserved "invoke"
    (portName, procName) <- parens $ do
        pName <- identifierParser
        _ <- reservedOp "."
        prName <- identifierParser
        return (pName, prName)
    argExprs <- parens (sepBy constExpressionParser comma)
    WCEPProcedureInvoke portName procName argExprs <$> blockPositionParser

allocBoxParser :: PathParser WCEPathBlock
allocBoxParser = do
    _ <- reserved "alloc"
    portName <- parens identifierParser
    WCEPAllocBox portName <$> blockPositionParser

freeBoxParser :: PathParser WCEPathBlock
freeBoxParser = do
    _ <- reserved "free"
    portName <- parens identifierParser
    WCEPFreeBox portName <$> blockPositionParser

regularBlockParser :: PathParser WCEPathBlock
regularBlockParser = do
    _ <- reserved "block"
    WCEPRegularBlock <$> blockPositionParser

returnParser :: PathParser WCEPathBlock
returnParser = do
    _ <- reserved "return"
    WCEPReturn <$> blockPositionParser

continueParser :: PathParser WCEPathBlock
continueParser = do
    _ <- reserved "continue"
    actionName <- parens identifierParser
    WCEPContinue actionName <$> blockPositionParser

rebootParser :: PathParser WCEPathBlock
rebootParser = do
    _ <- reserved "reboot"
    WCEPReboot <$> blockPositionParser

systemCallParser :: PathParser WCEPathBlock
systemCallParser = do
    _ <- reserved "syscall"
    sysCallName <- parens identifierParser
    WCEPSystemCall sysCallName <$> blockPositionParser

wcepPathBlockParser :: PathParser WCEPathBlock
wcepPathBlockParser =
    try regularBlockParser
    <|> try returnParser
    <|> try continueParser
    <|> try rebootParser
    <|> try systemCallParser
    <|> try condIfParser
    <|> try condElseIfParser
    <|> try condElseParser
    <|> try forLoopParser
    <|> try matchCaseParser
    <|> try sendMessageParser
    <|> try memberFunctionCallParser
    <|> try procedureInvokeParser
    <|> try allocBoxParser
    <|> freeBoxParser

transactionalWCEPParser :: PathParser TransactionalWCEPath
transactionalWCEPParser = do
    _ <- reserved "twcep"
    clsName <- identifierParser
    _ <- reservedOp "::"
    elementName <- identifierParser
    _ <- reservedOp "::"
    memberFunction <- identifierParser
    constParams <- parens (sepBy identifierParser comma)
    _ <- reservedOp "="
    blocks <- brackets (sepBy wcepPathBlockParser comma)
    return $ TransactionalWCEPath clsName elementName memberFunction constParams blocks

-- | Top Level parser
topLevel :: PathParser [TransactionalWCEPath]
topLevel = many $
  try transactionalWCEPParser