module EFP.Schedulability.TransPath.Parsing where

-- Importing parser combinators
import           Text.Parsec hiding (Error, Ok)

-- Importing tokenizer
import qualified Text.Parsec.Language as Lang
import qualified Text.Parsec.Token    as Tok

import EFP.Schedulability.TransPath.AST
import qualified Text.Parsec.Expr as Ex
import Data.Char
import EFP.Schedulability.TransPath.Types
import Utils.Annotations

type PathParser = Parsec String FilePath

lexer :: Tok.TokenParser FilePath
lexer = Tok.makeTokenParser langDef
  where
    reservedNames = []
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

condIfParser :: PathParser (WCEPathBlock ParserAnn)
condIfParser = do
    current <- getState
    startPos <- getPosition
    _ <- reserved "if"
    loc <- blockPositionParser
    blocks <- brackets (sepBy wcepPathBlockParser comma)
    WCEPathCondIf blocks loc . Position current startPos <$> getPosition

condElseIfParser :: PathParser (WCEPathBlock ParserAnn)
condElseIfParser = do
    current <- getState
    startPos <- getPosition
    _ <- reserved "elif"
    loc <- blockPositionParser
    blocks <- brackets (sepBy wcepPathBlockParser comma)
    WCEPathCondElseIf blocks loc . Position current startPos <$> getPosition

condElseParser :: PathParser (WCEPathBlock ParserAnn)
condElseParser = do
    current <- getState
    startPos <- getPosition
    _ <- reserved "else"
    loc <- blockPositionParser
    blocks <- brackets (sepBy wcepPathBlockParser comma)
    WCEPathCondElse blocks loc . Position current startPos <$> getPosition

parensConstExprParser :: PathParser (ConstExpression ParserAnn)
parensConstExprParser = parens constExpressionParser

constIntParser :: PathParser (ConstExpression ParserAnn)
constIntParser = do
  current <- getState
  pos <- getPosition
  tInteger <- integerParser
  ConstInt tInteger . Position current pos <$> getPosition

constObjectParser :: PathParser (ConstExpression ParserAnn)
constObjectParser = do
  current <- getState
  pos <- getPosition
  ident <- identifierParser
  ConstObject ident . Position current pos <$> getPosition

constExpressionTermParser :: PathParser (ConstExpression ParserAnn)
constExpressionTermParser =
  try constIntParser
  <|> try constObjectParser
  <|> parensConstExprParser

-- Expression TerminaParser
constExpressionParser :: PathParser (ConstExpression ParserAnn)
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
          current <- getState
          _ <- reservedOp s
          return $ \l r -> ConstBinOp f l r (Position current (getStartPosition (getAnnotation l)) (getEndPosition (getAnnotation r))))

    getStartPosition :: ParserAnn -> SourcePos
    getStartPosition (Position _ startPos _) = startPos
    getStartPosition _ = error "Internal error: expected Position annotation (this should not happen)"

    getEndPosition :: ParserAnn -> SourcePos
    getEndPosition (Position _ _ endPos) = endPos
    getEndPosition _ = error "Internal error: expected Position annotation (this should not happen)"


forLoopParser :: PathParser (WCEPathBlock ParserAnn)
forLoopParser = do
    current <- getState
    startPos <- getPosition
    _ <- reserved "for"
    (initExpr, finalExpr) <- parens $ do
        initE <- constExpressionParser
        _ <- reservedOp ".."
        finalE <- constExpressionParser
        return (initE, finalE)
    loc <- blockPositionParser
    blocks <- brackets (sepBy wcepPathBlockParser comma)
    WCEPathForLoop initExpr finalExpr blocks loc . Position current startPos <$> getPosition

matchCaseParser :: PathParser (WCEPathBlock ParserAnn)
matchCaseParser = do
    current <- getState
    startPos <- getPosition
    _ <- reserved "case"
    loc <- blockPositionParser
    blocks <- brackets (sepBy wcepPathBlockParser comma)
    WCEPathMatchCase blocks loc . Position current startPos <$> getPosition

sendMessageParser :: PathParser (WCEPathBlock ParserAnn)
sendMessageParser = do
    current <- getState
    startPos <- getPosition
    _ <- reserved "send"
    portName <- parens identifierParser
    loc <- blockPositionParser
    WCEPSendMessage portName loc . Position current startPos <$> getPosition

memberFunctionCallParser :: PathParser (WCEPathBlock ParserAnn)
memberFunctionCallParser = do
    current <- getState
    startPos <- getPosition
    _ <- reserved "call"
    funcName <- parens identifierParser
    argExprs <- option [] (parens (sepBy constExpressionParser comma))
    loc <- blockPositionParser
    WCEPathMemberFunctionCall funcName argExprs loc . Position current startPos <$> getPosition

procedureInvokeParser :: PathParser (WCEPathBlock ParserAnn)
procedureInvokeParser = do
    current <- getState
    startPos <- getPosition
    _ <- reserved "invoke"
    (portName, procName) <- parens $ do
        pName <- identifierParser
        _ <- reservedOp "."
        prName <- identifierParser
        return (pName, prName)
    argExprs <- option [] (parens (sepBy constExpressionParser comma))
    loc <- blockPositionParser
    WCEPProcedureInvoke portName procName argExprs loc . Position current startPos <$> getPosition

allocBoxParser :: PathParser (WCEPathBlock ParserAnn)
allocBoxParser = do
    current <- getState
    startPos <- getPosition
    _ <- reserved "alloc"
    portName <- parens identifierParser
    loc <- blockPositionParser
    WCEPAllocBox portName loc . Position current startPos <$> getPosition

freeBoxParser :: PathParser (WCEPathBlock ParserAnn)
freeBoxParser = do
    current <- getState
    startPos <- getPosition
    _ <- reserved "free"
    portName <- parens identifierParser
    loc <- blockPositionParser
    WCEPFreeBox portName loc . Position current startPos <$> getPosition

regularBlockParser :: PathParser (WCEPathBlock ParserAnn)
regularBlockParser = do
    current <- getState
    startPos <- getPosition
    _ <- reserved "block"
    loc <- blockPositionParser
    WCEPRegularBlock loc . Position current startPos <$> getPosition

returnParser :: PathParser (WCEPathBlock ParserAnn)
returnParser = do
    current <- getState
    startPos <- getPosition
    _ <- reserved "return"
    loc <- blockPositionParser
    WCEPReturn loc . Position current startPos <$> getPosition

continueParser :: PathParser (WCEPathBlock ParserAnn)
continueParser = do
    current <- getState
    startPos <- getPosition
    _ <- reserved "continue"
    actionName <- parens identifierParser
    loc <- blockPositionParser
    WCEPContinue actionName loc . Position current startPos <$> getPosition

rebootParser :: PathParser (WCEPathBlock ParserAnn)
rebootParser = do
    current <- getState
    startPos <- getPosition
    _ <- reserved "reboot"
    loc <- blockPositionParser
    WCEPReboot loc . Position current startPos <$> getPosition

systemCallParser :: PathParser (WCEPathBlock ParserAnn)
systemCallParser = do
    current <- getState
    startPos <- getPosition
    _ <- reserved "syscall"
    sysCallName <- parens identifierParser
    argExprs <- option [] (parens (sepBy constExpressionParser comma))
    loc <- blockPositionParser
    WCEPSystemCall sysCallName argExprs loc . Position current startPos <$> getPosition

wcepPathBlockParser :: PathParser (WCEPathBlock ParserAnn)
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

transactionalWCEPParser :: PathParser (TransactionalWCEPath ParserAnn)
transactionalWCEPParser = do
    current <- getState
    startPos <- getPosition
    _ <- reserved "twcep"
    clsName <- identifierParser
    _ <- reservedOp "::"
    elementName <- identifierParser
    _ <- reservedOp "::"
    memberFunction <- identifierParser
    constParams <- parens (sepBy identifierParser comma)
    _ <- reservedOp "="
    blocks <- brackets (sepBy wcepPathBlockParser comma)
    TransactionalWCEPath clsName elementName memberFunction constParams blocks . Position current startPos <$> getPosition

-- | Top Level parser
topLevel :: PathParser [TransactionalWCEPath ParserAnn]
topLevel = many $
  try transactionalWCEPParser

contents :: PathParser a -> PathParser a
contents p = wspcs *> p <* eof

terminaTransPathsParser :: PathParser [TransactionalWCEPath ParserAnn]
terminaTransPathsParser = contents topLevel