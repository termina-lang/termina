module EFP.Schedulability.RT.Parsing
  (terminaRTParser) where

-- Importing parser combinators
import Text.Parsec hiding (Error, Ok)

-- Importing tokenizer
import qualified Text.Parsec.Language as Lang
import qualified Text.Parsec.Token    as Tok

import EFP.Schedulability.RT.AST
import qualified Text.Parsec.Expr as Ex
import Data.Char
import EFP.Schedulability.RT.Types
import Utils.Annotations

type RTParser = Parsec String FilePath

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
                      ,"#" -- Label specifier
                      ,"=" -- Assignment
                      ,"(" -- Parens
                      ,")" -- Parens
                      ,"->" -- Step continuation
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
                      ,"{|" -- multicast start"
                      ,"|}" -- multicast end
                    ]
                   -- | Is the language case sensitive? It should be
                   , Tok.caseSensitive = True
                   }

reserved :: String -> RTParser ()
reserved = Tok.reserved lexer

reservedOp :: String -> RTParser ()
reservedOp = Tok.reservedOp lexer

identifierParser :: RTParser String
identifierParser = Tok.identifier lexer

angles :: RTParser a -> RTParser a
angles = Tok.angles lexer

braces :: RTParser a -> RTParser a
braces = Tok.braces lexer

parens :: RTParser a -> RTParser a
parens = Tok.parens lexer

wspcs :: RTParser ()
wspcs = Tok.whiteSpace lexer

comma :: RTParser String
comma = Tok.comma lexer

semi :: RTParser String
semi = Tok.semi lexer

number :: Integer -> RTParser Char -> RTParser Integer
number base baseDigit = do
    digits <- many1 baseDigit
    let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
    seq n (return n)

sign :: RTParser (Integer -> Integer)
sign = (char '-' >> return negate)
  <|> (char '+' >> return id)
  <|> return id

-- | TerminaParser for integer decimal numbers
-- This parser is used when defining regular integer literals
decimal :: RTParser Integer
decimal =  Tok.lexeme lexer $ do
  f <- Tok.lexeme lexer sign
  n <- number 10 digit
  return (f n)

hexadecimal :: RTParser Integer
hexadecimal = Tok.lexeme lexer $
  char '0' >> oneOf "xX" >> number 16 hexDigit

integerParser :: RTParser TInteger
integerParser = try hexParser <|> decParser
  where
    hexParser = flip TInteger HexRepr <$> hexadecimal
    decParser = flip TInteger DecRepr <$> decimal

transStepActionParser :: RTParser (RTTransStep ParserAnn)
transStepActionParser = do
    current <- getState
    startPos <- getPosition
    lbl <- identifierParser
    _ <- reservedOp "#"
    componentIdentifier <- identifierParser
    _ <- reservedOp "."
    actionIdentifier <- identifierParser
    _ <- reservedOp "::"
    pathIdentifier <- identifierParser
    nextStep <-  optionMaybe (reservedOp "->" >> nextStepParser)
    RTTransStepAction lbl componentIdentifier actionIdentifier pathIdentifier nextStep . Position current startPos <$> getPosition

rtTransStepMulticastParser :: RTParser (RTTransStep ParserAnn)
rtTransStepMulticastParser = do
    current <- getState
    startPos <- getPosition
    _ <- reservedOp "{|"
    steps <- many1 nextStepParser
    _ <- reservedOp "|}"
    RTTransStepMuticast steps . Position current startPos <$> getPosition

rtTransStepConditionalParser :: RTParser (RTTransStep ParserAnn)
rtTransStepConditionalParser = do
    current <- getState
    startPos <- getPosition
    branches <- angles (sepBy conditionalBranchParser comma)
    RTTransStepConditional branches . Position current startPos <$> getPosition

  where

    conditionalBranchParser :: RTParser (ConstExpression ParserAnn, RTTransStep ParserAnn)
    conditionalBranchParser = do
        times <- constExpressionParser
        _ <- reservedOp "!"
        step <- nextStepParser
        return (times, step)

parensConstExprParser :: RTParser (ConstExpression ParserAnn)
parensConstExprParser = parens constExpressionParser

constIntParser :: RTParser (ConstExpression ParserAnn)
constIntParser = do
  current <- getState
  pos <- getPosition
  tInteger <- integerParser
  ConstInt tInteger . Position current pos <$> getPosition

constObjectParser :: RTParser (ConstExpression ParserAnn)
constObjectParser = do
  current <- getState
  pos <- getPosition
  ident <- identifierParser
  ConstObject ident . Position current pos <$> getPosition

constExpressionTermParser :: RTParser (ConstExpression ParserAnn)
constExpressionTermParser =
  try constIntParser
  <|> try constObjectParser
  <|> parensConstExprParser

-- Expression TerminaParser
constExpressionParser' :: RTParser (ConstExpression ParserAnn)
constExpressionParser' = Ex.buildExpressionParser  -- New parser
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

constExpressionParser :: RTParser (ConstExpression ParserAnn)
constExpressionParser = try structInitializerParser
  <|> constExpressionParser'

nextStepParser :: RTParser (RTTransStep ParserAnn)
nextStepParser =
    try transStepActionParser
    <|> try rtTransStepMulticastParser
    <|> rtTransStepConditionalParser

rtTransactionParser :: RTParser (RTElement ParserAnn)
rtTransactionParser = do
    current <- getState
    startPos <- getPosition
    _ <- reserved "transaction"
    transName <- identifierParser
    _ <- reservedOp "="
    firstStep <- nextStepParser
    _ <- semi
    RTTransaction transName firstStep . Position current startPos <$> getPosition

structInitializerParser :: RTParser (ConstExpression ParserAnn)
structInitializerParser = do
    current <- getState
    startPos <- getPosition
    fieldAssignments <- braces (sepBy fieldAssignmentParser comma)
    ConstStructInitializer fieldAssignments . Position current startPos <$> getPosition

  where

    fieldAssignmentParser :: RTParser (ConstFieldAssignment ParserAnn)
    fieldAssignmentParser = do
        fieldName <- identifierParser
        _ <- reservedOp "="
        ConstFieldAssignment fieldName <$> constExpressionParser

rtSituationParser :: RTParser (RTElement ParserAnn)
rtSituationParser = do
    current <- getState
    startPos <- getPosition
    _ <- reserved "rts"
    rtsName <- identifierParser
    _ <- reservedOp "="
    initializerExpr <- constExpressionParser
    _ <- semi
    RTSituation rtsName initializerExpr . Position current startPos <$> getPosition

-- | Top Level parser
topLevel :: RTParser [RTElement ParserAnn]
topLevel = many $
  try rtTransactionParser
  <|> rtSituationParser

contents :: RTParser a -> RTParser a
contents p = wspcs *> p <* eof

terminaRTParser :: RTParser [RTElement ParserAnn]
terminaRTParser = contents topLevel