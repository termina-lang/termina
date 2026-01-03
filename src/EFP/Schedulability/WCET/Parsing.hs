module EFP.Schedulability.WCET.Parsing 
  (terminaWCETParser) where

-- Importing parser combinators
import Text.Parsec hiding (Error, Ok)

-- Importing tokenizer
import qualified Text.Parsec.Language as Lang
import qualified Text.Parsec.Token    as Tok

import EFP.Schedulability.WCET.AST
import qualified Text.Parsec.Expr as Ex
import Data.Char
import Utils.Annotations
import EFP.Schedulability.Core.Types

type WCETParser = Parsec String FilePath

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
                   , Tok.identLetter = alphaNum <|> char '_' <|> char '-'
                   -- | Operators begin with
                   , Tok.opStart = oneOf "="
                   , Tok.reservedNames = reservedNames
                   , Tok.reservedOpNames = [
                      "::" -- Scoping
                      ,"=" -- Assignment
                      ,"(" -- Parens
                      ,")" -- Parens
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
                    ]
                   -- | Is the language case sensitive? It should be
                   , Tok.caseSensitive = True
                   }

reserved :: String -> WCETParser ()
reserved = Tok.reserved lexer

reservedOp :: String -> WCETParser ()
reservedOp = Tok.reservedOp lexer

identifierParser :: WCETParser String
identifierParser = Tok.identifier lexer

parens :: WCETParser a -> WCETParser a
parens = Tok.parens lexer

wspcs :: WCETParser ()
wspcs = Tok.whiteSpace lexer

comma :: WCETParser String
comma = Tok.comma lexer

semi :: WCETParser String
semi = Tok.semi lexer

braces :: WCETParser a -> WCETParser a
braces = Tok.braces lexer

number :: Integer -> WCETParser Char -> WCETParser Integer
number base baseDigit = do
    digits <- many1 baseDigit
    let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
    seq n (return n)

sign :: WCETParser (Integer -> Integer)
sign = (char '-' >> return negate)
  <|> (char '+' >> return id)
  <|> return id

hexadecimal :: WCETParser Integer
hexadecimal = Tok.lexeme lexer $
  char '0' >> oneOf "xX" >> number 16 hexDigit

parensConstExprParser :: WCETParser (ConstExpression ParserAnn)
parensConstExprParser = parens constExpressionParser

constNumberParser :: WCETParser (ConstExpression ParserAnn)
constNumberParser = Tok.lexeme lexer $ do
  current <- getState
  pos <- getPosition
  try (do i <- hexadecimal
          ConstInt (TInteger i HexRepr) . Position current pos <$> getPosition)
    <|> do
          f <- sign
          x <- Tok.naturalOrFloat lexer
          case x of
            Left  i -> ConstInt (TInteger (f i) DecRepr) . Position current pos <$> getPosition
            Right d -> ConstDouble (applySign f d) . Position current pos <$> getPosition
  where
    applySign g d =
      -- g is either id or negate; apply to Double without pattern matching
      if g 1 == (-1) then negate d else d

constObjectParser :: WCETParser (ConstExpression ParserAnn)
constObjectParser = do
  current <- getState
  pos <- getPosition
  ident <- identifierParser
  ConstObject ident . Position current pos <$> getPosition

constExpressionTermParser :: WCETParser (ConstExpression ParserAnn)
constExpressionTermParser =
  try constNumberParser
  <|> try constObjectParser
  <|> parensConstExprParser

-- Expression TerminaParser
constExpressionParser :: WCETParser (ConstExpression ParserAnn)
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

transactionalWCETParser :: WCETParser (TransactionalWCET ParserAnn)
transactionalWCETParser = do
    current <- getState
    startPos <- getPosition
    clsName <- identifierParser
    _ <- reservedOp "::"
    elementName <- identifierParser
    _ <- reservedOp "::"
    memberFunction <- identifierParser
    constParams <- parens (sepBy identifierParser comma)
    _ <- reservedOp "="
    wcet <- constExpressionParser
    TransactionalWCET clsName elementName memberFunction constParams wcet . Position current startPos <$> getPosition

platformAssignmentParser :: WCETParser (WCETPlatformAssignment ParserAnn)
platformAssignmentParser = do
    current <- getState
    startPos <- getPosition
    _ <- reserved "wcet"
    platformName <- identifierParser
    _ <- reservedOp "="
    wcets <- braces (sepBy transactionalWCETParser comma)
    _ <- semi
    WCETPlatformAssignment platformName wcets . Position current startPos <$> getPosition

-- | Top Level parser
topLevel :: WCETParser [WCETPlatformAssignment ParserAnn]
topLevel = many $
  try platformAssignmentParser

contents :: WCETParser a -> WCETParser a
contents p = wspcs *> p <* eof

terminaWCETParser :: WCETParser [WCETPlatformAssignment ParserAnn]
terminaWCETParser = contents topLevel