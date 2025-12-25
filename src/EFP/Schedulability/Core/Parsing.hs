module EFP.Schedulability.Core.Parsing where
 
-- Importing parser combinators
import Text.Parsec hiding (Error, Ok)

-- Importing tokenizer
import qualified Text.Parsec.Language as Lang
import qualified Text.Parsec.Token    as Tok

import qualified Text.Parsec.Expr as Ex
import Data.Char
import Utils.Annotations
import EFP.Schedulability.Core.AST
import EFP.Schedulability.Core.Types

type SchedParser = Parsec String FilePath

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

reserved :: String -> SchedParser ()
reserved = Tok.reserved lexer

reservedOp :: String -> SchedParser ()
reservedOp = Tok.reservedOp lexer

identifierParser :: SchedParser String
identifierParser = Tok.identifier lexer

angles :: SchedParser a -> SchedParser a
angles = Tok.angles lexer

braces :: SchedParser a -> SchedParser a
braces = Tok.braces lexer

brackets :: SchedParser a -> SchedParser a
brackets = Tok.brackets lexer

parens :: SchedParser a -> SchedParser a
parens = Tok.parens lexer

wspcs :: SchedParser ()
wspcs = Tok.whiteSpace lexer

comma :: SchedParser String
comma = Tok.comma lexer

semi :: SchedParser String
semi = Tok.semi lexer

number :: Integer -> SchedParser Char -> SchedParser Integer
number base baseDigit = do
    digits <- many1 baseDigit
    let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
    seq n (return n)

sign :: SchedParser (Integer -> Integer)
sign = (char '-' >> return negate)
  <|> (char '+' >> return id)
  <|> return id

hexadecimal :: SchedParser Integer
hexadecimal = Tok.lexeme lexer $
  char '0' >> oneOf "xX" >> number 16 hexDigit

decimal :: SchedParser Integer
decimal = Tok.natural lexer

parensConstExprParser :: SchedParser (ConstExpression ParserAnn)
parensConstExprParser = parens constExpressionParser

decIntOrFloat :: SchedParser (Either Integer Double)
decIntOrFloat = try float <|> (Left <$> decimal)
  where
    digits1 = many1 digit

    exponentPart = do
      e <- oneOf "eE"
      s <- option "" (fmap (:[]) (oneOf "+-"))
      ds <- digits1
      return $ e : s ++ ds

    float = do
      intp <- decimal  -- parses the integer part
      try (do _ <- char '.'
              notFollowedBy (char '.')     -- prevents consuming '..'
              frac <- option "" digits1
              expn <- option "" exponentPart
              let s = show intp ++ "." ++ frac ++ expn
              case reads s of
                [(d,"")] -> return $ Right d
                _        -> fail "invalid float")
        <|>
      -- Case: int exponent (e.g., 1e3)
        (do expn <- exponentPart
            let s = show intp ++ expn
            case reads s of
              [(d,"")] -> pure (Right d)
              _        -> fail "invalid float")

constNumberParser :: SchedParser (ConstExpression ParserAnn)
constNumberParser = do
  current <- getState
  pos <- getPosition
  try (do i <- hexadecimal
          ConstInt (TInteger i HexRepr) . Position current pos <$> getPosition)
    <|> do
          f <- sign
          x <- decIntOrFloat
          case x of
            Left  i -> ConstInt (TInteger (f i) DecRepr) . Position current pos <$> getPosition
            Right d -> ConstDouble (applySign f d) . Position current pos <$> getPosition
  where
    applySign g d =
      -- g is either id or negate; apply to Double without pattern matching
      if g 1 == (-1) then negate d else d

constObjectParser :: SchedParser (ConstExpression ParserAnn)
constObjectParser = do
  current <- getState
  pos <- getPosition
  ident <- identifierParser
  ConstObject ident . Position current pos <$> getPosition

constExpressionTermParser :: SchedParser (ConstExpression ParserAnn)
constExpressionTermParser =
  try constNumberParser
  <|> try constObjectParser
  <|> parensConstExprParser

-- Expression TerminaParser
constExpressionParser' :: SchedParser (ConstExpression ParserAnn)
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

constExpressionParser :: SchedParser (ConstExpression ParserAnn)
constExpressionParser = constExpressionParser'

contents :: SchedParser a -> SchedParser a
contents p = wspcs *> p <* eof
