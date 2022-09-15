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

-- | This is what we will build in this section
type PositionAST = AAST SourcePos

----------------------------------------
-- Lexer
----------------------------------------

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef
  where
    reservedOps = ["Global","Function","Agent"]
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
                   , Tok.reservedNames = reservedOps
                   -- | Is the language case sensitive? It should be
                   , Tok.caseSensitive = True
                   }

-- Lexemes

wspcs :: Parsec String () ()
wspcs = Tok.whiteSpace lexer

braces :: Parsec String () a -> Parsec String () a
braces = Tok.braces lexer

reserved :: String -> Parsec String () ()
reserved = Tok.reserved lexer

----------------------------------------
-- Parser
----------------------------------------

multLines :: Parser [String]
multLines = sepBy (many (satisfy (\c -> (c /= '}') && (c /= '{')) )) newline

globals :: Parser [Global]
globals = reserved "Global" >> braces multLines

functions :: Parser [Function]
functions = reserved "Function" >> braces multLines

agents :: Parser [Agent]
agents = reserved "Agent" >> braces multLines

withPos :: Parser a -> Parser (a, SourcePos)
withPos p = do
  pos <- getPosition
  res <- p
  return (res,pos)

-- This is an unrealistic (but extremely simple) sourcecode parser
topLevel :: Parser PositionAST
topLevel = do
  glbs_pos <- withPos globals
  wspcs
  fns_pos <- withPos functions
  wspcs
  ags_pos <- withPos agents
  return $ AST glbs_pos fns_pos ags_pos

contents :: Parser a -> Parser a
contents p = wspcs *> p <* eof
