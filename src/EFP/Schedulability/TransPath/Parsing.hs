module EFP.Schedulability.TransPath.Parsing where

-- Importing parser combinators
import           Text.Parsec hiding (Error, Ok)

-- Importing tokenizer
import qualified Text.Parsec.Language as Lang
import qualified Text.Parsec.Token    as Tok

import EFP.Schedulability.TransPath.AST
import Semantic.Types

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

wspcs :: PathParser ()
wspcs = Tok.whiteSpace lexer

comma :: PathParser String
comma = Tok.comma lexer

conditionalParser :: PathParser (WCEPathBlock SemanticAnn)
conditionalParser = undefined

wcepPathBlockParser :: PathParser (WCEPathBlock SemanticAnn)
wcepPathBlockParser =
    try conditionalParser

transactionalWCEPParser :: PathParser (TransactionalWCEPath SemanticAnn)
transactionalWCEPParser = do
    _ <- reserved "wcep"
    clsName <- identifierParser
    _ <- reservedOp "::"
    elementName <- identifierParser
    _ <- reservedOp "::"
    memberFunction <- identifierParser
    _ <- reservedOp "="
    blocks <- brackets (sepBy (wspcs *> wcepPathBlockParser <* wspcs) comma)
    return $ TransactionalWCEPath clsName elementName memberFunction blocks

-- | Top Level parser
topLevel :: PathParser [TransactionalWCEPath SemanticAnn]
topLevel = many $
  try transactionalWCEPParser