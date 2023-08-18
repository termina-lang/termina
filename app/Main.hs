module Main (main) where

-- import AST
import Parsing
import Options
import PPrinter
-- import Control.Applicative
import Semantic.TypeChecking

import Text.Parsec (runParser)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- data Mode = Transpiler | Interpreter

data MainOptions = MainOptions
  {optREPL :: Bool
  , optPrintAST :: Bool
  , optOutput :: Maybe String
  }

instance Options MainOptions where
  defineOptions = MainOptions
        <$> simpleOption "repl" False
            "Choose Termina transpiler mode: Transpiler or Interpreter."
        <*> simpleOption "print" False
            "Prints AST after parsed"
        <*> simpleOption "output" Nothing
            "Output file"

main :: IO ()
main = runCommand $ \opts args ->
    let
      output = maybe print TIO.writeFile (optOutput opts)
    in
    if optREPL opts then
        putStrLn "Not Implemented yet"
    else
        case args of
            [] -> ioError $ userError "No file?"
            [filepath] -> do
              src_code <- readFile filepath
              case runParser (contents topLevel) () filepath src_code of
                Left err -> ioError $ userError $ "Parser Error: " ++ show err
                Right ast ->
                  output
                  (if optPrintAST opts then
                     case typeCheckRun ast of
                       Left err -> T.pack $ "ERRORRRRRRRRR:: " ++ show err
                       Right tast -> ppHeaderFile tast
                  else
                    T.pack "Ok!" )
            _ -> ioError $ userError "Too much arguments king!"
