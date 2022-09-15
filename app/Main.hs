module Main (main) where

-- import AST
import Parsing
import Options
-- import Control.Applicative

import Text.Parsec (runParser)

-- data Mode = Transpiler | Interpreter

data MainOptions = MainOptions
  {optREPL :: Bool
  }

instance Options MainOptions where
  defineOptions = pure MainOptions
        <*> simpleOption "repl" False
            "Choose Termina transpiler mode: Transpiler or Interpreter."

main :: IO ()
main = runCommand $ \opts args ->
    if optREPL opts then
        putStrLn "Not Implemented yet"
    else
        case args of
            [] -> ioError $ userError "No file?"
            [filepath] -> do
              src_code <- readFile filepath
              case runParser (contents topLevel) () filepath src_code of
                Left err -> ioError $ userError $ "Parser Error: " ++ show err
                Right _ -> putStrLn "Ok!"
            _ -> ioError $ userError "Too much arguments king!"
