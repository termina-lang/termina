module Main (main) where

-- import AST
import Parsing
import Options
import PPrinter
-- import Control.Applicative
import Semantic.TypeChecking

import Text.Parsec (runParser)

import qualified Data.Text.IO as TIO

import Control.Monad

-- data Mode = Transpiler | Interpreter

data MainOptions = MainOptions
  {optREPL :: Bool
  , optPrintAST :: Bool
  , optPrintASTTyped :: Bool
  , optOutput :: Maybe String
  }

instance Options MainOptions where
  defineOptions = MainOptions
        <$> simpleOption "repl" False
            "Choose Termina transpiler mode: Transpiler or Interpreter."
        <*> simpleOption "print" False
            "Prints AST after parsed"
        <*> simpleOption "printTyped" False
            "Prints AST after parsed + type"
        <*> simpleOption "output" Nothing
            "Output file"

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
                Left err -> ioError $ userError $ "Parser Error ::\n" ++ show err
                Right ast ->
                  case typeCheckRun ast of
                    Left err -> ioError $ userError $ "Type Check Error ::\n" ++ show err
                    Right tast ->
                      when (optPrintAST opts) (print ast) >>
                      when (optPrintASTTyped opts) (print tast) >>
                      case optOutput opts of
                        Nothing -> print (ppHeaderFile tast)
                        Just fn ->
                          let header = fn ++ ".h" in
                          let source = fn ++ ".c" in
                            TIO.writeFile header (ppHeaderFile tast)
                            >> TIO.writeFile source (ppSourceFile tast)
            _ -> ioError $ userError "Arguments error"
