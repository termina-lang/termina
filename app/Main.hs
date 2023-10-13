module Main (main) where

-- import AST
import Parser.Parsing
import Options
import PPrinter
-- import Control.Applicative
import Semantic.TypeChecking

import Text.Parsec (runParser)

import qualified Data.Text.IO as TIO
import qualified Data.Text as T

import Control.Monad

import Modules.Modules
import AST.Core

import System.Path
import System.Path.IO

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

-- Relative file path!
loadFile :: Path Unrooted -> IO () --(TerminaProgram Annotation)
loadFile pathFile = do
  absHere <- makeAbsolute (fromFilePath "")
  let absPathFile = absHere </> pathFile
  eFile <- doesFileExist absPathFile
  unless eFile (fail ("File \"" ++ toFilePath absPathFile ++ "\" does not exist :@."))
  src_code <- readStrictText absPathFile
  case runParser terminaProgram () (toFilePath absPathFile) (T.unpack src_code) of
    Left err -> ioError $ userError $ "Parser Error ::\n" ++ show err
    Right _ -> print "ok"


main :: IO ()
main = runCommand $ \opts args ->
    if optREPL opts then
        putStrLn "Not Implemented yet"
    else
        case args of
            [] -> ioError $ userError "No file?"
            [filepath] -> do
              src_code <- readFile filepath
              case runParser terminaProgram () filepath src_code of
                Left err -> ioError $ userError $ "Parser Error ::\n" ++ show err
                Right (Termina mdls frags) ->
                  let mdlsPF = map moduleStringToPath mdls in
                    mapM_ loadFile  (map moduleIdentifier mdlsPF)
                  -- case typeCheckRun ast of
                  --   Left err -> ioError $ userError $ "Type Check Error ::\n" ++ show err
                  --   Right tast ->
                  --     when (optPrintAST opts) (print ast) >>
                  --     when (optPrintASTTyped opts) (print tast) >>
                  --     case optOutput opts of
                  --       Nothing -> print (ppHeaderFile tast)
                  --       Just fn ->
                  --         let header = fn ++ ".h" in
                  --         let source = fn ++ ".c" in
                  --           TIO.writeFile header (ppHeaderFile tast)
                  --           >> TIO.writeFile source (ppSourceFile tast)
            _ -> ioError $ userError "Arguments error"
