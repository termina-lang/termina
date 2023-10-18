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
import qualified AST.Parser as PAST

-- FilePath and IO
import System.Path
import System.Path.IO

-- Containers
import qualified Data.Map.Strict as M

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

getFileSrc :: Path Absolute -> IO (Path Absolute)
getFileSrc path =
  doesDirectoryExist path >>= \isDir ->
  if isDir then
    let srcFile = (path </> fragment "src" <.> terminaExt) in
      do
        isSrcFile <- doesFileExist srcFile
        unless
          isSrcFile
          (fail ("Module name ("++ show path ++") is a directory but src found:" ++ show srcFile))
        return srcFile
  else
    let filePath = path <.> terminaExt in
    do
      isFile <- doesFileExist filePath
      unless isFile (fail ("File not found:" ++ show filePath ++"."))
      return filePath

-- We need to Module names and File Paths
-- Load File takes an absolute path (could be something else but whatev)
loadFile :: Path Absolute -> IO (PAST.TerminaProgram Annotation) --(TerminaProgram Annotation)
loadFile absPath = do
  -- Get file name
  absPathFile <- getFileSrc absPath
  -- read it
  src_code <- readStrictText absPathFile
  -- parse it
  case runParser terminaProgram () (toFilePath absPathFile) (T.unpack src_code) of
    Left err -> ioError $ userError $ "Parser Error ::\n" ++ show err
    Right term -> return term

routeToMain :: Path Absolute -> Path Absolute
routeToMain = takeDirectory

main :: IO ()
main = runCommand $ \opts args ->
    if optREPL opts then
        putStrLn "Not Implemented yet"
    else
        case args of
            [filepath] -> do
              -- Get root File Path
              let fspath = fromFilePath filepath
              absPath <- makeAbsolute fspath
              let rootDir = routeToMain absPath
              -- Main is special
              terminaMain <- loadFile absPath
              -- let mainName = takeBaseName fspath
              -- Termina Map from paths to Parser ASTs.
              mapProject <- loadProject (loadFile . (rootDir </>)) (terminaProgramImports terminaMain)
              --
              print (sortOrLoop $ M.map fst mapProject)
            ----------------------------------------
            -- Wrong arguments Errors
            [] -> ioError $ userError "No file?"
            _ -> ioError $ userError "Arguments error"


              -- src_code <- readFile filepath
              -- case runParser terminaProgram () filepath src_code of
              --   Left err -> ioError $ userError $ "Parser Error ::\n" ++ show err
              --   Right (Termina mdls frags) ->
              --     let mdlsPF = map moduleStringToPath mdls in
              --       mapM_ loadFile  (map moduleIdentifier mdlsPF)
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
