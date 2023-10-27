module Main (main) where

-- import AST
import Parser.Parsing
import Options
import PPrinter
-- import Control.Applicative
import Semantic.TypeChecking
import Semantic.Monad (SemanticAnns)
import Semantic.Errors (ppError) --Printing errors

import Text.Parsec (runParser)

import qualified Data.Text.IO as TIO
import qualified Data.Text as T

import Control.Monad

import Modules.Modules
import Modules.Typing
import qualified Modules.Printing  as MPP
import AST.Core
import qualified AST.Parser as PAST
import qualified AST.Seman as SAST

-- FilePath and IO
import System.Path
import System.Path.IO

-- Containers
import qualified Data.Map.Strict as M

data MainOptions = MainOptions
  { optREPL :: Bool
  , optPrintAST :: Bool
  , optPrintASTTyped :: Bool
  , optOutputDir :: Maybe String
  , optChatty :: Bool
  }

instance Options MainOptions where
  defineOptions = MainOptions
        <$> simpleOption "repl" False
            "Choose Termina transpiler mode: Transpiler or Interpreter."
        <*> simpleOption "print" False
            "Prints AST after parsed"
        <*> simpleOption "printTyped" False
            "Prints AST after parsed + type"
        <*> simpleOption "outputdir" Nothing
            "Output file"
        <*> simpleOption "chatty" False
            "Chatty compilation"

fst3 :: (a ,b ,c ) -> a
fst3 (a,_,_) = a
snd3 :: (a , b , c) -> b
snd3 (_,b,_) = b

getFileSrc :: Path Absolute -> IO ( ModuleMode, Path Absolute)
getFileSrc path =
  doesDirectoryExist path >>= \isDir ->
  if isDir then
    let srcFile = (path </> fragment "src" <.> terminaExt) in
      do
        isSrcFile <- doesFileExist srcFile
        unless
          isSrcFile
          (fail ("Module name ("++ show path ++") is a directory but src found:" ++ show srcFile))
        return (DirMod, srcFile)
  else
    let filePath = path <.> terminaExt in
    do
      isFile <- doesFileExist filePath
      unless isFile (fail ("File not found:" ++ show filePath ++"."))
      return (SrcFile, filePath)

-- We need to Module names and File Paths
-- Load File takes an absolute path (could be something else but whatev)
loadFile :: Path Absolute -> IO (ModuleMode , PAST.TerminaProgram Annotation) --(TerminaProgram Annotation)
loadFile absPath = do
  -- Get file name
  (mMode, absPathFile) <- getFileSrc absPath
  -- read it
  src_code <- readStrictText absPathFile
  -- parse it
  case runParser terminaProgram () (toFilePath absPathFile) (T.unpack src_code) of
    Left err -> ioError $ userError $ "Parser Error ::\n" ++ show err
    Right term -> return (mMode , term)

routeToMain :: Path Absolute -> Path Absolute
routeToMain = takeDirectory

whenChatty :: MainOptions -> IO () -> IO ()
whenChatty = when . optChatty

-- Replicate users tree. There are two ways to definea module...
printModule :: ModuleMode -> Bool
  -- Src Dir
  -> Path Absolute
  -- Headers Dir
  -> Path Absolute
  --
  -> ModuleName
  -> [(ModuleName, ModuleMode)]
  -> SAST.AnnotatedProgram SemanticAnns
  -> IO ()
printModule mMode chatty srcdir hdrsdir mName deps tyModule = do
  when chatty $ print $ "PP Module:" ++ show mName
  -- Let's check if files already exists.
  hExists <- doesFileExist hFile
  cExists <- doesFileExist cFile
  -- If they exist create a copy .bkp
  when hExists (renameFile hFile (hFile <.> FileExt "bkp"))
  when cExists (renameFile cFile (cFile <.> FileExt "bkp"))
  -- Folder checking and creation
  when chatty $ print "Creating Project folders."
  -- Creating resulting project Folder Structure
  createDirectoryIfMissing True (takeDirectory (fileRoute srcdir))
  createDirectoryIfMissing True (takeDirectory (fileRoute hdrsdir))
  -- Now, both files are no more.
  when chatty $ print $ "Writing to" ++ show hFile
  let docMName = ppHeaderFileDefine $ MPP.moduleNameToText mName
  writeStrictText hFile (MPP.ppHeaderFile docMName (MPP.includes deps) tyModule)
  when chatty $ print $ "Writing to" ++ show cFile
  writeStrictText cFile (MPP.ppSourceFile (MPP.ppModuleName mName) tyModule)
  where
    fileRoute dd =
      case mMode of
        DirMod -> dd </> mName </> fragment "src"
        SrcFile -> dd </> mName
    (cFile,hFile) = (fileRoute srcdir <.> FileExt "c", fileRoute hdrsdir <.> FileExt "h")

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
              whenChatty opts (print ("Reading Main:" ++ show absPath))
              (mMode , terminaMain) <- loadFile absPath
              when (isMDir mMode) (fail "Main cannot be in a folder")
              -- Termina Map from paths to Parser ASTs.
              whenChatty opts $ print "Loading project and parsing modules"
              let baseMainName = takeBaseName absPath
              mapProject <- M.insert baseMainName (terminaProgramImports terminaMain, mMode, terminaMain) <$> loadProject (loadFile . (rootDir </>)) (terminaProgramImports terminaMain)
              whenChatty opts $ print "Finished Parsing"
              if M.size mapProject == 1
                -- Single file project.
                -- so we can still use it :sweat_smile: until the other part is done.
              then
                let
                  tAST = frags terminaMain
                in case typeCheckRun tAST of
                    Left err ->
                      TIO.putStr (render (ppError err)) >> fail "Type Check Error Fin."
                    Right typedAST
                        -> when (optPrintAST opts) (print tAST) >> print (optPrintAST opts)
                        >> when (optPrintASTTyped opts) (print typedAST)
                        >> maybe -- check this, it sees weird.
                                (print (ppHeaderFile [T.pack "output"] [] typedAST))
                                (\fn ->
                                  let -- System.Path
                                      header = fn ++ ".h"
                                      source = fn ++ ".c"
                                  in TIO.writeFile header (ppHeaderFile [T.pack fn] [] typedAST)
                                  >> TIO.writeFile source (ppSourceFile [T.pack fn] typedAST)
                                ) (optOutputDir opts)
                        >> print "Perfect ✓"
              else do
                --
                analyzeOrd <- either (fail . ("Cycle between modules: " ++) . show ) return (sortOrLoop (M.map fst3 mapProject))
                let toModuleAST = M.map mAstFromPair mapProject
                -- Let's make it interactive (for the use)
                -- typedProject :: Map ModuleName (ModuleAST TypedModule)
                typedProject <- foldM (\env m -> do
                     whenChatty opts $ print ("Type Checking Module: " ++ show m)
                     case typeModule toModuleAST m env of
                      Left err ->
                        print "--------" >>
                        TIO.putStrLn (render (MPP.ppModError err)) >>
                        fail ("[FAIL]")
                      Right typedM ->
                        whenChatty opts (print " >> [DONE]")
                        >> return (M.insert m typedM env)
                     ) M.empty (analyzeOrd ++ [baseMainName])
                whenChatty opts $ print "Finished Module typing"
                ----------------------------------------
                -- Printing Project
                whenChatty opts $ print "Printing Project"
                (srcDir, hdrsDir) <-
                     (\i -> (i </> fragment "src",i </> fragment "include")) <$>
                    (case optOutputDir opts of
                      Nothing -> return rootDir
                      Just fp -> makeAbsolute (fromFilePath fp))
                -- let srcDir = maybe rootDir fromFilePath (optOutputDir opts) </> fragment "src"
                -- let hdrsDir = maybe rootDir fromFilePath (optOutputDir opts) </> fragment "include"
                mapM_ (\(mName,mTyped) ->
                      --  Get deps modes
                      maybe (fail "Internal error: missing modulename in mapProject")
                          (return . snd3) (M.lookup mName mapProject)
                      >>= \moduleMode ->
                      mapM (\i -> maybe (fail "Internal error: something went missing in mapProject") (\(_,mode,_) -> return (i, mode)) (M.lookup i mapProject)) (moduleDeps mTyped) >>= \depModes ->
                      printModule moduleMode (optChatty opts) srcDir hdrsDir mName depModes (frags $ typedModule $ moduleData mTyped))
                      (M.toList typedProject)
                whenChatty opts $ print "Finished PProject"
                ----------------------------------------
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
