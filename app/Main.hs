module Main (main) where

-- import AST
import Parser.Parsing
import Options
import PPrinter
-- import Control.Applicative
import Semantic.TypeChecking
import Semantic.Monad
import Semantic.Errors (ppError) --Printing errors

import Text.Parsec (runParser)

import qualified Data.Text.IO as TIO
import qualified Data.Text as T

import Control.Monad

import Modules.Modules
import Modules.Typing
import qualified Modules.Printing  as MPP

import qualified AST.Parser as PAST
import qualified AST.Seman as SAST

import DataFlow.DF

-- FilePath and IO
import System.Path
import System.Path.IO

-- Containers
import qualified Data.Map.Strict as M
import PPrinter.Application.Initialization (ppInitFile)
import PPrinter.Application.OS.RTEMSNOEL (ppMainFile)

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
  createDirectoryIfMissing True (takeDirectory (srcFileRoute srcdir))
  createDirectoryIfMissing True (takeDirectory (hdrFileRoute hdrsdir))
  -- Now, both files are no more.
  when chatty $ print $ "Writing to" ++ show hFile
  let docMName = ppHeaderFileDefine $ MPP.moduleNameToText mName mMode
  writeStrictText hFile (MPP.ppHeaderFile docMName (MPP.includes deps) tyModule)
  when chatty $ print $ "Writing to" ++ show cFile
  writeStrictText cFile (MPP.ppSourceFile (MPP.ppModuleName mName mMode) tyModule)
  where
    srcFileRoute dd =
      case mMode of
        DirMod -> dd </> mName </> fragment "src"
        SrcFile -> dd </> mName
    hdrFileRoute dd =
      case mMode of
        DirMod -> dd </> mName </> fragment "header"
        SrcFile -> dd </> mName
    (cFile,hFile) = (srcFileRoute srcdir <.> FileExt "c", hdrFileRoute hdrsdir <.> FileExt "h")

printInitFile :: Bool -> Path Absolute -> [(ModuleName, ModuleMode, SAST.AnnotatedProgram SemanticAnns)] -> IO ()
printInitFile chatty targetDir prjprogs = do
  when chatty $ print "Creating init file"
  createDirectoryIfMissing True targetDir
  iExists <- doesFileExist initFile
  when iExists (renameFile initFile (initFile <.> FileExt "bkp"))
  writeStrictText initFile (render $ ppInitFile prjprogs)
  where
    initFile = targetDir </> fragment "init" <.> FileExt "c"

printMainFile :: Bool -> Path Absolute -> [(ModuleName, ModuleMode, SAST.AnnotatedProgram SemanticAnns)] -> IO ()
printMainFile chatty targetDir prjprogs = do
  when chatty $ print "Creating main file"
  createDirectoryIfMissing True targetDir
  iExists <- doesFileExist mainFile
  when iExists (renameFile mainFile (mainFile <.> FileExt "bkp"))
  writeStrictText mainFile (render $ ppMainFile prjprogs)
  where
    mainFile = targetDir </> fragment "main" <.> FileExt "c"

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
              -- Prepare stuff to print.
              -- A little bit nonsense to do it here.
              -- But we need it.
              (outputDir, srcDir, hdrsDir) <-
                     (\i -> (i, i </> fragment "src",i </> fragment "include")) <$>
                    (case optOutputDir opts of
                      Nothing -> return rootDir
                      Just fp -> do
                      -- Check optOutputDir exists.
                        p <- makeAbsolute (fromFilePath fp)
                        exists <- doesDirectoryExist p
                        unless exists (fail "Output Folder does not exist.")
                        return p)
              --
              let modDeps = M.map fst3 mapProject
              whenChatty opts $ print ("Deps:" ++ show modDeps)

              analyzeOrd <- either (fail . ("Cycle between modules: " ++) . show ) return (sortOrLoop modDeps)
              whenChatty opts $ print ("Project Order: " ++ show analyzeOrd)
              --
              let toModuleAST = M.map mAstFromPair mapProject
              -- Let's make it interactive (for the use)
              -- typedProject :: Map ModuleName (ModuleAST TypedModule)
              typedProject <- foldM (\env m -> do
                   whenChatty opts $ print ("Type Checking Module: " ++ show m)
                   -- Here is something wrong!!
                   case typeModule toModuleAST m env of
                    Left err ->
                      print "--------" >>
                      TIO.putStrLn (render (MPP.ppModError err)) >>
                      fail "[FAIL]"
                    Right typedM ->
                      whenChatty opts (print " >> [DONE]")
                      ----------------------------------------
                      -- Use/Def Ann
                      >> maybe
                          (when (optChatty opts) (print ("UseDef " ++ show m ++  " Passed")))
                          (fail . (("UseDef ["++ show m ++"]  Error : ") ++) . show)
                          (runUDTerminaProgram (typedModule $ moduleData typedM))
                      ----------------------------------------
                      >> return (M.insert m typedM env)
                   ) M.empty (analyzeOrd ++ [baseMainName])
              whenChatty opts $ print "Finished Module typing"
              ----------------------------------------
              -- Printing Project
              whenChatty opts $ print "Printing Project"
              mapM_ (\(mName,mTyped) ->
                    --  Get deps modes
                    maybe (fail "Internal error: missing modulename in mapProject")
                        (return . snd3) (M.lookup mName mapProject)
                    >>= \moduleMode ->
                    mapM (\i -> maybe (fail "Internal error: something went missing in mapProject") (\(_,mode,_) -> return (i, mode)) (M.lookup i mapProject)) (moduleDeps mTyped) >>= \depModes ->
                      printModule moduleMode (optChatty opts) srcDir hdrsDir mName depModes (SAST.frags $ typedModule $ moduleData mTyped))
                    (M.toList typedProject)
              allModules <- mapM (\(mName, mTyped) -> maybe (fail "Internal error: something went missing in mapProject") (\(_,mMode,_) -> return (mName, mMode, mTyped)) (M.lookup mName mapProject)) $ M.toList typedProject
              let prjprogs = map (\(mName, mMode, mTyped) -> (mName, mMode, SAST.frags $ typedModule $ moduleData mTyped)) allModules
              printInitFile (optChatty opts) outputDir prjprogs
              printMainFile (optChatty opts) outputDir prjprogs
              whenChatty opts $ print "Finished PProject"
                ----------------------------------------
            ----------------------------------------
            -- Wrong arguments Errors
            [] -> ioError $ userError "No file?"
            _ -> ioError $ userError "Arguments error"
