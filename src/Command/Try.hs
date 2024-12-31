module Command.Try (
    tryCmdArgsParser, tryCommand, TryCmdArgs
) where

import Options.Applicative
import Control.Monad
import Command.Utils
import Modules.Modules
import Command.Types
import System.FilePath
import System.Exit
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map as M
import Parser.Parsing (terminaModuleParser)
import Text.Parsec (runParser)
import Generator.LanguageC.Printer (runCPrinter)
import Semantic.TypeChecking (runTypeChecking, typeTerminaModule)
import Generator.CodeGen.Module (runGenSourceFile, runGenHeaderFile)
import Semantic.Monad
import Core.AST
import Configuration.Configuration 
import Configuration.Platform
import Utils.Errors
import Parser.Errors
import Utils.Annotations
import Text.Parsec.Error

-- | Data type for the "try" command arguments
data TryCmdArgs =
    TryCmdArgs
        FilePath -- ^ Path of the file to translate
        Bool -- ^ Check variable usage
        Bool -- ^ Print header file
        Bool -- ^ Debug build
    deriving (Show,Eq)

-- | Parser for the "new" command arguments
tryCmdArgsParser :: Parser TryCmdArgs
tryCmdArgsParser = TryCmdArgs
    <$> argument str (metavar "FILE"
        <> help "Path of the file to translate")
    <*> switch (long "no-usage-checking" <> help "Disable variable usage checking")
    <*> switch (long "print-header" <> help "Print header file")
    <*> switch (long "debug" <> help "Debug build")

-- | Load Termina file 
loadSingleModule ::
  -- | Path of the file to load
  FilePath
  -> IO ParsedModule
loadSingleModule filePath = do
    let noExtension = dropExtension filePath
    -- read it
    src_code <- TIO.readFile filePath
    -- parse it
    case runParser terminaModuleParser () filePath (T.unpack src_code) of
        Left err -> 
            let pErr = annotateError (Position (errorPos err) (errorPos err)) (EParseError err)
                fileMap = M.singleton filePath src_code
            in
            TIO.putStrLn (toText pErr fileMap) >> exitFailure
        Right term -> do
            -- Imported modules are ignored
            return $ TerminaModuleData noExtension filePath [] src_code (ParsingData . frags $ term)

typeSingleModule :: ParsedModule -> IO TypedModule
typeSingleModule parsedModule = do
    let config = (\c -> c{ enableSystemInit = True }) $ defaultConfig "test" TestPlatform
        result = runTypeChecking (makeInitialGlobalEnv (Just config) []) (typeTerminaModule . parsedAST . metadata $ parsedModule)
    case result of
        (Left err) ->
            let sourceFilesMap = M.fromList [(fullPath parsedModule, sourcecode parsedModule)] in
            TIO.putStrLn (toText err sourceFilesMap) >> exitFailure
        (Right (typedProgram, _)) -> 
            return $ TerminaModuleData
                    (qualifiedName parsedModule)
                    (fullPath parsedModule)
                    []
                    (sourcecode parsedModule)
                    (SemanticData typedProgram)

printSourceModule :: Bool -> BasicBlocksModule -> IO ()
printSourceModule debugBuild bbModule = do
    let tAST = basicBlocksAST . metadata $ bbModule
        config = defaultConfig "test" TestPlatform
    case runGenSourceFile config (qualifiedName bbModule) tAST of
        Left err -> die. errorMessage $ show err
        Right cSourceFile -> TIO.putStrLn $ runCPrinter debugBuild cSourceFile

printHeaderModule :: Bool -> BasicBlocksModule -> IO ()
printHeaderModule debugBuild bbModule = do
    let tAST = basicBlocksAST . metadata $ bbModule
        configParams = defaultConfig "test" TestPlatform
        moduleDeps = (\(ModuleDependency qname _) -> qname) <$> importedModules bbModule
    case runGenHeaderFile configParams False (qualifiedName bbModule) moduleDeps tAST M.empty of
        Left err -> die . errorMessage $ show err
        Right cHeaderFile -> TIO.putStrLn $ runCPrinter debugBuild cHeaderFile

-- | Command handler for the "try" command
tryCommand :: TryCmdArgs -> IO ()
tryCommand (TryCmdArgs targetFile noUsageChecking printHeader debugBuild) = do
    -- | Load and parse the module
    terminaModule <- loadSingleModule targetFile
    -- | Type check the module
    typedModule <- typeSingleModule terminaModule
    -- | Generate basic blocks
    case genBasicBlocksModule typedModule of
        Left err -> 
            let sourceFilesMap = M.fromList [(fullPath typedModule, sourcecode typedModule)] in
            TIO.putStrLn (toText err sourceFilesMap) >> exitFailure
        Right bbModule -> do
            maybe (return ()) 
                (\err -> 
                    let sourceFilesMap = M.fromList [(fullPath bbModule, sourcecode bbModule)] in
                    TIO.putStrLn (toText err sourceFilesMap) >> exitFailure) 
                $ basicBlockPathsCheckModule bbModule
            -- | Check variable usage (if enabled)
            unless noUsageChecking (
                    maybe (return ()) 
                        (\err -> 
                            let sourceFilesMap = M.fromList [(fullPath bbModule, sourcecode bbModule)] in
                            TIO.putStrLn (toText err sourceFilesMap) >> exitFailure) 
                        $ useDefCheckModule bbModule
                )
            if printHeader then
                -- | Print the resulting header file into the standard output
                printHeaderModule debugBuild bbModule
            else
                -- | Print the resulting source file into the standard output
                printSourceModule debugBuild bbModule




