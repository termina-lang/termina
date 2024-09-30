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
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Map.Strict as M
import Parser.Parsing (terminaModuleParser)
import Text.Parsec (runParser)
import Generator.LanguageC.Printer (runCPrinter)
import Semantic.TypeChecking (runTypeChecking, typeTerminaModule)
import Semantic.Errors.PPrinting (ppError)
import Generator.CodeGen.Module (runGenSourceFile, runGenHeaderFile)
import Semantic.Monad
import Core.AST

-- | Data type for the "try" command arguments
data TryCmdArgs =
    TryCmdArgs
        FilePath -- ^ Path of the file to translate
        Bool -- ^ Check variable usage
        Bool -- ^ Print header file
    deriving (Show,Eq)

-- | Parser for the "new" command arguments
tryCmdArgsParser :: Parser TryCmdArgs
tryCmdArgsParser = TryCmdArgs
    <$> argument str (metavar "FILE"
        <> help "Path of the file to translate")
    <*> switch (long "no-usage-checking" <> help "Disable variable usage checking")
    <*> switch (long "print-header" <> help "Print header file")

-- | Load Termina file 
loadSingleModule ::
  -- | Path of the file to load
  FilePath
  -> IO ParsedModule
loadSingleModule filePath = do
    let noExtension = dropExtension filePath
    -- read it
    src_code <- TLIO.readFile filePath
    -- parse it
    case runParser terminaModuleParser () filePath (TL.unpack src_code) of
        Left err -> die . errorMessage $ "Parsing error: " ++ show err
        Right term -> do
            imports <- getModuleImports term
            return $ TerminaModuleData noExtension filePath imports src_code (ParsingData . frags $ term)

typeSingleModule :: ParsedModule -> IO TypedModule
typeSingleModule parsedModule = do
    let result = runTypeChecking (makeInitialGlobalEnv []) (typeTerminaModule . parsedAST . metadata $ parsedModule)
    case result of
        (Left err) ->
            let sourceFilesMap = M.fromList [(fullPath parsedModule, sourcecode parsedModule)] in
            ppError sourceFilesMap err >> exitFailure
        (Right (typedProgram, _)) -> 
            return $ TerminaModuleData
                    (qualifiedName parsedModule)
                    (fullPath parsedModule)
                    []
                    (sourcecode parsedModule)
                    (SemanticData typedProgram)

printSourceModule :: BasicBlocksModule -> IO ()
printSourceModule bbModule = do
    let tAST = basicBlocksAST . metadata $ bbModule
    case runGenSourceFile (qualifiedName bbModule) tAST of
        Left err -> die. errorMessage $ show err
        Right cSourceFile -> TIO.putStrLn $ runCPrinter cSourceFile

printHeaderModule :: BasicBlocksModule -> IO ()
printHeaderModule bbModule = do
    let tAST = basicBlocksAST . metadata $ bbModule
    case runGenHeaderFile False (qualifiedName bbModule) (importedModules bbModule) tAST M.empty of
        Left err -> die . errorMessage $ show err
        Right cHeaderFile -> TIO.putStrLn $ runCPrinter cHeaderFile

-- | Command handler for the "try" command
tryCommand :: TryCmdArgs -> IO ()
tryCommand (TryCmdArgs targetFile noUsageChecking printHeader) = do
    -- | Load and parse the module
    terminaModule <- loadSingleModule targetFile
    -- | Type check the module
    typedModule <- typeSingleModule terminaModule
    -- | Generate basic blocks
    bbModule <- genBasicBlocksModule typedModule
    -- | Check variable usage (if enabled)
    unless noUsageChecking (useDefCheckModule bbModule)
    if printHeader then
        -- | Print the resulting header file into the standard output
        printHeaderModule bbModule
    else
        -- | Print the resulting source file into the standard output
        printSourceModule bbModule




