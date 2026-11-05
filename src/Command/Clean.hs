-- | termina clean: remove what the transpiler writes in a project and can
-- write again, output/ and .termina/.
module Command.Clean (
    cleanCmdArgsParser, cleanCommand, CleanCmdArgs
) where

import Configuration.Configuration (outputFolderName)

import Control.Monad (forM_, when)
import qualified Options.Applicative as O
import System.Directory

-- | termina clean takes no options.
data CleanCmdArgs = CleanCmdArgs
    deriving Show

cleanCmdArgsParser :: O.Parser CleanCmdArgs
cleanCmdArgsParser = pure CleanCmdArgs

-- | Run from the folder of the project.
cleanCommand :: CleanCmdArgs -> IO ()
cleanCommand CleanCmdArgs =
    forM_ [outputFolderName, ".termina"] $ \folder -> do
      exists <- doesDirectoryExist folder
      when exists $ do
        removeDirectoryRecursive folder
        putStrLn $ "Removed " ++ folder ++ "/"
