-- | The folder of the project that a command runs on, looked for from the
-- current folder up.
module Command.ProjectRootSpec (spec) where

import Command.Utils (enterProject)

import Control.Monad (forM_)
import System.Directory
import System.FilePath
import Test.Hspec

-- | An application with a library in a local folder inside it and a package
-- in its vendor/, each with its termina.yaml.
withTree :: (FilePath -> IO a) -> IO a
withTree action = do
    tmp <- getTemporaryDirectory >>= canonicalizePath
    let root = tmp </> "termina-root-spec"
    removePathForcibly root
    forM_ ["obdh/src/pus", "obdh/libs/drivers/src/drivers", "obdh/vendor/github.com/acme/ccsds/src/ccsds"] $
        createDirectoryIfMissing True . (root </>)
    forM_ ["obdh", "obdh/libs/drivers", "obdh/vendor/github.com/acme/ccsds"] $ \folder ->
        writeFile (root </> folder </> "termina.yaml") ""
    action root

-- | The folder that enterProject goes to from a folder.
projectFrom :: FilePath -> IO FilePath
projectFrom folder = withCurrentDirectory folder (enterProject >> getCurrentDirectory)

spec :: Spec
spec = describe "folder of the project" $ do

  it "is the nearest folder up with a termina.yaml" $
    withTree $ \root -> do
      projectFrom (root </> "obdh" </> "src" </> "pus") `shouldReturn` (root </> "obdh")
      projectFrom (root </> "obdh") `shouldReturn` (root </> "obdh")

  it "is a library in a local folder when the command runs inside it" $
    withTree $ \root ->
      projectFrom (root </> "obdh" </> "libs" </> "drivers" </> "src" </> "drivers") `shouldReturn` (root </> "obdh" </> "libs" </> "drivers")

  it "is the project that holds a package of vendor/ when the command runs inside the package" $
    withTree $ \root ->
      projectFrom (root </> "obdh" </> "vendor" </> "github.com" </> "acme" </> "ccsds" </> "src" </> "ccsds") `shouldReturn` (root </> "obdh")
