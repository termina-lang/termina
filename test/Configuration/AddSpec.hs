{-# LANGUAGE OverloadedStrings #-}

-- | "termina add" on a project in a fresh folder, with its packages in local
-- repositories that git reaches for https://example.invalid/ through an
-- insteadOf rule in the environment.
module Configuration.AddSpec (spec) where

import Command.Add
import Command.Dependencies (Fetching(..), Sync(..), credentialsMessage, describeChange, syncDependencies)
import Command.Restore (restoreVendor)
import Command.Update (checkTags, updateDependencies, verifyProject)
import Command.Utils (Place(..), loadLock)
import Configuration.Lock
import Configuration.Manifest (decodeManifest)

import Control.Exception (bracket_)
import Control.Monad (forM_, void)
import qualified Data.ByteString.Char8 as BS
import Data.List (intercalate, isInfixOf, isPrefixOf)
import qualified Data.Text as T
import System.Directory
import System.Environment (setEnv, unsetEnv)
import System.FilePath
import System.Process (readProcess)
import Test.Hspec

import Configuration.Helpers

url :: String -> T.Text
url name = T.pack ("https://example.invalid/acme/" ++ name)

-- | A library in a new repository of the folder, with one exported module,
-- one file that is not copied and its dependencies, tagged.
library :: FilePath -> String -> [(String, String)] -> String -> IO ()
library repos name deps tag = do
    let repo = repos </> "acme" </> name
    createDirectoryIfMissing True (repo </> "src" </> name)
    void $ readProcess "git" ["init", "-q", repo] ""
    BS.writeFile (repo </> "termina.yaml") $ BS.pack $ header name ++ "exports: [" ++ name ++ "]\n"
        ++ (if null deps then "" else "dependencies:\n" ++ concat ["  - git: " ++ T.unpack (url d) ++ "\n    rev: \"" ++ t ++ "\"\n" | (d, t) <- deps])
    BS.writeFile (repo </> "src" </> name </> "m.fin") ""
    BS.writeFile (repo </> "README.md") "not copied\n"
    void $ git repo ["add", "."]
    void $ git repo ["commit", "-q", "-m", "first"]
    void $ git repo ["tag", tag]

-- | Run the action on a new project, with the folder of its repositories,
-- while git takes https://example.invalid/ from that folder.
withProject :: String -> String -> (FilePath -> FilePath -> IO a) -> IO a
withProject label tests action = do
    tmp <- getTemporaryDirectory
    let root = tmp </> "termina-add-spec" </> label
        repos = root </> "repos"
        project = root </> "project"
    removePathForcibly root
    createDirectoryIfMissing True repos
    createDirectoryIfMissing True project
    writeFile (project </> "termina.yaml") (header "app" ++ "app:\n  platform: posix-gcc\n  profile: release\n" ++ tests)
    bracket_
        (setEnv "GIT_CONFIG_COUNT" "1" >> setEnv "GIT_CONFIG_KEY_0" ("url.file://" ++ repos ++ "/.insteadOf")
            >> setEnv "GIT_CONFIG_VALUE_0" "https://example.invalid/")
        (mapM_ unsetEnv ["GIT_CONFIG_COUNT", "GIT_CONFIG_KEY_0", "GIT_CONFIG_VALUE_0"])
        (action repos project)

add :: FilePath -> String -> String -> Maybe String -> IO (Either [String] AddReport)
add project name = addDependency Computer project (url name) . T.pack

lockOf :: FilePath -> IO [(T.Text, T.Text)]
lockOf project = either error (map (\p -> (lockedUrl p, lockedRev p)) . lockPackages) <$> decodeLockFile (project </> "termina.lock")

spec :: Spec
spec = do

  describe "termina add" $ do

    it "adds a package with its transitive dependency to the manifest, the lock and vendor" $
      withProject "simple" "" $ \repos project -> do
        library repos "ccsds" [] "v1.0.0"
        library repos "pus" [("ccsds", "v1.0.0")] "v0.1.0"
        fmap reportLines <$> add project "pus" "v0.1.0" Nothing `shouldReturn` Right
          ["added https://example.invalid/acme/pus v0.1.0", "pulled https://example.invalid/acme/ccsds v1.0.0 (app -> pus)"]
        readFile (project </> "termina.yaml") >>= (`shouldSatisfy` isInfixOf "  - git: https://example.invalid/acme/pus\n    rev: \"v0.1.0\"\n")
        lockOf project `shouldReturn` [(url "ccsds", "v1.0.0"), (url "pus", "v0.1.0")]
        let vendored = project </> "vendor" </> "example.invalid" </> "acme" </> "pus"
        mapM (doesFileExist . (vendored </>)) ["termina.yaml", "src/pus/m.fin", "README.md"] `shouldReturn` [True, True, False]

    it "refuses a package that is already a dependency" $
      withProject "present" "" $ \repos project -> do
        library repos "pus" [] "v0.1.0"
        void $ add project "pus" "v0.1.0" Nothing
        add project "pus" "v0.2.0" Nothing `shouldReturn`
          Left ["https://example.invalid/acme/pus is already a dependency of the project: use termina update to change its tag"]

    it "leaves the project as it was on a version conflict" $
      withProject "conflict" "" $ \repos project -> do
        library repos "hal" [] "v1"
        _ <- git (repos </> "acme" </> "hal") ["commit", "-q", "--allow-empty", "-m", "second"]
        _ <- git (repos </> "acme" </> "hal") ["tag", "v2"]
        library repos "drivers" [("hal", "v1")] "v1"
        library repos "boards" [("hal", "v2")] "v1"
        void $ add project "drivers" "v1" Nothing
        original <- mapM (BS.readFile . (project </>)) ["termina.yaml", "termina.lock"]
        result <- add project "boards" "v1" Nothing
        result `shouldSatisfy` either (any ("version conflict on example.invalid/acme/hal" `isInfixOf`)) (const False)
        mapM (BS.readFile . (project </>)) ["termina.yaml", "termina.lock"] `shouldReturn` original
        doesDirectoryExist (project </> "vendor" </> "example.invalid" </> "acme" </> "boards") `shouldReturn` False

    it "refuses a package that is not a library" $
      withProject "invalid" "" $ \repos project -> do
        let repo = repos </> "acme" </> "tool"
        createDirectoryIfMissing True repo
        void $ readProcess "git" ["init", "-q", repo] ""
        writeFile (repo </> "termina.yaml") (header "tool" ++ "app:\n  platform: posix-gcc\n  profile: release\n")
        void $ git repo ["add", "."] >> git repo ["commit", "-q", "-m", "first"] >> git repo ["tag", "v1"]
        add project "tool" "v1" Nothing `shouldReturn` Left
          [ "https://example.invalid/acme/tool v1 (required by app): the package is not valid:\n"
            ++ "  the package is an application: it has no exports, so it cannot be a dependency" ]

    it "adds a package to the dependencies of a test" $
      withProject "test" "tests:\n  hk:\n    platforms: [posix-gcc]\n    timeout: 10\n" $ \repos project -> do
        library repos "fixtures" [] "v0.1.0"
        fmap reportLines <$> add project "fixtures" "v0.1.0" (Just "hk") `shouldReturn`
          Right ["added https://example.invalid/acme/fixtures v0.1.0 to the test hk"]
        readFile (project </> "termina.yaml") >>= (`shouldSatisfy` isInfixOf "    dependencies:\n      - git: https://example.invalid/acme/fixtures\n")
        lockOf project `shouldReturn` [(url "fixtures", "v0.1.0")]

    it "takes a clone of the prefetch folder, and removes it" $
      withProject "prefetch" "" $ \repos project -> do
        library (repos </> "elsewhere") "private" [] "v1"
        let clone = project </> ".termina" </> "fetch" </> "example.invalid" </> "acme" </> "private"
        void $ readProcess "git" ["-c", "advice.detachedHead=false", "clone", "-q", "--branch", "v1", "--depth", "1",
            "file://" ++ repos </> "elsewhere" </> "acme" </> "private", clone] ""
        fmap reportLines <$> add project "private" "v1" Nothing `shouldReturn` Right ["added https://example.invalid/acme/private v1"]
        doesDirectoryExist clone `shouldReturn` False

    it "keeps a locked package with the same tag even if the tag moved" $
      withProject "moved" "" $ \repos project -> do
        library repos "ccsds" [] "v1.0.0"
        library repos "pus" [("ccsds", "v1.0.0")] "v0.1.0"
        library repos "tm" [("ccsds", "v1.0.0")] "v0.1.0"
        void $ add project "pus" "v0.1.0" Nothing
        locked <- BS.readFile (project </> "termina.lock")
        let ccsds = repos </> "acme" </> "ccsds"
        _ <- git ccsds ["commit", "-q", "--allow-empty", "-m", "second"]
        _ <- git ccsds ["tag", "-f", "v1.0.0"]
        fmap reportLines <$> add project "tm" "v0.1.0" Nothing `shouldReturn` Right ["added https://example.invalid/acme/tm v0.1.0"]
        commits <- filter ("resolved-rev" `isInfixOf`) . lines <$> readFile (project </> "termina.lock")
        forM_ (filter ("resolved-rev" `isInfixOf`) (lines (BS.unpack locked))) (\l -> commits `shouldContain` [l])

  describe "reconciliation of the lock with the manifest" $ do

    let sync project = do
          manifest <- either error id . decodeManifest <$> BS.readFile (project </> "termina.yaml")
          lock <- either (error . unlines) id <$> loadLock project
          fmap (map describeChange . syncChanges) <$> syncDependencies Computer "termina build" Reuse True project manifest lock
        edit project from to = do
          text <- readFile (project </> "termina.yaml")
          length text `seq` writeFile (project </> "termina.yaml") (replace from to text)
        replace from to s@(c : cs)
          | from `isPrefixOf` s = to ++ replace from to (drop (length from) s)
          | otherwise = c : replace from to cs
        replace _ _ [] = []

    it "changes nothing when the manifest and the lock agree" $
      withProject "sync-same" "" $ \repos project -> do
        library repos "pus" [] "v0.1.0"
        void $ add project "pus" "v0.1.0" Nothing
        sync project `shouldReturn` Right []

    it "follows a rev changed by hand in the manifest" $
      withProject "sync-rev" "" $ \repos project -> do
        library repos "pus" [] "v0.1.0"
        let pus = repos </> "acme" </> "pus"
        void $ add project "pus" "v0.1.0" Nothing
        BS.writeFile (pus </> "src" </> "pus" </> "m.fin") "second\n"
        void $ git pus ["commit", "-q", "-a", "-m", "second"]
        void $ git pus ["tag", "v0.2.0"]
        edit project "\"v0.1.0\"" "\"v0.2.0\""
        sync project `shouldReturn` Right ["https://example.invalid/acme/pus v0.1.0 -> v0.2.0"]
        lockOf project `shouldReturn` [(url "pus", "v0.2.0")]
        BS.readFile (project </> "vendor" </> "example.invalid" </> "acme" </> "pus" </> "src" </> "pus" </> "m.fin")
          `shouldReturn` "second\n"

    it "adds a dependency written by hand, and removes one taken out of the manifest" $
      withProject "sync-add-remove" "" $ \repos project -> do
        library repos "pus" [] "v0.1.0"
        library repos "tm" [] "v0.1.0"
        void $ add project "pus" "v0.1.0" Nothing
        edit project "acme/pus" "acme/tm"
        sync project `shouldReturn` Right ["added https://example.invalid/acme/tm v0.1.0", "removed https://example.invalid/acme/pus v0.1.0"]
        lockOf project `shouldReturn` [(url "tm", "v0.1.0")]
        doesDirectoryExist (project </> "vendor" </> "example.invalid" </> "acme" </> "pus") `shouldReturn` False
        edit project "dependencies:\n  - git: https://example.invalid/acme/tm\n    rev: \"v0.1.0\"\n" ""
        sync project `shouldReturn` Right ["removed https://example.invalid/acme/tm v0.1.0"]
        doesFileExist (project </> "termina.lock") `shouldReturn` False
        doesDirectoryExist (project </> "vendor" </> "example.invalid") `shouldReturn` False

  describe "termina update and termina verify" $ do

    let moveTag repos name = do
          let repo = repos </> "acme" </> name
          BS.writeFile (repo </> "src" </> name </> "m.fin") "moved\n"
          void $ git repo ["commit", "-q", "-a", "-m", "moved"]
          void $ git repo ["tag", "-f", "v0.1.0"]
        update project = updateDependencies Computer project
        edit project from to = do
          text <- readFile (project </> "termina.yaml")
          length text `seq` writeFile (project </> "termina.yaml") (replace from to text)
        replace from to s@(c : cs)
          | from `isPrefixOf` s = to ++ replace from to (drop (length from) s)
          | otherwise = c : replace from to cs
        replace _ _ [] = []

    it "adopts a tag that moved, of every dependency or of the one named" $
      withProject "update" "" $ \repos project -> do
        library repos "pus" [] "v0.1.0"
        library repos "tm" [] "v0.1.0"
        void $ add project "pus" "v0.1.0" Nothing
        void $ add project "tm" "v0.1.0" Nothing
        update project Nothing `shouldReturn` Right ([], ["termina.lock is up to date"])
        moveTag repos "pus"
        moveTag repos "tm"
        fmap (map (takeWhile (/= ' ')) . snd) <$> update project (Just "pus") `shouldReturn` Right ["termina.lock:"]
        BS.readFile (project </> "vendor" </> "example.invalid" </> "acme" </> "pus" </> "src" </> "pus" </> "m.fin") `shouldReturn` "moved\n"
        BS.readFile (project </> "vendor" </> "example.invalid" </> "acme" </> "tm" </> "src" </> "tm" </> "m.fin") `shouldReturn` ""
        update project (Just "hal") `shouldReturn` Left ["hal is not a dependency of this project."]

    it "reports what update would change, without writing" $
      withProject "update-check" "" $ \repos project -> do
        library repos "pus" [] "v0.1.0"
        void $ add project "pus" "v0.1.0" Nothing
        checkTags project `shouldReturn` Right []
        moveTag repos "pus"
        edit project "\"v0.1.0\"" "\"v0.2.0\""
        result <- checkTags project
        case result of
          Right [manifestDrift, tagDrift] -> do
            manifestDrift `shouldBe` "termina.yaml asks for v0.2.0 of https://example.invalid/acme/pus, and termina.lock has v0.1.0"
            tagDrift `shouldSatisfy` isPrefixOf "the tag v0.1.0 of https://example.invalid/acme/pus now points to "
          other -> expectationFailure (show other)

    it "checks without network that vendor/ and the lock match the manifest" $
      withProject "verify" "" $ \repos project -> do
        library repos "pus" [] "v0.1.0"
        void $ add project "pus" "v0.1.0" Nothing
        verifyProject Computer project `shouldReturn` Right ()
        edit project "\"v0.1.0\"" "\"v0.2.0\""
        verifyProject Computer project `shouldReturn` Left
          [ "https://example.invalid/acme/pus v0.2.0 (required by app): termina.lock has v0.1.0 instead, and fetching it needs the network. "
            ++ "Run termina build without --offline, or termina update, to update termina.lock." ]
        edit project "dependencies:\n  - git: https://example.invalid/acme/pus\n    rev: \"v0.2.0\"\n" ""
        verifyProject Computer project `shouldReturn` Left
          [ "termina.lock has https://example.invalid/acme/pus v0.1.0, which termina.yaml no longer needs. "
            ++ "Run termina build or termina update to update termina.lock." ]
        edit project "profile: release\n" "profile: release\ndependencies:\n  - path: ../drivers\n"
        verifyProject Computer project `shouldReturn` Left ["termina.yaml takes ../drivers from a local folder, which termina.lock cannot pin."]

  describe "termina restore" $ do

    let pusFolder project = project </> "vendor" </> "example.invalid" </> "acme" </> "pus"
        added label action = withProject label "" $ \repos project -> do
          library repos "pus" [] "v0.1.0"
          void $ add project "pus" "v0.1.0" Nothing
          action repos project

    it "restores nothing when vendor/ matches the lock" $
      added "restore-nothing" $ \_ project ->
        restoreVendor Computer project `shouldReturn` Right []

    it "brings back a modified copy and a missing one" $
      added "restore-broken" $ \repos project -> do
        library repos "tm" [] "v0.1.0"
        void $ add project "tm" "v0.1.0" Nothing
        BS.writeFile (pusFolder project </> "src" </> "pus" </> "m.fin") "changed\n"
        removePathForcibly (project </> "vendor" </> "example.invalid" </> "acme" </> "tm")
        restoreVendor Computer project `shouldReturn`
          Right ["restored vendor/example.invalid/acme/pus (v0.1.0)", "restored vendor/example.invalid/acme/tm (v0.1.0)"]
        BS.readFile (pusFolder project </> "src" </> "pus" </> "m.fin") `shouldReturn` ""
        restoreVendor Computer project `shouldReturn` Right []

    it "brings back the locked commit by its SHA when the tag has moved" $
      added "restore-moved" $ \repos project -> do
        let pus = repos </> "acme" </> "pus"
        BS.writeFile (pus </> "src" </> "pus" </> "m.fin") "second\n"
        void $ git pus ["commit", "-q", "-a", "-m", "second"]
        void $ git pus ["tag", "-f", "v0.1.0"]
        removePathForcibly (pusFolder project)
        restoreVendor Computer project `shouldReturn` Right ["restored vendor/example.invalid/acme/pus (v0.1.0)"]
        BS.readFile (pusFolder project </> "src" </> "pus" </> "m.fin") `shouldReturn` ""

  describe "messages that depend on where termina runs" $ do

    it "says where to set the rule for SSH only in a container" $
      withProject "ssh" "" $ \_ project -> do
        let ssh place = either (map (last . lines)) (const []) <$> addDependency place project "git@github.com:uah/drivers" "v1" Nothing
        ssh Computer `shouldReturn` ["git will then use SSH for every https://github.com/ address."]
        ssh VSCodeContainer `shouldReturn` ["When it is rebuilt, this container loses its git configuration."]
        ssh CommandLineContainer `shouldReturn`
          ["This container has no SSH keys, so it cannot clone over SSH. Run termina add on your computer or in VS Code."]

    it "gives the clone to run on the host for every package without credentials" $
      credentialsMessage "termina add" [(url "a", "v1"), (url "b", "v2")] `shouldBe` intercalate "\n"
        [ "git has no credentials in this container for:"
        , "  https://example.invalid/acme/a v1"
        , "  https://example.invalid/acme/b v2"
        , "To bring them, do one of these:"
        , "  1. On your computer, from the project folder, run:"
        , "       git clone --branch v1 --depth 1 https://example.invalid/acme/a .termina/fetch/example.invalid/acme/a"
        , "       git clone --branch v2 --depth 1 https://example.invalid/acme/b .termina/fetch/example.invalid/acme/b"
        , "     and then run termina add again here."
        , "  2. Run termina add with termina installed on your computer."
        , "  3. Open the project in VS Code, which passes your credentials to its container."
        , "GitHub also asks for credentials when a repository does not exist, so if it is public, check its URL." ]
