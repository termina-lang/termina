{-# LANGUAGE OverloadedStrings #-}

-- | The deliverable of the application, and of every test with --tests: the
-- plain C of the deployment with the files of the OSAL that its build uses and
-- a flat Makefile, which compiles with nothing but the toolchain of the
-- platform. It is built and checked against the binary built from the C with
-- the #line pragmas.
module Command.Deliver (
    deliverCmdArgsParser, deliverCommand, DeliverCmdArgs,
    dependencies, inFolder, pathsIn, relocatePaths, toolchainVariables, releaseBuild, genDoxyfile,
    genDoxygenGroups, naturalKey
) where

import Command.Build
import Command.Extern (ExternGroup(..), groupObject, groupObjects)
import Command.Project (Dependencies(..), loadProject)
import Command.Provenance (buildInfo, provenanceFields, readHashes, writeProvenanceFile)
import Command.Sources (LoadedProject(..))
import Command.Utils
import Configuration.Configuration
import Configuration.Manifest
import Configuration.NamespaceFile (readNamespaceDescription)
import Configuration.Package (Package(..))
import Configuration.Vendor (isLicenseFile)
import Generator.Makefile.AST
import Generator.CodeGen.Doc (moduleNamespace, namespaceGroup)
import Generator.Makefile.Printer
import Utils.Annotations (QualifiedName)
import Utils.Files (filesUnder)
import Utils.Hash (sha256File)

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Codec.Compression.GZip as GZip
import Control.Monad
import Data.Aeson (object, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlphaNum, isDigit, isSpace)
import Data.List (intercalate, isPrefixOf, isSuffixOf, sortOn, nub, sort)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Options.Applicative as O
import System.Directory
import System.Exit (ExitCode(..))
import System.FilePath
import System.Process (proc, readCreateProcessWithExitCode, readProcessWithExitCode, CreateProcess(cwd))

-- | Data type for the "deliver" command arguments
data DeliverCmdArgs =
    DeliverCmdArgs
        Bool -- ^ Verbose mode
        Bool -- ^ Deliver the tests as well
        Bool -- ^ Pack each deliverable into a .tar.gz
        Bool -- ^ Print the documentation comments in the C and document it with Doxygen
        Bool -- ^ Draw the graphs of the documentation
    deriving Show

-- | Parser for the "deliver" command arguments
deliverCmdArgsParser :: O.Parser DeliverCmdArgs
deliverCmdArgsParser = DeliverCmdArgs
    <$> O.switch (O.long "verbose"
        <> O.short 'v'
        <> O.help "Enable verbose mode")
    <*> O.switch (O.long "tests"
        <> O.help "Deliver every test as well, on every platform of its list")
    <*> O.switch (O.long "archive"
        <> O.help "Pack each deliverable into a .tar.gz")
    <*> O.switch (O.long "docs"
        <> O.help "Print the documentation comments in the C, and document it with Doxygen in doc/html")
    <*> O.switch (O.long "graphs"
        <> O.help "With --docs, draw the graphs of the includes, the structures and the groups, which need dot of Graphviz")

-- | Command handler for the "deliver" command
deliverCommand :: DeliverCmdArgs -> IO ()
deliverCommand (DeliverCmdArgs chatty withTests archive docs graphs) = do
    when (graphs && not docs) $
      dieWithError "--graphs is for the documentation of --docs"
    -- | The tools are checked before the first build, which takes long
    tools <- if docs then Just <$> documentationTools graphs else return Nothing
    -- A deliverable is built from the state in git, with termina.lock and
    -- vendor/ as they are.
    loaded <- loadProject Frozen
    let manifest = loadedManifest loaded
        hasApp = isJust (appDeployment manifest)
        selection = case (withTests, hasApp) of
            (False, _) -> SelectApp
            (True, True) -> SelectAll
            (True, False) -> SelectTests
    unless (hasApp || withTests) $
      dieWithError $ "the project is a library and has no application: deliver its tests with --tests"
    case manifestNature manifest of
        Library _ -> checkExportedModules chatty loaded
        Application _ -> return ()
    deployments <- either dieWithError return $ selectDeployments manifest selection Nothing Nothing
    when (null deployments) (putStrLn . infoMessage $ nothingSelected manifest selection Nothing)
    forM_ deployments $ \deployment -> do
      announce deployments ("Delivering " ++ deploymentLabel deployment)
      deliverDeployment chatty loaded archive tools (deliverableFolder (deploymentConfig deployment))
        (documentedBuild docs (releaseBuild deployment))

-- | The tools that document the deliverables, with their versions: Doxygen,
-- and dot of Graphviz when the documentation has graphs
data DocTools = DocTools {
    doxygenVersion :: String,
    graphvizVersion :: Maybe String
  }

-- | The version of Doxygen of the container image
expectedDoxygen :: String
expectedDoxygen = "1.18.0"

-- | The tools of --docs in the PATH, and their versions
documentationTools :: Bool -> IO DocTools
documentationTools graphs = do
    doxygen <- findExecutable "doxygen"
    when (isNothing doxygen) $
      dieWithError "--docs documents the C with doxygen, which is not in the PATH"
    (_, out, _) <- readProcessWithExitCode "doxygen" ["--version"] ""
    let version = takeWhile (not . isSpace) out
    unless (version == expectedDoxygen) $
      putStrLn . warnMessage $ "the documentation is checked with Doxygen " ++ expectedDoxygen
        ++ ", and the one in the PATH is " ++ version
    dot <- if not graphs then return Nothing else do
      found <- findExecutable "dot"
      when (isNothing found) $
        dieWithError "--graphs needs dot of Graphviz, which is not in the PATH: install Graphviz, or leave out --graphs"
      -- | dot -V writes "dot - graphviz version X.Y.Z (date)" to stderr
      (_, _, err) <- readProcessWithExitCode "dot" ["-V"] ""
      return . Just $ case dropWhile (/= "version") (words err) of
        _ : v : _ -> v
        _ -> takeWhile (/= '\n') err
    return (DocTools version dot)

-- | Runs Doxygen in the folder of a deliverable. With QUIET it writes nothing
-- but its warnings, and any of them stops the delivery.
runDoxygen :: String -> FilePath -> IO ()
runDoxygen label folder = do
    (code, out, err) <- readCreateProcessWithExitCode ((proc "doxygen" []) { cwd = Just folder }) ""
    let messages = filter (not . all isSpace) (lines out ++ lines err)
    unless (code == ExitSuccess && null messages) $
      dieWithError $ "Doxygen did not document " ++ label ++ " without warnings:\n" ++ unlines messages

-- | The folder of the deliverable of a deployment: "deliverable/app",
-- "deliverable/tests/<name>/<platform>".
deliverableFolder :: TerminaConfig -> FilePath
deliverableFolder config = outputFolderName </> "deliverable" </> makeRelative outputFolderName (outputFolder config)

-- | The Doxyfile of a deliverable, which documents its C from its root with
-- nothing else: the name and the version of the project, the folders of the
-- C that the deliverable has but the third-party files of the OSAL, and the
-- settings that the generated C needs. Doxygen lists the static
-- functions, where the methods and viewers of the classes go, only with
-- EXTRACT_STATIC. What is not documented is reported by the transpiler, so
-- Doxygen does not warn about it.
genDoxyfile :: ProjectInfo -> TerminaConfig -> [FilePath] -> Bool -> T.Text
genDoxyfile project config folders graphs = T.unlines $
    [ "PROJECT_NAME          = \"" <> projectName project <> testSuffix <> "\""
    , "PROJECT_NUMBER        = " <> projectVersion project
    , "OUTPUT_DIRECTORY      = doc"
    , "INPUT                 = " <> T.unwords (map T.pack ("groups.dox" : nub folders))
    , "RECURSIVE             = YES"
    , "EXCLUDE               = osal/vendor"
    , "FILE_PATTERNS         = *.h *.c"
    , "OPTIMIZE_OUTPUT_FOR_C = YES"
    , "EXTRACT_STATIC        = YES"
    , "MARKDOWN_SUPPORT      = YES"
    , "WARN_IF_UNDOCUMENTED  = NO"
    , "WARN_IF_INCOMPLETE_DOC = NO"
    , "QUIET                = YES"
    , "GENERATE_LATEX        = NO"
    , "HAVE_DOT              = " <> (if graphs then "YES" else "NO")
    ] ++
    -- | The graphs of the includes, of the structures and of the groups, but
    -- not those of the calls nor those of the files that include a header,
    -- which for termina.h has a node for every file
    (if graphs
        then [ "DOT_IMAGE_FORMAT      = svg"
             , "INTERACTIVE_SVG       = YES"
             , "DOT_GRAPH_MAX_NODES   = 100"
             , "INCLUDED_BY_GRAPH     = NO" ]
        else [])

  where

    testSuffix = if isTestBuild config then ", test " <> name config else ""

-- | The Doxygen groups of a deliverable: the project with the namespaces of
-- its modules, the libraries, each package with the namespaces of its
-- modules, and the OSAL with a group for each of its folders. The modules of a
-- package are those whose source is in its folder, and the rest are of the
-- project. Each namespace goes inside the one that contains it, with the given
-- description if it has one, and the groups at each level are in natural
-- order.
genDoxygenGroups :: ProjectInfo -> [(FilePath, ProjectInfo)] -> M.Map [String] T.Text -> [(QualifiedName, FilePath)] -> T.Text
genDoxygenGroups project packages descriptions modules = T.unlines $
    group (namespaceGroup []) ("Application " <> projectName project) Nothing
        (fromMaybe ("Application " <> projectName project <> ", version " <> projectVersion project <> ".") (projectDescription project))
    ++ namespaceGroups Nothing (namespaceGroup [])
    ++ (if null usedPackages then [] else
          group librariesGroup "Libraries" Nothing ""
          ++ concat [ group (packageGroup info) (projectName info <> " " <> projectVersion info) (Just librariesGroup)
                        (fromMaybe ("Library " <> projectName info <> ", version " <> projectVersion info <> ".") (projectDescription info))
                      ++ namespaceGroups (Just folder) (packageGroup info)
                    | (folder, info) <- usedPackages ])
    ++ group osalGroup "OSAL" Nothing "Operating system abstraction layer on which the generated C runs."
    ++ concat [ group (osalGroup ++ "__" ++ f) ("OSAL " <> T.pack f) (Just osalGroup) (T.pack d)
              | (f, d) <- [ ("api", "Interface of the OSAL that the generated C calls.")
                          , ("shared", "Implementation of the OSAL common to every operating system.")
                          , ("os", "Implementation of the OSAL on each operating system.")
                          , ("platform", "Support of each platform: start-up, devices and linking.") ] ]

  where

    librariesGroup = "termina__libraries"
    osalGroup = "termina__osal"

    packageGroup info = "termina__package__" ++ map (\c -> if isAlphaNum c then c else '_') (T.unpack (projectName info))

    -- | The package whose folder holds the source of a module, if any
    packageOf source = listToMaybe [ folder | (folder, _) <- packages, splitDirectories (normalise folder) `isPrefixOf` splitDirectories (normalise source) ]

    usedPackages = sortOn (naturalKey . T.unpack . projectName . snd) [ p | p@(folder, _) <- packages, Just folder `elem` map (packageOf . snd) modules ]

    -- | The namespaces of the modules of a package, or of the project, each
    -- one inside its parent and the first level inside the given group
    namespaceGroups pkg top = concat
        [ group (namespaceGroup ns) (T.pack (dotted ns)) (Just (if length ns == 1 then top else namespaceGroup (init ns)))
            (M.findWithDefault "" ns descriptions)
        | ns <- sortOn (map naturalKey) . nub $
            [ take n ns | (qname, source) <- modules, packageOf source == pkg, let ns = moduleNamespace qname, n <- [1 .. length ns] ] ]

    dotted = intercalate "."

    group label title within brief =
        ["/**", " * @defgroup " <> T.pack label <> " " <> title]
        ++ [ " * @ingroup " <> T.pack p | Just p <- [within] ]
        ++ (if T.null brief then [] else [" *", " * @brief " <> brief])
        ++ [" */"]

-- | The descriptions of the namespaces of the modules, given the folder of the
-- project and the one of the deliverable: the one of the "namespace.yaml" in
-- the folder of the namespace, or else the brief of the module named as the
-- namespace, as its header in the deliverable has it.
namespaceDescriptions :: FilePath -> FilePath -> [(QualifiedName, FilePath)] -> IO (M.Map [String] T.Text)
namespaceDescriptions root deliverable modules = M.fromList . concat <$> mapM describe (M.toList folders)

  where

    -- | Each namespace with its folder from the project, from the source of a
    -- module in it
    folders = M.fromList
        [ (take n ns, joinPath (take (length parts - (length ns - n)) parts))
        | (qname, source) <- modules, let ns = moduleNamespace qname
        , let parts = splitDirectories (takeDirectory source), n <- [1 .. length ns] ]

    describe (ns, dir) = do
        own <- readNamespaceDescription (root </> dir)
        case own of
            Left err -> dieWithError err
            Right (Just d) -> return [(ns, d)]
            Right Nothing -> maybe [] (\b -> [(ns, b)]) <$> headerBrief (deliverable </> "include" </> joinPath ns <.> "h")

-- | The brief of the comment of the file of a header written by the
-- generator, the comment that the header starts with, where the brief goes on
-- a line that starts with "@brief" and on the indented lines below. A missing
-- header has none.
headerBrief :: FilePath -> IO (Maybe T.Text)
headerBrief path = do
    exists <- doesFileExist path
    if not exists then return Nothing else do
        ls <- T.lines <$> TIO.readFile path
        let fileComment = takeWhile (/= " */") ls
        return $ case break (" * @brief " `T.isPrefixOf`) fileComment of
            (_, first : rest) ->
                Just (T.unwords (T.drop 10 first : map (T.strip . T.drop 2) (takeWhile continues rest)))
            _ -> Nothing

  where

    continues l = " *  " `T.isPrefixOf` l && not (" * @" `T.isPrefixOf` l)

-- | The modules of a deliverable whose header has no brief, because the module
-- has no documentation comment, given the folder of the deliverable.
undocumentedModules :: FilePath -> [(QualifiedName, FilePath)] -> IO [QualifiedName]
undocumentedModules deliverable modules = filterM undocumented (nub (map fst modules))

  where

    undocumented qname = do
        let header = deliverable </> "include" </> qname <.> "h"
        exists <- doesFileExist header
        if exists then (== Nothing) <$> headerBrief header else return False

-- | A key that sorts names with numbers in natural order: service3 before
-- service12.
naturalKey :: String -> [Either Integer String]
naturalKey [] = []
naturalKey s@(c : _)
    | isDigit c = let (d, rest) = span isDigit s in Left (read d) : naturalKey rest
    | otherwise = let (t, rest) = break isDigit s in Right t : naturalKey rest

-- | A deployment whose plain C, the one delivered, carries the documentation
-- comments or not. The C with the #line pragmas, against whose binary the
-- deliverable is checked, never carries them.
documentedBuild :: Bool -> Deployment -> Deployment
documentedBuild docs deployment = deployment {
    deploymentPlainConfig = (deploymentPlainConfig deployment) { docComments = docs }
  }

-- | A deployment built in release under ".termina/deliver", whatever the
-- profile of the manifest, so that delivering leaves the build of the
-- project in "output" as it is.
releaseBuild :: Deployment -> Deployment
releaseBuild deployment = deployment {
    deploymentConfig = moved (deploymentConfig deployment),
    deploymentPlainConfig = moved (deploymentPlainConfig deployment)
  }

  where

    moved config = config {
        profile = Release,
        outputFolder = ".termina" </> "deliver" </> makeRelative outputFolderName (outputFolder config)
      }

-- | Build a deployment and its deliverable in the given folder, and compare
-- their binaries.
deliverDeployment :: Bool -> LoadedProject -> Bool -> Maybe DocTools -> FilePath -> Deployment -> IO ()
deliverDeployment chatty loaded archive tools folder deployment = do
    let config = deploymentConfig deployment
        label = deploymentLabel deployment
        built = outputFolder config
        plain = outputFolder (deploymentPlainConfig deployment)
        binary = "bin" </> T.unpack (name config)
    -- | The build with the #line pragmas gives the values of the Makefile,
    -- the dependency files and the binary to compare with
    (modules, externs) <- buildDeployment (BuildCmdArgs chatty False Nothing SelectApp Nothing Nothing True NoCoverage) loaded deployment
    info <- buildInfo label built
    let value key = M.findWithDefault "" key info
        list key = words (value key)
        osalDir = dropTrailingPathSeparator (value "TERMINA_OSAL_DIR")
        outDir = dropTrailingPathSeparator (value "R")
        relocated = relocatePaths (deliverableSubstitutions config info)
    -- | The values of make are lists separated by spaces, and the paths in
    -- them are rewritten by their folder, so both folders must be absolute
    -- and have no spaces
    forM_ [("TERMINA_OSAL_DIR", osalDir), ("the output folder", outDir)] $ \(what, dir) ->
      unless (isAbsolute dir && not (any isSpace dir)) $
        dieWithError $ what ++ " must be an absolute path without spaces to deliver " ++ label ++ ": \"" ++ dir ++ "\""
    forM_ (toolchainVariables info) $ \var ->
      unless (M.member ("VAR " ++ var) info) $
        dieWithError $ "make gave no value of " ++ var ++ ", which CROSS names, for " ++ label
    let fromOutput paths = forM paths $ \p ->
          if inFolder outDir p
            then return (relativeTo outDir p)
            else dieWithError $ "the build of " ++ label ++ " names a file outside its output folder: " ++ p
    exists <- doesDirectoryExist folder
    when exists (removeDirectoryRecursive folder)
    createDirectoryIfMissing True folder
    -- | The plain C of the deployment, which is what the build with the #line
    -- pragmas compiles without them
    sources <- fromOutput (list "SRCS")
    let externSrcs = [ groupFolder g </> src | g <- externs, src <- groupSources g ]
        externLibs = [ groupFolder g </> lib | g <- externs, lib <- groupLibraries g ]
    headers <- map ("include" </>) . filter ((== ".h") . takeExtension) <$> filesUnder (plain </> "include")
    -- | The headers of each group of C, which its sources include
    externHeaders <- concat <$> forM externs (\g ->
        let headerFolder = groupFolder g </> externHeaderFolder
        in map (headerFolder </>) <$> filesUnder (plain </> headerFolder))
    forM_ (sources ++ headers ++ externSrcs ++ externHeaders ++ externLibs) $ \file -> copyInto (plain </> file) (folder </> file)
    -- | The files of the OSAL that the build opens, as the dependency files
    -- of its objects list them, and those named in the flags, such as a
    -- linker script. Each one goes to the same path under "osal", so the
    -- compiler of the deliverable finds it as the one of the build did.
    depFiles <- filterM doesFileExist (list "DEPS")
    opened <- concatMap (dependencies . T.unpack) <$> mapM TIO.readFile depFiles
    named <- filterM doesFileExist (filter (inFolder osalDir) (concatMap pathsIn (list "CFLAGS" ++ list "LDFLAGS")))
    let osalSources = list "OSAL_SRCS"
        externScript = [osalDir </> "tools" </> "extern" </> "link.sh" | not (null externs)]
        osalUsed = nub . sort . filter (inFolder osalDir) $ osalSources ++ opened ++ named ++ externScript
    licenses <- licenseFiles osalDir osalUsed
    forM_ (map (relativeTo osalDir) (osalUsed ++ licenses)) $ \file -> copyInto (osalDir </> file) (folder </> "osal" </> file)
    -- | The licenses of the project, at the root of the deliverable, and those
    -- of its dependencies, whose code the deliverable carries, in
    -- "licenses/<name of the package>"
    projectLicenses <- licensesIn (loadedRoot loaded)
    forM_ projectLicenses $ \file -> copyInto file (folder </> takeFileName file)
    forM_ (M.elems (loadedRemotes loaded) ++ M.elems (loadedLocals loaded)) $ \p -> do
      let packageName = T.unpack (projectName (manifestProject (packageManifest p)))
      packageLicenses <- licensesIn (packageFolder p)
      forM_ packageLicenses $ \file -> copyInto file (folder </> "licenses" </> packageName </> takeFileName file)
    forM_ (projectLinkerScript config info) $ \(script, path) -> copyInto script (folder </> path)
    let makefile = runMakefilePrinter $
            genDeliverableMakefile config info (map relocated osalSources) sources externs (testModules config modules)
    TIO.writeFile (folder </> "Makefile") makefile
    forM_ tools $ \t -> do
      present <- filterM (doesDirectoryExist . (folder </>)) ["include", "src", "osal"]
      TIO.writeFile (folder </> "Doxyfile") $
        genDoxyfile (manifestProject (loadedManifest loaded)) config (present ++ [ groupFolder g | g <- externs ])
          (isJust (graphvizVersion t))
      undocumented <- undocumentedModules folder modules
      forM_ undocumented $ \qname ->
        putStrLn . warnMessage $ "module " ++ intercalate "." (splitDirectories qname) ++ " has no documentation comment, so its file has no description."
      descriptions <- namespaceDescriptions (loadedRoot loaded) folder modules
      TIO.writeFile (folder </> "groups.dox") $
        genDoxygenGroups (manifestProject (loadedManifest loaded))
          [ (packageFolder p, manifestProject (packageManifest p)) | p <- M.elems (loadedRemotes loaded) ++ M.elems (loadedLocals loaded) ]
          descriptions modules
    -- | The files delivered, as they are on disk
    delivered <- filesUnder folder
    -- | Build the deliverable and compare its binary with the one of the C
    -- with the #line pragmas, both without debugging information
    runMake label folder []
    runMake label folder ["-s", "stripped-hashes"]
    builtHashes <- readHashes (built </> "bin" </> "stripped.sha256")
    deliveredHashes <- readHashes (folder </> "bin" </> "stripped.sha256")
    case (M.lookup binary builtHashes, M.lookup binary deliveredHashes) of
        (Just h1, Just h2) ->
            unless (h1 == h2) $
              dieWithError $ "the binary of the deliverable of " ++ label ++ " differs without debugging information from the one built from the C with the #line pragmas ("
                ++ h2 ++ " in " ++ folder ++ ", " ++ h1 ++ " in " ++ built ++ ")"
        _ -> dieWithError $ "the binaries of " ++ label ++ " have no hashes to compare (is " ++ value "OBJCOPY" ++ " installed?)"
    forM_ tools (const (runDoxygen label folder))
    -- | provenance.json of the deliverable, which adds its files to the one
    -- of a build, and the versions of the tools of its documentation, whose
    -- files it does not hash
    fileHashes <- mapM (\f -> (f,) <$> sha256File (folder </> f)) delivered
    fields <- provenanceFields folder config info deliveredHashes modules
    writeProvenanceFile folder (fields
        ++ ["files" .= map (\(f, h) -> object ["path" .= f, "sha256" .= h]) fileHashes]
        ++ concat [ ["doxygen" .= doxygenVersion t, "graphviz" .= graphvizVersion t] | Just t <- [tools] ])
    when archive $ do
      let project = manifestProject (loadedManifest loaded)
          -- | An application has a single platform, and a test one of those of its list
          archiveName = intercalate "-" $
            [T.unpack (projectName project), T.unpack (projectVersion project)]
            ++ (if isTestBuild config then [T.unpack (name config), T.unpack (platform config)] else [])
          archiveFile = outputFolderName </> "deliverable" </> archiveName <.> "tar" <.> "gz"
          mapFile = binary <.> "map"
      hasMap <- doesFileExist (folder </> mapFile)
      docFiles <- if isJust tools then map (("doc" </> "html") </>) <$> filesUnder (folder </> "doc" </> "html") else return []
      let contents = map (\f -> (f, folder </> f)) ("provenance.json" : delivered ++ docFiles)
            ++ [(binary, folder </> binary)]
            ++ [(mapFile, folder </> mapFile) | hasMap]
      writeArchive archiveFile archiveName contents
      when chatty (putStrLn . debugMessage $ "Archive written to \"" ++ archiveFile ++ "\"")

-- | The paths that the values of the build name, and what the flat Makefile
-- writes in their place: the output folder is the folder of the deliverable,
-- the OSAL goes to its "osal" folder, and the folder of the toolchain is
-- written back as the variable from which CROSS takes it. The linker script
-- of the project goes to the "ld" folder.
deliverableSubstitutions :: TerminaConfig -> M.Map String String -> [(String, String)]
deliverableSubstitutions config info =
    sortOn (negate . length . fst) $
        [(dropTrailingPathSeparator (value "R"), ""), (dropTrailingPathSeparator (value "TERMINA_OSAL_DIR"), "osal")]
        ++ [ (v, "$(" ++ var ++ ")") | var <- toolchainVariables info, let v = value ("VAR " ++ var), not (null v) ]
        ++ maybe [] pure (projectLinkerScript config info)

  where

    value key = M.findWithDefault "" key info

-- | The linker script of the project, as the build names it, and its path in
-- the deliverable, under "ld" with its file name.
projectLinkerScript :: TerminaConfig -> M.Map String String -> Maybe (FilePath, FilePath)
projectLinkerScript config info =
    fmap (\path -> (dropTrailingPathSeparator (M.findWithDefault "" "TERMINA_PROJECT_DIR" info) </> path, "ld" </> takeFileName path))
        (normalise <$> linkerScript config)

-- | The variables that CROSS refers to, which hold the folder of the
-- toolchain.
toolchainVariables :: M.Map String String -> [String]
toolchainVariables info = nub (references (M.findWithDefault "" "CROSS" info))

  where

    references ('$' : '(' : rest) =
      let (ref, rest') = break (`elem` [')', ' ', ',']) rest in
      if all (\c -> c == '_' || c `elem` ['A'..'Z'] || c `elem` ['0'..'9']) ref && not (null ref)
        then ref : references rest'
        else references rest
    references (_ : rest) = references rest
    references [] = []

-- | The parts of a word of a value of make: a word is cut after each "=" and
-- ",", as in -Wl,-Map=<file>, and each part may start with an option of one
-- letter, as in -I<folder> or -T<file>.
wordParts :: String -> [String]
wordParts s = case break (`elem` partDelimiters) s of
    (part, delimiter : rest) -> (part ++ [delimiter]) : wordParts rest
    (part, []) -> [part]

partDelimiters :: String
partDelimiters = "=,"

-- | A part without its delimiter, split into its option and its path.
splitPart :: String -> (String, String, String)
splitPart part =
    let (body, delimiter) = break (`elem` partDelimiters) part in
    case body of
        ('-' : c : rest) | c /= '-' -> (['-', c], rest, delimiter)
        _ -> ("", body, delimiter)

-- | The paths that a word names.
pathsIn :: String -> [FilePath]
pathsIn word = [ path | part <- wordParts word, let (_, path, _) = splitPart part, not (null path) ]

-- | Replace the paths of a word. A path is replaced by the longest folder of
-- the substitutions that it is or that it lies in; the output folder becomes
-- the folder of the deliverable itself.
relocatePaths :: [(String, String)] -> String -> String
relocatePaths substitutions = concatMap relocatePart . wordParts

  where

    relocatePart part =
      let (option, path, delimiter) = splitPart part in
      option ++ replacePath path ++ delimiter

    replacePath path =
      case [ (old, new) | (old, new) <- sortOn (negate . length . fst) substitutions, not (null old), old == path || inFolder old path ] of
        [] -> path
        (old, new) : _ ->
          let rest = relativeTo old path in
          if null new
            then (if null rest then "." else rest)
            else (if null rest then new else new </> rest)

-- | The flat Makefile of the deliverable: the toolchain, the flags and the
-- explicit list of sources, resolved for the platform and the profile.
genDeliverableMakefile :: TerminaConfig -> M.Map String String -> [FilePath] -> [FilePath] -> [ExternGroup] -> [QualifiedName] -> Makefile
genDeliverableMakefile config info osalSources sources externs testQNames =
    Makefile $ [
        MakeBlock [
            MVariable MSimple "PROJECT_NAME" [MFragment . T.unpack . name $ config],
            MVariable MSimple "PLATFORM" [MFragment . T.unpack . platform $ config],
            MVariable MSimple "TERMINA_PROFILE" [MFragment (case profile config of { Debug -> "debug"; Release -> "release" })]
        ],
        -- | The folder of the toolchain may be set by the environment or on
        -- the command line of make
        MakeBlock $
            [ MVariable MConditional var [MFragment (value ("VAR " ++ var))] | var <- toolchainVariables info ]
            ++ [ MVariable MRecursive "CROSS" [MFragment (value "CROSS")] ],
        MakeBlock $
            [ MVariable MRecursive tool [MFragment (crossTool (value tool))]
            | tool <- ["CC"] ++ ["AS" | hasAssembly] ++ ["LD", "OBJCOPY", "NM", "OBJDUMP"] ]
            ++ [ MVariable MConditional "TERMINA_SHA256" [MFragment "$(if $(shell command -v sha256sum 2>/dev/null),sha256sum,shasum -a 256)"] ],
        -- | The debugging information names the files relative to the folder
        -- of the deliverable
        MakeBlock $ [
            MVariable MRecursive "CFLAGS" [MFragment (flags "CFLAGS")],
            MVariable MAppend "CFLAGS" [MFragment "-ffile-prefix-map=$(CURDIR)=."],
            MVariable MRecursive "CPPFLAGS" [MFragment (flags "CPPFLAGS")],
            MVariable MRecursive "LDFLAGS" [MFragment (flags "LDFLAGS")]
        ] ++ [MVariable MRecursive "ASMFLAGS" [MFragment (flags "ASMFLAGS")] | hasAssembly],
        MakeBlock $ map (\src -> MVariable MAppend "OSAL_SRCS" [MFragment src]) osalSources,
        MakeBlock $ map (\src -> MVariable MAppend "SRCS" [MFragment src]) sources,
        MakeBlock [
            MVariable MSimple "TARGET_DIR_NAME" [MFragment "bin"],
            MVariable MSimple "OBJ_DIR_NAME" [MFragment "obj"],
            MVariable MSimple "TARGET" [MFragment "$(TARGET_DIR_NAME)/$(PROJECT_NAME)"]
        ]
        ] ++ externVariables ++ [
        MakeBlock $ [
            -- | The objects are linked in the order of the build of the
            -- project, which the comparison of the binaries needs
            MVariable MSimple "OBJS" [MFunction "patsubst" [MFragment "%", MFragment "$(OBJ_DIR_NAME)/%.o", MFunction "basename" [MFragment "$(OSAL_SRCS) $(SRCS)"]]],
            MVariable MSimple "MODULE_OBJS" [MFunction "patsubst" [MFragment "%.c", MFragment "$(OBJ_DIR_NAME)/%.o", MFunction "filter" [MFragment "src/%", MFragment "$(SRCS)"]]]
        ] ++ [MVariable MAppend "OBJS" [MFragment ("$(OBJ_DIR_NAME)" </> groupObject g)] | g <- externs],
        MakeBlock [
            MVariable MSimple "DEPS" [MFunction "patsubst" [MFragment "%.o", MFragment "%.d",
                MFragment (unwords ("$(OBJS)" : [ "$(" ++ groupObjects g ++ ")" | g <- externs ]))]]
        ],
        MakeBlock $ [
            MRule "all" ["$(TARGET)"] [],
            MRule ".DELETE_ON_ERROR" [] []
            ] ++ [
            MRule "$(OBJ_DIR_NAME)/%.o" ["%.s"] [
                MakeCommand False [MFragment "@echo Building $<"],
                MakeCommand False [MFragment "@mkdir -p $(@D)"],
                MakeCommand False [MFragment "@$(AS) $(ASMFLAGS) $(CFLAGS) $(CPPFLAGS) -MMD -MP -c -o $@ $<"]
            ] | hasAssembly ] ++ [
            MRule "$(OBJ_DIR_NAME)/%.o" ["%.c"] [
                MakeCommand False [MFragment "@echo Building $<"],
                MakeCommand False [MFragment "@mkdir -p $(@D)"],
                MakeCommand False [MFragment "@$(CC) $(CFLAGS) $(TERMINA_EXTERN_CPPFLAGS) $(CPPFLAGS) -MMD -MP -c -o $@ $<"]
            ]] ++ testModuleFlags ++ [
            MRule ".PHONY" ["stripped-hashes"] [],
            MRule "stripped-hashes" ["$(TARGET)", "$(MODULE_OBJS)"] [
                MakeCommand False [MFragment "@$(RM) $(TARGET_DIR_NAME)/stripped.sha256"],
                MakeCommand False [MFragment $ unwords [
                    "@if command -v $(OBJCOPY) >/dev/null 2>&1; then",
                    "for o in $(TARGET) $(MODULE_OBJS); do $(OBJCOPY) --strip-debug --remove-section=.termina_test $$o $$o.stripped || exit 1;",
                    "printf '%s %s\\n' $$($(TERMINA_SHA256) < $$o.stripped | cut -c 1-64) $$o; $(RM) $$o.stripped; done > $(TARGET_DIR_NAME)/stripped.sha256;",
                    "else echo $(OBJCOPY) not found: the objects are not hashed; fi"]]
            ],
            MRule "$(TARGET)" ["$(OBJS)"] [
                MakeCommand False [MFragment "@echo Linking $@"],
                MakeCommand False [MFragment "@mkdir -p $(@D)"],
                MakeCommand False [MFragment "@$(CC) $(CFLAGS) $(OBJS) $(LDFLAGS) -o $@"]
            ]
        ] ++ externRule ++ [
            MRule ".PHONY" ["clean"] [],
            MRule "clean" [] [
                MakeCommand False [MFragment "@$(RM) -r $(TARGET_DIR_NAME) $(OBJ_DIR_NAME)"]
            ],
            MInclude False "$(DEPS)"
        ]
    ]

  where

    value key = M.findWithDefault "" key info

    substitutions = deliverableSubstitutions config info

    -- | The flags with the paths of the deliverable. The prefix map of the
    -- project gives way to the one of the deliverable, and the link map,
    -- which the target that printed the values named after itself, goes
    -- next to the binary.
    flags key = unwords
        . map (replaceMap . relocatePaths substitutions)
        . filter (not . ("-ffile-prefix-map=" `isPrefixOf`))
        . words $ value key

    replaceMap tok
        | "build-info.map" `isSuffixOf` tok = take (length tok - length ("build-info.map" :: String)) tok ++ "$@.map"
        | otherwise = tok

    crossTool tool =
        let cross = value "EXPANDED_CROSS" in
        if not (null cross) && cross `isPrefixOf` tool
            then "$(CROSS)" ++ drop (length cross) tool
            else unwords (map (relocatePaths substitutions) (words tool))

    hasAssembly = any ((== ".s") . takeExtension) osalSources

    -- | As in the build, a block for each group of C
    externVariables =
        [ MakeBlock [MVariable MSimple "TERMINA_EXTERN_LIBC" [MFragment (value "TERMINA_EXTERN_LIBC")]] | not (null externs) ]
        ++ [ MakeBlock [MVariable MSimple (groupObjects g)
                [MFragment (unwords [ "$(OBJ_DIR_NAME)" </> groupFolder g </> replaceExtension src "o" | src <- groupSources g ])]]
           | g <- externs ]

    externRule = concat
        [ [ MRule ("$(" ++ groupObjects g ++ ")") ["TERMINA_EXTERN_CPPFLAGS:=-iquote " ++ groupFolder g </> externHeaderFolder] []
          | not (null (groupSources g)) ] ++ [
          MRule ("$(OBJ_DIR_NAME)" </> groupObject g) (("$(" ++ groupObjects g ++ ")") : libraries) [
            MakeCommand False [MFragment "@echo Linking $@"],
            MakeCommand False [MFragment "@mkdir -p $(@D)"],
            MakeCommand False [MFragment $ unwords [
                "@CC='$(CC)' CFLAGS='$(CFLAGS)' LD='$(LD)' OBJCOPY='$(OBJCOPY)' NM='$(NM)' OBJDUMP='$(OBJDUMP)'",
                "LIBC='$(TERMINA_EXTERN_LIBC)'",
                "sh osal/tools/extern/link.sh '$@' '$(" ++ groupObjects g ++ ")' '" ++ unwords libraries ++ "' '" ++ unwords (groupSymbols g) ++ "'"]]
          ] ]
        | g <- externs
        , let libraries = [ groupFolder g </> lib | lib <- groupLibraries g ] ]

    -- | As in the build of the project, the modules of the test itself keep
    -- each call as a call
    testModuleFlags =
        [ MRule ("$(OBJ_DIR_NAME)" </> "src" </> qname <.> "o") ["CFLAGS+=-fno-optimize-sibling-calls"] []
        | qname <- testQNames ]

-- | The modules of a test that are in its own folder.
testModules :: TerminaConfig -> [(QualifiedName, FilePath)] -> [QualifiedName]
testModules config modules =
    [ qname | isTestBuild config, (qname, source) <- modules, take 1 (splitDirectories source) == ["tests"] ]

-- | The files that a dependency file of make lists as prerequisites. Lines
-- are continued with a backslash, spaces and "#" in a path are escaped with
-- one, and "$" is written "$$".
dependencies :: String -> [FilePath]
dependencies = concatMap prerequisites . lines . joinLines

  where

    joinLines ('\\' : '\n' : rest) = ' ' : joinLines rest
    joinLines (c : rest) = c : joinLines rest
    joinLines [] = []

    prerequisites line = case targetEnd line of
        Nothing -> []
        Just rest -> paths rest

    -- | What follows the colon that ends the targets
    targetEnd (':' : rest) | null rest || isSpace (head rest) = Just rest
    targetEnd ('\\' : _ : rest) = targetEnd rest
    targetEnd (_ : rest) = targetEnd rest
    targetEnd [] = Nothing

    paths s = case dropWhile isSpace s of
        [] -> []
        s' -> let (p, rest) = path s' in p : paths rest

    path ('\\' : c : rest) | isSpace c || c == '#' = let (p, r) = path rest in (c : p, r)
    path ('$' : '$' : rest) = let (p, r) = path rest in ('$' : p, r)
    path (c : rest) | isSpace c = ([], rest)
                    | otherwise = let (p, r) = path rest in (c : p, r)
    path [] = ([], [])

-- | Whether a path lies inside a folder, as it is written. The path may
-- climb with "..", as in src/../include/<file>, as long as it does not leave
-- the folder; it is copied with the same components into the deliverable,
-- so its compiler opens it as the one of the build did.
inFolder :: FilePath -> FilePath -> Bool
inFolder folder path =
    addTrailingPathSeparator folder `isPrefixOf` path
    && all (>= 0) (scanl depth 0 (splitDirectories (drop (length (addTrailingPathSeparator folder)) path)))

  where

    depth d ".." = d - 1
    depth d "." = d
    depth d _ = d + 1 :: Int

-- | A path inside a folder, relative to it and as it is written.
relativeTo :: FilePath -> FilePath -> FilePath
relativeTo folder = dropWhile isPathSeparator . drop (length folder)

-- | The licenses that go with the files of the OSAL: the one of the OSAL,
-- and the one of each vendored project from which a file is taken.
licenseFiles :: FilePath -> [FilePath] -> IO [FilePath]
licenseFiles osalDir used = do
    let vendored = nub [ osalDir </> "vendor" </> v | f <- used, ("vendor" : v : _ : _) <- [splitDirectories (relativeTo osalDir f)] ]
    concat <$> mapM licensesIn (osalDir : vendored)

-- | The license files at the root of a folder, such as "LICENSE" or
-- "LICENSE.md".
licensesIn :: FilePath -> IO [FilePath]
licensesIn dir = do
    entries <- listDirectory dir
    filterM doesFileExist [ dir </> e | e <- sort entries, isLicenseFile e ]

copyInto :: FilePath -> FilePath -> IO ()
copyInto from to = do
    createDirectoryIfMissing True (takeDirectory to)
    copyFileWithMetadata from to

-- | Pack the files into a .tar.gz under a folder of the given name. Every
-- entry has the same time and no owner, and is executable when its file is,
-- so the same files give the same archive.
writeArchive :: FilePath -> FilePath -> [(FilePath, FilePath)] -> IO ()
writeArchive archiveFile root contents = do
    entries <- forM contents $ \(path, source) -> do
      tarPath <- either (dieWithError . (("cannot archive " ++ path ++ ": ") ++)) return
        (Tar.toTarPath False (root </> path))
      isExecutable <- executable <$> getPermissions source
      -- | Read whole, since the lazy read keeps one descriptor open per file
      content <- BL.fromStrict <$> BS.readFile source
      return (Tar.fileEntry tarPath content) {
        Tar.entryPermissions = if isExecutable then Tar.executableFilePermissions else Tar.ordinaryFilePermissions
      }
    createDirectoryIfMissing True (takeDirectory archiveFile)
    BL.writeFile archiveFile . GZip.compress . Tar.write $ entries
