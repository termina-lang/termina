{-# LANGUAGE OverloadedStrings #-}

-- | The deliverable of the application, and of every test with --tests: the
-- plain C of the deployment with the files of the OSAL that its build uses and
-- a flat Makefile, which compiles with nothing but the toolchain of the
-- platform. It is built and checked against the binary built from the C with
-- the #line pragmas.
module Command.Deliver (
    deliverCmdArgsParser, deliverCommand, DeliverCmdArgs,
    dependencies, inFolder, pathsIn, relocate, toolchainVariables, deliverableInfo
) where

import Command.Build
import Command.Extern (externFolder)
import Command.Update (verifyProject)
import Command.Utils
import Configuration.Configuration
import Configuration.Manifest
import Generator.Makefile.AST
import Generator.Makefile.Printer
import Utils.Annotations (QualifiedName)

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Codec.Compression.GZip as GZip
import Control.Monad
import Data.Aeson (object, (.=))
import Data.Aeson.Encode.Pretty (Config(..), defConfig, encodePretty', keyOrder)
import qualified Data.ByteString.Lazy as BL
import Data.Char (isSpace)
import Data.List (intercalate, isPrefixOf, isSuffixOf, sortOn, nub, sort)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Version (versionBranch)
import qualified Options.Applicative as O
import Paths_termina (version)
import System.Directory
import System.Exit
import System.FilePath
import System.Process (proc, readCreateProcessWithExitCode)

-- | Data type for the "deliver" command arguments
data DeliverCmdArgs =
    DeliverCmdArgs
        Bool -- ^ Verbose mode
        Bool -- ^ Deliver the tests as well
        Bool -- ^ Pack each deliverable into a .tar.gz
        Bool -- ^ Add to the archive the binary built from the C with the #line pragmas
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
    <*> O.switch (O.long "with-trace"
        <> O.help "Add to the archive the binary built from the C with the #line pragmas")

-- | Command handler for the "deliver" command
deliverCommand :: DeliverCmdArgs -> IO ()
deliverCommand (DeliverCmdArgs chatty withTests archive withTrace) = do
    when (withTrace && not archive) $
      dieWithError $ "--with-trace adds a binary to the archive: use it with --archive"
    -- A deliverable is built from the state in git, which verify checks without writing.
    place <- currentPlace
    verifyProject place "." >>= either dieWithFirstError return
    manifest <- loadProject chatty True
    let hasApp = isJust (appDeployment manifest)
        selection = case (withTests, hasApp) of
            (False, _) -> SelectApp
            (True, True) -> SelectAll
            (True, False) -> SelectTests
    unless (hasApp || withTests) $
      dieWithError $ "the project is a library and has no application: deliver its tests with --tests"
    deployments <- either dieWithError return $ selectDeployments manifest selection Nothing Nothing
    forM_ deployments $ \deployment -> do
      announce deployments ("Delivering " ++ deploymentLabel deployment)
      deliverDeployment chatty manifest archive withTrace deployment

-- | The folder of the deliverable of a deployment: "deliverable/app",
-- "deliverable/tests/<name>/<platform>".
deliverableFolder :: TerminaConfig -> FilePath
deliverableFolder config = outputFolderName </> "deliverable" </> makeRelative outputFolderName (outputFolder config)

deliverDeployment :: Bool -> Manifest -> Bool -> Bool -> Deployment -> IO ()
deliverDeployment chatty manifest archive withTrace deployment = do
    let config = deploymentConfig deployment
        label = deploymentLabel deployment
        built = outputFolder config
        plain = outputFolder (deploymentPlainConfig deployment)
        folder = deliverableFolder config
        binary = "bin" </> T.unpack (name config)
    -- | The build with the #line pragmas gives the values of the Makefile,
    -- the dependency files and the binary to compare with
    modules <- buildDeployment (BuildCmdArgs chatty False Nothing SelectApp Nothing Nothing True) deployment
    info <- deliverableInfo label built
    let value key = M.findWithDefault "" key info
        list key = words (value key)
        osalDir = dropTrailingPathSeparator (value "TERMINA_OSAL_DIR")
        outDir = dropTrailingPathSeparator (value "R")
        relocated = relocate (deliverableSubstitutions info)
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
    externSrcs <- fromOutput (list "EXTERN_SRCS")
    externLibs <- fromOutput (list "EXTERN_LIBS")
    headers <- map (makeRelative plain) . filter ((== ".h") . takeExtension) <$> filesUnder (plain </> "include")
    -- | The headers of the C of the project, which its sources include
    externHeaders <- map (makeRelative plain) <$> filesUnder (plain </> externFolder </> externHeaderFolder)
    forM_ (sources ++ headers ++ externSrcs ++ externHeaders ++ externLibs) $ \file -> copyInto (plain </> file) (folder </> file)
    -- | The files of the OSAL that the build opens, as the dependency files
    -- of its objects list them, and those named in the flags, such as a
    -- linker script. Each one goes to the same path under "osal", so the
    -- compiler of the deliverable finds it as the one of the build did.
    depFiles <- filterM doesFileExist (list "DEPS")
    opened <- concatMap (dependencies . T.unpack) <$> mapM TIO.readFile depFiles
    named <- filterM doesFileExist (filter (inFolder osalDir) (concatMap pathsIn (list "CFLAGS" ++ list "LDFLAGS")))
    let osalSources = list "OSAL_SRCS"
        externScript = [osalDir </> "tools" </> "extern" </> "link.sh" | not (null externSrcs && null externLibs)]
        osalUsed = nub . sort . filter (inFolder osalDir) $ osalSources ++ opened ++ named ++ externScript
    licenses <- licenseFiles osalDir osalUsed
    forM_ (map (relativeTo osalDir) (osalUsed ++ licenses)) $ \file -> copyInto (osalDir </> file) (folder </> "osal" </> file)
    let makefile = runMakefilePrinter $
            genDeliverableMakefile config info (map relocated osalSources) sources
                externSrcs externLibs (testModules config modules)
    TIO.writeFile (folder </> "Makefile") makefile
    -- | The files delivered, as they are on disk
    delivered <- map (makeRelative folder) <$> filesUnder folder
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
    -- | provenance.json of the deliverable
    fileHashes <- mapM (\f -> (f,) <$> sha256File (folder </> f)) delivered
    osalVersion <- readOsalVersion osalDir
    let objectOf qname = "obj" </> "src" </> qname <.> "o"
        moduleEntry (qname, _) = object [
            "module" .= map (\c -> if c == '/' then '.' else c) qname,
            "object" .= objectOf qname,
            "stripped-sha256" .= M.lookup (objectOf qname) deliveredHashes
          ]
        provenance = object [
            "name" .= name config,
            "deployment" .= (if isTestBuild config then "test" else "app" :: String),
            "platform" .= platform config,
            "profile" .= profile config,
            "termina" .= terminaVersion,
            "osal" .= osalVersion,
            "compiler" .= value "CC_VERSION",
            "binary" .= object [
                "path" .= binary,
                "stripped-sha256" .= M.lookup binary deliveredHashes
              ],
            "modules" .= map moduleEntry modules,
            "files" .= map (\(f, h) -> object ["path" .= f, "sha256" .= h]) fileHashes
          ]
    BL.writeFile (folder </> "provenance" <.> "json") $
      encodePretty' defConfig { confCompare = keyOrder ["name", "deployment", "platform", "profile", "termina", "osal", "compiler", "binary", "modules", "files", "path", "module", "object"] } provenance
    when archive $ do
      let project = manifestProject manifest
          archiveName = intercalate "-" $
            [T.unpack (projectName project), T.unpack (projectVersion project)]
            ++ [T.unpack (name config) | isTestBuild config]
            ++ [T.unpack (platform config)]
          archiveFile = outputFolderName </> "deliverable" </> archiveName <.> "tar" <.> "gz"
          mapFile = binary <.> "map"
      hasMap <- doesFileExist (folder </> mapFile)
      let contents = map (\f -> (f, folder </> f)) ("provenance.json" : delivered)
            ++ [(binary, folder </> binary)]
            ++ [(mapFile, folder </> mapFile) | hasMap]
            ++ [("trace" </> T.unpack (name config), built </> binary) | withTrace]
      writeArchive archiveFile archiveName contents
      when chatty (putStrLn . debugMessage $ "Archive written to \"" ++ archiveFile ++ "\"")

-- | The values that the deliverable-info target of the generated Makefile
-- prints, one per line, as "NAME=value", and "VAR NAME=value" for the
-- variables of the OSAL and of the toolchain.
deliverableInfo :: String -> FilePath -> IO (M.Map String String)
deliverableInfo label folder = do
    (code, out, err) <- readCreateProcessWithExitCode (proc "make" ["-s", "--no-print-directory", "-C", folder, "deliverable-info"]) ""
    case code of
        ExitSuccess -> return . M.fromList . map (fmap (drop 1) . break (== '=')) . filter ('=' `elem`) . lines $ out
        ExitFailure _ -> dieWithError $ "make failed to give the values of the build of " ++ label ++ ":\n" ++ err

-- | The paths that the values of the build name, and what the flat Makefile
-- writes in their place: the output folder is the folder of the deliverable,
-- the OSAL goes to its "osal" folder, and the folder of the toolchain is
-- written back as the variable from which CROSS takes it.
deliverableSubstitutions :: M.Map String String -> [(String, String)]
deliverableSubstitutions info =
    sortOn (negate . length . fst) $
        [(dropTrailingPathSeparator (value "R"), ""), (dropTrailingPathSeparator (value "TERMINA_OSAL_DIR"), "osal")]
        ++ [ (v, "$(" ++ var ++ ")") | var <- toolchainVariables info, let v = value ("VAR " ++ var), not (null v) ]

  where

    value key = M.findWithDefault "" key info

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
relocate :: [(String, String)] -> String -> String
relocate substitutions = concatMap relocatePart . wordParts

  where

    relocatePart part =
      let (option, path, delimiter) = splitPart part in
      option ++ relocatePath path ++ delimiter

    relocatePath path =
      case [ (old, new) | (old, new) <- sortOn (negate . length . fst) substitutions, not (null old), old == path || inFolder old path ] of
        [] -> path
        (old, new) : _ ->
          let rest = relativeTo old path in
          if null new
            then (if null rest then "." else rest)
            else (if null rest then new else new </> rest)

-- | The flat Makefile of the deliverable: the toolchain, the flags and the
-- explicit list of sources, resolved for the platform and the profile.
genDeliverableMakefile :: TerminaConfig -> M.Map String String -> [FilePath] -> [FilePath] -> [FilePath] -> [FilePath] -> [QualifiedName] -> Makefile
genDeliverableMakefile config info osalSources sources externSrcs externLibs testQNames =
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
        ] ++ [MVariable MAppend "OBJS" [MFragment "$(EXTERN_OBJ)"] | hasExtern],
        MakeBlock [
            MVariable MSimple "DEPS" [MFunction "patsubst" [MFragment "%.o", MFragment "%.d",
                MFragment (if hasExtern then "$(OBJS) $(EXTERN_OBJS)" else "$(OBJS)")]]
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
                    "for o in $(TARGET) $(MODULE_OBJS); do $(OBJCOPY) --strip-debug $$o $$o.stripped || exit 1;",
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

    substitutions = deliverableSubstitutions info

    -- | The flags with the paths of the deliverable. The prefix map of the
    -- project gives way to the one of the deliverable, and the link map,
    -- which the target that printed the values named after itself, goes
    -- next to the binary.
    flags key = unwords
        . map (replaceMap . relocate substitutions)
        . filter (not . ("-ffile-prefix-map=" `isPrefixOf`))
        . words $ value key

    replaceMap tok
        | "deliverable-info.map" `isSuffixOf` tok = take (length tok - length ("deliverable-info.map" :: String)) tok ++ "$@.map"
        | otherwise = tok

    crossTool tool =
        let cross = value "EXPANDED_CROSS" in
        if not (null cross) && cross `isPrefixOf` tool
            then "$(CROSS)" ++ drop (length cross) tool
            else unwords (map (relocate substitutions) (words tool))

    hasExtern = not (null externSrcs && null externLibs)

    hasAssembly = any ((== ".s") . takeExtension) osalSources

    externVariables =
        [ MakeBlock $
            map (\src -> MVariable MAppend "EXTERN_SRCS" [MFragment src]) externSrcs
            ++ map (\lib -> MVariable MAppend "EXTERN_LIBS" [MFragment lib]) externLibs
            ++ [
                MVariable MSimple "EXTERN_SYMBOLS" [MFragment (value "EXTERN_SYMBOLS")],
                MVariable MSimple "TERMINA_EXTERN_LIBC" [MFragment (value "TERMINA_EXTERN_LIBC")],
                MVariable MSimple "EXTERN_OBJS" [MFunction "patsubst" [MFragment "%.c", MFragment "$(OBJ_DIR_NAME)/%.o", MFragment "$(EXTERN_SRCS)"]],
                MVariable MSimple "EXTERN_OBJ" [MFragment "$(OBJ_DIR_NAME)/$(PROJECT_NAME).ext.o"]
            ]
        | hasExtern ]

    externRule = concat
        [ [ MRule "$(EXTERN_OBJS)" ["TERMINA_EXTERN_CPPFLAGS:=-iquote " ++ externFolder </> externHeaderFolder] [],
          MRule "$(EXTERN_OBJ)" ["$(EXTERN_OBJS)", "$(EXTERN_LIBS)"] [
            MakeCommand False [MFragment "@echo Linking $@"],
            MakeCommand False [MFragment "@mkdir -p $(@D)"],
            MakeCommand False [MFragment $ unwords [
                "@CC='$(CC)' CFLAGS='$(CFLAGS)' LD='$(LD)' OBJCOPY='$(OBJCOPY)' NM='$(NM)' OBJDUMP='$(OBJDUMP)'",
                "LIBC='$(TERMINA_EXTERN_LIBC)'",
                "sh osal/tools/extern/link.sh '$@' '$(EXTERN_OBJS)' '$(EXTERN_LIBS)' '$(EXTERN_SYMBOLS)'"]]
          ] ]
        | hasExtern ]

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

  where

    licensesIn dir = do
      entries <- listDirectory dir
      filterM doesFileExist [ dir </> e | e <- sort entries, "LICENSE" `isPrefixOf` e ]

filesUnder :: FilePath -> IO [FilePath]
filesUnder dir = do
    exists <- doesDirectoryExist dir
    if not exists then return [] else do
      entries <- map (dir </>) . sort <$> listDirectory dir
      concat <$> mapM (\e -> do
        isDir <- doesDirectoryExist e
        if isDir then filesUnder e else return [e]) entries

copyInto :: FilePath -> FilePath -> IO ()
copyInto from to = do
    createDirectoryIfMissing True (takeDirectory to)
    copyFileWithMetadata from to

-- | The hashes that the stripped-hashes target writes, by path.
readHashes :: FilePath -> IO (M.Map FilePath String)
readHashes file = do
    exists <- doesFileExist file
    if not exists then return M.empty else
      M.fromList . concatMap (toPair . words) . lines <$> readFile file

  where

    toPair [hash, path] = [(normalise path, hash)]
    toPair _ = []

readOsalVersion :: FilePath -> IO (Maybe String)
readOsalVersion osalDir = do
    let file = osalDir </> "VERSION"
    exists <- doesFileExist file
    if exists then Just . filter (not . isSpace) <$> readFile file else return Nothing

-- | The package version is PVP (X.Y.Z.0); the public one drops the fourth
-- component.
terminaVersion :: String
terminaVersion = intercalate "." (map show (take 3 (versionBranch version)))

-- | Pack the files into a .tar.gz under a folder of the given name. Every
-- entry has the same time and no owner, and is executable when its file is,
-- so the same files give the same archive.
writeArchive :: FilePath -> FilePath -> [(FilePath, FilePath)] -> IO ()
writeArchive archiveFile root contents = do
    entries <- forM contents $ \(path, source) -> do
      tarPath <- either (dieWithError . (("cannot archive " ++ path ++ ": ") ++)) return
        (Tar.toTarPath False (root </> path))
      isExecutable <- executable <$> getPermissions source
      content <- BL.readFile source
      return (Tar.fileEntry tarPath content) {
        Tar.entryPermissions = if isExecutable then Tar.executableFilePermissions else Tar.ordinaryFilePermissions
      }
    createDirectoryIfMissing True (takeDirectory archiveFile)
    BL.writeFile archiveFile . GZip.compress . Tar.write $ entries
