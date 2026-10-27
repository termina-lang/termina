{-# LANGUAGE OverloadedStrings #-}

-- | The transitive resolution of the dependencies of a project: one version
-- of each package for the application and all its tests, with the overrides
-- of the root, and the path that brought in each requirement.
module Configuration.Resolve (
    Obtained(..),
    ResolvedPackage(..),
    AppliedOverride(..),
    Resolution(..),
    ResolveError(..),
    resolve,
    resolveWith,
    describeResolveError
) where

import Data.List (intercalate)
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Configuration.Fetch (FetchedFile)
import Configuration.Lock (normalizeUrl)
import Configuration.Manifest

-- | A package as the function that brings it gives it: the commit of its
-- tag, its validated manifest and its files.
data Obtained = Obtained {
    obtainedSha :: !T.Text,
    obtainedManifest :: !Manifest,
    obtainedFiles :: ![FetchedFile]
  } deriving (Eq, Show)

-- | A package of the resolution: the URL as the first manifest that names it
-- writes it, the tag, what was obtained, and the path that first required it.
data ResolvedPackage = ResolvedPackage {
    resolvedUrl :: !T.Text,
    resolvedTag :: !T.Text,
    resolvedObtained :: !Obtained,
    resolvedPath :: ![String]
  } deriving (Eq, Show)

-- | An override of the root that changed the tag of a requirement: the
-- normalized URL, the forced tag, the requested one and who requested it.
data AppliedOverride = AppliedOverride {
    overriddenUrl :: !T.Text,
    forcedTag :: !T.Text,
    requestedTag :: !T.Text,
    requestedBy :: ![String]
  } deriving (Eq, Show)

data Resolution = Resolution {
    -- | The packages, by normalized URL
    resolvedPackages :: !(M.Map T.Text ResolvedPackage),
    appliedOverrides :: ![AppliedOverride],
    -- | The overrides in the manifest of a dependency, which do not count:
    -- the name of the package and the URL of each override.
    ignoredOverrides :: ![(T.Text, T.Text)]
  } deriving (Eq, Show)

data ResolveError e =
    -- | Two tags for one normalized URL, each with the path that required it.
    VersionConflict !T.Text (T.Text, [String]) (T.Text, [String])
    -- | A package that could not be brought: its URL, tag, path and error.
    | ObtainFailed !T.Text !T.Text ![String] e
  deriving (Eq, Show)

data Requirement = Requirement !T.Text !T.Text ![String]

data State e = State {
    -- | The tag and the path of each normalized URL already met
    seen :: !(M.Map T.Text (T.Text, [String])),
    packages :: !(M.Map T.Text ResolvedPackage),
    applied :: ![AppliedOverride],
    ignored :: ![(T.Text, T.Text)],
    errors :: ![ResolveError e]
  }

-- | Resolve the dependencies of the root and of its tests, breadth first, so
-- that the URL of a package is the one written closest to the root. A package
-- already met with the same tag is not brought again, which also ends the
-- cycles; every error is collected before giving up.
resolve :: Monad m => (T.Text -> T.Text -> m (Either e Obtained)) -> Manifest -> m (Either [ResolveError e] Resolution)
resolve obtain root = resolveWith obtain root []

-- | Resolve as "resolve" does, with more requirements after those of the
-- root and its tests, each with the path that brings it: the dependencies
-- of the libraries that the project takes from local folders.
resolveWith :: Monad m => (T.Text -> T.Text -> m (Either e Obtained)) -> Manifest -> [(GitDependency, [String])]
    -> m (Either [ResolveError e] Resolution)
resolveWith obtain root extra = do
    final <- go (initial ++ [Requirement (gitUrl g) (gitRev g) path | (g, path) <- extra]) (State M.empty M.empty [] [] [])
    return $ case reverse (errors final) of
        [] -> Right (Resolution (packages final) (reverse (applied final)) (reverse (ignored final)))
        errs -> Left errs

  where

    rootName = T.unpack (projectName (manifestProject root))

    overrides = M.fromList [(normalizeUrl (gitUrl o), gitRev o) | o <- manifestOverrides root]

    initial = [Requirement (gitUrl g) (gitRev g) [rootName] | DepGit g <- manifestDependencies root]
        ++ [ Requirement (gitUrl g) (gitRev g) [rootName ++ " (test " ++ T.unpack t ++ ")"]
           | (t, entry) <- M.toList (manifestTests root), DepGit g <- testDependencies entry ]

    go [] st = return st
    go (Requirement url requested path : rest) st0 = do
        let key = normalizeUrl url
            tag = M.findWithDefault requested key overrides
            st = st0 { applied = [AppliedOverride key tag requested path | tag /= requested] ++ applied st0 }
        case M.lookup key (seen st) of
            Just (other, otherPath)
                | other == tag -> go rest st
                | otherwise -> go rest st { errors = VersionConflict key (other, otherPath) (tag, path) : errors st }
            Nothing -> do
                result <- obtain url tag
                let st' = st { seen = M.insert key (tag, path) (seen st) }
                case result of
                    Left err -> go rest st' { errors = ObtainFailed url tag path err : errors st' }
                    Right got -> do
                        let manifest = obtainedManifest got
                            name = projectName (manifestProject manifest)
                            deps = [Requirement (gitUrl g) (gitRev g) (path ++ [T.unpack name]) | DepGit g <- manifestDependencies manifest]
                        go (rest ++ deps) st' {
                            packages = M.insert key (ResolvedPackage url tag got path) (packages st'),
                            ignored = reverse [(name, gitUrl o) | o <- manifestOverrides manifest] ++ ignored st' }

-- | The message of an error, with the path of each requirement ending in the
-- package it requires.
describeResolveError :: (e -> String) -> ResolveError e -> String
describeResolveError _ (VersionConflict key (tag1, path1) (tag2, path2)) =
    "version conflict on " ++ T.unpack key ++ "\n"
        ++ "  " ++ T.unpack tag1 ++ "  " ++ chain path1 ++ "\n"
        ++ "  " ++ T.unpack tag2 ++ "  " ++ chain path2 ++ "\n"
        ++ "  write an override in termina.yaml to force one version for the whole project"

  where

    chain path = intercalate " -> " (path ++ [T.unpack (last (T.splitOn "/" key))])

describeResolveError describe (ObtainFailed url tag path err) =
    T.unpack url ++ " " ++ T.unpack tag ++ " (required by " ++ intercalate " -> " path ++ "): " ++ describe err
