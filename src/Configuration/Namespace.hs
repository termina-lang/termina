-- | The namespaces of a project: the prefixes that a library exports.
module Configuration.Namespace (
    checkExports,
    prefixText
) where

import Control.Monad
import Data.List (isPrefixOf, nub, tails, (\\))
import qualified Data.Text as T
import System.Directory
import System.FilePath

import Configuration.Manifest

-- | The prefix as it is written in the manifest, such as "drivers.gaisler".
prefixText :: Prefix -> String
prefixText (Prefix segments) = T.unpack (T.intercalate (T.pack ".") segments)

-- | Check the prefixes of the exports of a library against each other and
-- against its sources, and collect every problem so that all of them are
-- reported at once. An application exports nothing and has nothing to check.
checkExports :: FilePath -> Manifest -> IO [String]
checkExports root manifest =
    case manifestNature manifest of
        Application _ -> return []
        Library [] -> return ["a library exports at least one prefix"]
        Library prefixes -> do
            emptyErrors <- concat <$> mapM checkHasModule (nub prefixes)
            return (repeatedErrors prefixes ++ overlapErrors (nub prefixes) ++ emptyErrors)

  where

    repeatedErrors prefixes =
        ["the prefix " ++ prefixText p ++ " is listed twice in exports" | p <- nub (prefixes \\ nub prefixes)]

    overlapErrors prefixes =
        [ "the prefixes " ++ prefixText outer ++ " and " ++ prefixText inner ++ " of exports overlap: "
            ++ prefixText outer ++ " contains " ++ prefixText inner
        | (a : rest) <- tails prefixes, b <- rest, (outer, inner) <- [(a, b), (b, a)], contains outer inner ]

    contains (Prefix outer) (Prefix inner) = outer `isPrefixOf` inner && outer /= inner

    -- | A prefix owns the module that it names and everything under the
    -- folder of the same name.
    checkHasModule p@(Prefix segments) = do
        let path = "src" </> joinPath (map T.unpack segments)
        isModule <- doesFileExist (root </> path <.> "fin")
        hasModules <- if isModule then return True else holdsModule (root </> path)
        return ["the prefix " ++ prefixText p ++ " of exports has no module: there is no "
            ++ path <.> "fin" ++ " and no .fin under " ++ path ++ "/" | not hasModules]

    holdsModule dir = do
        isDir <- doesDirectoryExist dir
        if not isDir then return False else do
            entries <- filter (not . ("." `isPrefixOf`)) <$> listDirectory dir
            let fins = [e | e <- entries, takeExtension e == ".fin"]
            if not (null fins) then return True else
                anyM (\e -> holdsModule (dir </> e)) entries

    anyM f = foldM (\found x -> if found then return True else f x) False
