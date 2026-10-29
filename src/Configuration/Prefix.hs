-- | The prefixes that a library exports, and what each one owns: the module
-- that it names and every module under the folder of the same name.
module Configuration.Prefix (
    manifestPrefixes,
    prefixText,
    prefixSegments,
    prefixFolder,
    ownsModule,
    ownsPath,
    overlaps
) where

import Data.List (intercalate, isPrefixOf)
import qualified Data.Text as T

import Configuration.Manifest

-- | The prefixes of a library, and none of an application.
manifestPrefixes :: Manifest -> [Prefix]
manifestPrefixes manifest = case manifestNature manifest of
    Library prefixes -> prefixes
    Application _ -> []

-- | The prefix as it is written in the manifest, such as "drivers.gaisler".
prefixText :: Prefix -> String
prefixText = intercalate "." . prefixSegments

prefixSegments :: Prefix -> [String]
prefixSegments (Prefix segments) = map T.unpack segments

-- | The folder of the prefix in the package, with "/" separators, such as
-- "src/drivers/gaisler".
prefixFolder :: Prefix -> String
prefixFolder p = intercalate "/" ("src" : prefixSegments p)

-- | Whether the prefix owns a module, given by the segments of its name.
ownsModule :: Prefix -> [String] -> Bool
ownsModule p segments = prefixSegments p `isPrefixOf` segments

-- | Whether a file of the package, by its path with "/" separators, is the
-- module that the prefix names or lies under its folder.
ownsPath :: Prefix -> String -> Bool
ownsPath p path = path == prefixFolder p ++ ".fin" || (prefixFolder p ++ "/") `isPrefixOf` path

-- | Whether one of two prefixes contains the other, or both are the same.
overlaps :: Prefix -> Prefix -> Bool
overlaps a b = ownsModule a (prefixSegments b) || ownsModule b (prefixSegments a)
