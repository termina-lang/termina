-- | The SHA-256 of bytes and of files, in hexadecimal.
module Utils.Hash (
    sha256Hex,
    sha256File
) where

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Text.Printf (printf)

-- | The SHA-256 of the bytes, in lowercase hexadecimal
sha256Hex :: BL.ByteString -> String
sha256Hex = concatMap (printf "%02x") . BS.unpack . SHA256.hashlazy

-- | The SHA-256 of the content of a file, in lowercase hexadecimal
sha256File :: FilePath -> IO String
sha256File file = sha256Hex <$> BL.readFile file
