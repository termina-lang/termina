{-# LANGUAGE OverloadedStrings #-}

-- | The descriptor of a test, test.json, as the runner reads it.
module Runner.Descriptor (
    Descriptor(..), DescriptorCase(..), DescriptorCheck(..), Symbols(..),
    decodeDescriptor, readDescriptor
) where

import Data.Aeson (FromJSON(..), eitherDecode, withObject, (.:), (.:?), (.!=))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Numeric (readHex)

data Descriptor = Descriptor {
    descriptorVersion :: Integer,
    descriptorStamp :: Integer,
    descriptorName :: String,
    descriptorLevel :: String,
    descriptorPlatform :: String,
    descriptorTimeout :: Maybe Integer,
    descriptorCases :: [DescriptorCase],
    descriptorChecks :: [DescriptorCheck],
    descriptorTasks :: M.Map Integer String,
    descriptorEmitters :: M.Map Integer String,
    descriptorSymbols :: Symbols
  } deriving (Eq, Show)

data DescriptorCase = DescriptorCase {
    caseNumber :: Integer,
    caseId :: String,
    caseInstance :: String,
    caseName :: String,
    caseFile :: FilePath,
    caseLine :: Integer,
    caseVerifies :: [String]
  } deriving (Eq, Show)

data DescriptorCheck = DescriptorCheck {
    checkFile :: FilePath,
    checkLine :: Integer,
    checkProcedure :: String,
    checkText :: String
  } deriving (Eq, Show)

data Symbols = Symbols {
    symbolHeader :: String,
    symbolInit :: String,
    symbolEnd :: String,
    symbolReboot :: String
  } deriving (Eq, Show)

instance FromJSON Descriptor where
  parseJSON = withObject "descriptor" $ \o -> do
    stampText <- o .: "stamp"
    stamp <- case readHex (dropPrefix stampText) of
        [(value, "")] -> return value
        _ -> fail ("the stamp " ++ stampText ++ " is not a hexadecimal number")
    tasks <- o .: "tasks"
    emitters <- o .: "emitters"
    Descriptor
      <$> o .: "version"
      <*> pure stamp
      <*> o .: "name"
      <*> o .: "level"
      <*> o .: "platform"
      <*> o .:? "timeout"
      <*> o .: "cases"
      <*> o .: "checks"
      <*> pure (M.fromList [ (ident, name) | Entry ident name <- tasks ])
      <*> pure (M.fromList [ (ident, name) | Entry ident name <- emitters ])
      <*> o .: "symbols"

    where

      dropPrefix text =
        case text of
          '0' : 'x' : rest -> rest
          _ -> text

instance FromJSON DescriptorCase where
  parseJSON = withObject "case" $ \o -> DescriptorCase
    <$> o .: "number" <*> o .: "id" <*> o .: "instance" <*> o .: "case"
    <*> o .: "file" <*> o .: "line" <*> o .:? "verifies" .!= []

instance FromJSON DescriptorCheck where
  parseJSON = withObject "check" $ \o -> DescriptorCheck
    <$> o .: "file" <*> o .: "line" <*> o .: "procedure" <*> o .: "text"

instance FromJSON Symbols where
  parseJSON = withObject "symbols" $ \o -> Symbols
    <$> o .: "header" <*> o .: "init" <*> o .: "end" <*> o .: "reboot"

-- | An entry of the tables of tasks and of emitters
data Entry = Entry Integer String

instance FromJSON Entry where
  parseJSON = withObject "entry" $ \o -> Entry <$> o .: "id" <*> o .: "name"

decodeDescriptor :: BL.ByteString -> Either String Descriptor
decodeDescriptor = eitherDecode

readDescriptor :: FilePath -> IO (Either String Descriptor)
readDescriptor path = decodeDescriptor <$> BL.readFile path
