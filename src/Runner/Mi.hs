-- | The lines of the machine interface of GDB (GDB/MI) that the executor of
-- GDB reads: the result of each command, the stops and the text of the console.
module Runner.Mi (
    MiValue(..), MiRecord(..),
    parseMiLine, miField, miText, miEscape
) where

import Data.Char (isDigit)
import Text.Parsec
import Text.Parsec.String (Parser)

data MiValue =
    MiString String
    | MiTuple [(String, MiValue)]
    | MiList [MiValue]
  deriving (Eq, Show)

data MiRecord =
    MiResult (Maybe Integer) String [(String, MiValue)] -- ^ token, class (done, running, error, exit) and results
    | MiAsync Char String [(String, MiValue)] -- ^ kind (* exec, + status, = notify), class and results
    | MiStream Char String -- ^ kind (~ console, @ target, & log) and text
    | MiPrompt
    | MiOther String -- ^ a line that is not of GDB/MI, such as the output of the program
  deriving (Eq, Show)

parseMiLine :: String -> MiRecord
parseMiLine line =
    case parse (record <* eof) "" (dropCarriageReturn line) of
        Right r -> r
        Left _ -> MiOther line

  where

    dropCarriageReturn text = if not (null text) && last text == '\r' then init text else text

-- | The value of a field of a list of results
miField :: String -> [(String, MiValue)] -> Maybe MiValue
miField = lookup

-- | The text of a field that is a string
miText :: String -> [(String, MiValue)] -> Maybe String
miText name results =
    case miField name results of
        Just (MiString text) -> Just text
        _ -> Nothing

-- | A string as a C string of GDB/MI, between quotes
miEscape :: String -> String
miEscape text = "\"" ++ concatMap escapeChar text ++ "\""

  where

    escapeChar c =
        case c of
            '"' -> "\\\""
            '\\' -> "\\\\"
            '\n' -> "\\n"
            _ -> [c]

record :: Parser MiRecord
record = prompt <|> stream <|> resultOrAsync

  where

    prompt = MiPrompt <$ try (string "(gdb)" <* spaces)

    stream = do
        kind <- oneOf "~@&"
        MiStream kind <$> cString

    resultOrAsync = do
        number <- optionMaybe (read <$> many1 (satisfy isDigit))
        kind <- oneOf "^*+="
        cls <- many1 (noneOf ",")
        results <- many (char ',' *> result)
        return $ if kind == '^' then MiResult number cls results else MiAsync kind cls results

result :: Parser (String, MiValue)
result = do
    name <- many1 (noneOf "={}[],\"")
    _ <- char '='
    v <- value
    return (name, v)

value :: Parser MiValue
value = (MiString <$> cString) <|> tuple <|> list

  where

    tuple = MiTuple <$> between (char '{') (char '}') (result `sepBy` char ',')

    -- | A list holds values or results; the names of the results are dropped
    list = MiList <$> between (char '[') (char ']') (element `sepBy` char ',')

    element = try (snd <$> result) <|> value

cString :: Parser String
cString = between (char '"') (char '"') (many (escaped <|> noneOf "\"\\"))

  where

    escaped = do
        _ <- char '\\'
        c <- anyChar
        return $ case c of
            'n' -> '\n'
            't' -> '\t'
            _ -> c
