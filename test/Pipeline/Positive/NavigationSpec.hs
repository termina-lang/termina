-- | The index the language server builds from a typed module, which is what
-- answers "go to the definition". The module is typed through the whole
-- pipeline, so what is checked here is the index over the same AST the server
-- holds, without a server in the middle.
module Pipeline.Positive.NavigationSpec (spec) where

import Pipeline.Common

import qualified Data.Map.Strict as M
import Data.List (isPrefixOf)
import qualified Data.Text as T
import Test.Hspec

import LSP.Index
import Utils.Annotations (Location(..))
import Text.Parsec.Pos (sourceLine)

-- | A module with a function called from another one, a local, a parameter and
-- a resource with a method called on it.
source :: String
source = unlines
  [ "interface ICounter {"                            -- 1
  , "    procedure bump(&mut self, step : u32);"      -- 2
  , "};"                                              -- 3
  , ""                                                -- 4
  , "resource class CCounter provides ICounter {"     -- 5
  , "    count : u32;"                                -- 6
  , "    procedure bump(&mut self, step : u32) {"     -- 7
  , "        self->count = self->count + step;"       -- 8
  , "        return;"                                 -- 9
  , "    }"                                           -- 10
  , "};"                                              -- 11
  , ""                                                -- 12
  , "function twice(value : u32) -> u32 {"            -- 13
  , "    return value + value;"                       -- 14
  , "}"                                               -- 15
  , ""                                                -- 16
  , "function caller() -> u32 {"                      -- 17
  , "    var local : u32 = 3;"                        -- 18
  , "    return twice(local);"                        -- 19
  , "}"                                               -- 20
  , ""                                                -- 21
  , "function unwrap(opt : Option<u32>) -> u32 {"     -- 22
  , "    var acc : u32 = 0;"                          -- 23
  , "    match opt {"                                 -- 24
  , "        case Some(inner) => {"                   -- 25
  , "            acc = inner;"                        -- 26
  , "        }"                                       -- 27
  , "        case None => {}"                         -- 28
  , "    }"                                           -- 29
  , "    return acc;"                                 -- 30
  , "}"                                               -- 31
  ]

-- | The position of the first occurrence of a piece of text in a line, in the
-- one-based coordinates the index speaks.
positionOf :: Int -> String -> (Int, Int)
positionOf line needle =
    case lines source of
        ls | length ls >= line ->
            case [ col | (col, rest) <- zip [1 ..] (tails' (ls !! (line - 1)))
                       , needle `isPrefixOf` rest ] of
                (col:_) -> (line, col)
                [] -> error ("the line does not contain " ++ needle)
        _ -> error "the source has no such line"

    where

        tails' [] = [[]]
        tails' s@(_:rest) = s : tails' rest

index :: ModuleIndex
index =
    case runTypedModule "test" [("test", source)] of
        Left err -> error ("pipeline failed: " ++ show (failMessage err))
        Right typedProgram -> indexModule typedProgram

-- | The line a target resolves to, so the expectations read as line numbers.
lineOf :: Location -> Int
lineOf (Position _ start _) = sourceLine start
lineOf _ = error "the definition has no position"

definitionLine :: Target -> Int
definitionLine (Local loc) = lineOf loc
definitionLine (TopLevel ident) =
    maybe (error ("undefined: " ++ ident)) lineOf (M.lookup ident (indexTopLevel index))
definitionLine (Member owner ident) =
    maybe (error ("undefined member: " ++ ident)) lineOf
        (M.lookup (owner, ident) (indexMembers index))

jumpsTo :: (Int, Int) -> Int -> Expectation
jumpsTo here line =
    case referenceAt index here of
        Nothing -> expectationFailure ("nothing under " ++ show here)
        Just target -> definitionLine target `shouldBe` line

spec :: Spec
spec = describe "Navigation: the index of a typed module" $ do

  it "jumps from a call to the function it calls" $
    positionOf 19 "twice" `jumpsTo` 13

  it "jumps from a local to its declaration" $
    positionOf 19 "local)" `jumpsTo` 18

  it "jumps from a parameter to the place it is declared" $
    positionOf 14 "value +" `jumpsTo` 13

  it "jumps from a field to the place it is declared" $
    positionOf 8 "count =" `jumpsTo` 6

  it "jumps from a parameter of a procedure to its declaration" $
    positionOf 8 "step;" `jumpsTo` 7

  it "jumps from a variable bound by a case to the case that binds it" $
    positionOf 26 "inner;" `jumpsTo` 25

  it "jumps from self to the signature of the member it belongs to" $
    positionOf 8 "self->count = " `jumpsTo` 7

  it "finds every use of the same name" $
    let target = referenceAt index (positionOf 19 "twice")
        uses = [ loc | (loc, found) <- indexRefs index, Just found == target ]
    in length uses `shouldBe` 1

  it "finds the two uses of a parameter" $
    let target = referenceAt index (positionOf 14 "value +")
        uses = [ loc | (loc, found) <- indexRefs index, Just found == target ]
    in length uses `shouldBe` 2

  it "gives the type of what the cursor rests on" $
    typeAt index (positionOf 19 "local)") `shouldBe` Just (T.pack "u32")

  it "gives nothing where no name is written" $
    referenceAt index (16, 1) `shouldBe` Nothing

  it "reads the word under the cursor for what the walk does not reach" $
    wordAt (T.pack source) (positionOf 18 "u32") `shouldBe` Just "u32"
