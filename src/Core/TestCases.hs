-- | The numbers of the cases of a unit test, which the harness, the descriptor
-- and termina test --list share.
module Core.TestCases (
    NumberedCase(..), numberCases
) where

import qualified Data.Map.Strict as M
import Data.List (sortOn)
import Core.AST (Identifier)
import Utils.Annotations (Location)

-- | A case of a test instance and its number
data NumberedCase = NumberedCase {
    caseNumber :: Integer,
    caseInstance :: Identifier,
    caseClass :: Identifier,
    caseName :: Identifier,
    caseLocation :: Location
} deriving Show

-- | The test instances in the order of declaration and, for each one, the
-- cases of its class in the order of declaration, numbered from 1. It gives
-- back the instance and the class of an instance whose class is not known.
numberCases ::
    [(Identifier, Identifier, Location)]
    -> M.Map Identifier [(Identifier, Location)]
    -> Either (Identifier, Identifier) [NumberedCase]
numberCases instances classes =
    zipWith number [1 ..] . concat <$> mapM casesOf (sortOn third instances)

    where

        third (_, _, loc) = loc

        casesOf (ident, cls, _) =
            case M.lookup cls classes of
                Nothing -> Left (ident, cls)
                Just cases -> Right [ (ident, cls, name, loc) | (name, loc) <- sortOn snd cases ]

        number n (ident, cls, name, loc) = NumberedCase n ident cls name loc
