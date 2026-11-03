-- | The numbers of the cases of a unit test, which the harness, the descriptor
-- and termina test --list share.
module Core.TestCases (
    NumberedCase(..), parsedCases, typedCases
) where

import qualified Data.Map.Strict as M
import Data.List (sortOn)
import Core.AST
import Utils.Annotations (Location, Located(..))

-- | A case of a test instance and its number
data NumberedCase = NumberedCase {
    caseNumber :: Integer,
    caseInstance :: Identifier,
    caseClass :: Identifier,
    caseName :: Identifier,
    caseLocation :: Location
} deriving Show

-- | The numbered cases of a program as the parser gives it, where the type of
-- a test instance names its class.
parsedCases :: Located a => [AnnASTElement' (TypeSpecifier' texpr) expr blk a] -> Either String [NumberedCase]
parsedCases = programCases classOf

    where

        classOf (TSDefinedType cls []) = Just cls
        classOf _ = Nothing

-- | The numbered cases of a program after type checking, where the type of a
-- test instance is a test class.
typedCases :: Located a => [AnnASTElement' (TerminaType' texpr) expr blk a] -> Either String [NumberedCase]
typedCases = programCases classOf

    where

        classOf (TGlobal TestClass cls) = Just cls
        classOf _ = Nothing

-- | The numbered cases of the test instances of a program, given its elements
-- and the function that reads the class from the type of an instance.
programCases :: Located a =>
    (ty a -> Maybe Identifier)
    -> [AnnASTElement' ty expr blk a]
    -> Either String [NumberedCase]
programCases classOf elements =
    either notTestClass Right (numberCases instances classes)

    where

        instances = [ (ident, cls, getLocation ann)
                    | GlobalDeclaration (Test ident ty _ _ ann) <- elements, Just cls <- [classOf ty] ]

        classes = M.fromList [ (ident, [ (name, getLocation ann) | ClassCase _ name _ _ ann <- members ])
                             | TypeDefinition (Class TestClass ident members _ _) _ <- elements ]

        notTestClass (ident, cls) =
            Left $ "the test instance " ++ ident ++ " is of type " ++ cls ++ ", which is not a test class"

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
