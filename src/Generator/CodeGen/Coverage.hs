{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The flags of the coverage before the statements and the witnesses in the decisions.
module Generator.CodeGen.Coverage (
    statementsArrayName,
    witnessesArrayName,
    statementPoint,
    witness,
    newPoint,
    casePoint,
    currentStatement,
    setCoveredFunction,
    coverageItems,
    renderCoverageMap
) where

import Generator.CodeGen.Common
import Generator.LanguageC.AST
import Generator.LanguageC.Embedded
import Utils.Annotations
import Utils.Hash (sha256Hex)
import qualified Control.Monad.State as ST
import Control.Monad.Except (throwError)
import Data.List (intercalate)
import System.FilePath (splitDirectories)
import Data.Aeson (object, (.=), encode)
import Data.Aeson.Encode.Pretty (Config(..), defConfig, encodePretty', keyOrder)
import qualified Data.ByteString.Lazy as BL
import Text.Parsec.Pos (sourceLine, sourceColumn)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- | The array of the flags of the statements of a module.
statementsArrayName :: QualifiedName -> Ident
statementsArrayName mName = "termina__cov_stmt__" ++ intercalate "__" (splitDirectories mName)

-- | The array of the witnesses of a module, a row of two flags each.
witnessesArrayName :: QualifiedName -> Ident
witnessesArrayName mName = "termina__cov_cond__" ++ intercalate "__" (splitDirectories mName)

-- | The name of the function that sets the flag of the value of a witness.
witnessName :: Ident
witnessName = "termina__cov__w"

flagType :: CType
flagType = CTInt IntSize8 Unsigned volatile

-- | A row of the array of the witnesses: [0] the value true, [1] the value false.
witnessRowType :: CType
witnessRowType = CTArray flagType (dec 2 @: size_t)

-- | Takes the next index of the array of the point, and notes the point.
newPoint :: CoveragePointKind -> Location -> CGenerator Integer
newPoint kind loc = do
    st <- ST.gets coverageState
    case st of
        Nothing -> throwError $ InternalError "A coverage point in a module without coverage"
        Just cov -> do
            let (index, cov') = case kind of
                    StatementPoint _ -> (nextStatement cov, cov { nextStatement = nextStatement cov + 1 })
                    _ -> (nextWitness cov, cov { nextWitness = nextWitness cov + 1 })
                point = CoveragePoint index kind loc (coveredFunction cov)
            ST.modify (\env -> env { coverageState = Just cov' { coveragePoints = point : coveragePoints cov' } })
            return index

-- | Notes a case of a match without a test, which has no flag of its own.
casePoint :: CaseEntry -> Location -> CGenerator ()
casePoint entry loc =
    ST.modify (\env -> env { coverageState = (\cov -> cov { coveragePoints =
        CoveragePoint (-1) (CasePoint entry) loc (coveredFunction cov) : coveragePoints cov }) <$> coverageState env })

-- | The index of the last statement point, the one of the statement being generated.
currentStatement :: CGenerator (Maybe Integer)
currentStatement = ST.gets (fmap (subtract 1 . nextStatement) . coverageState)

-- | The C function whose body is being generated, for the map.
setCoveredFunction :: String -> CGenerator ()
setCoveredFunction fname =
    ST.modify (\env -> env { coverageState = (\cov -> cov { coveredFunction = fname }) <$> coverageState env })

-- | The flag before a statement, or nothing when the module has no coverage.
statementPoint :: Maybe String -> Location -> CGenerator [CCompoundBlockItem]
statementPoint construct loc = do
    st <- ST.gets coverageState
    case st of
        Nothing -> return []
        Just _ -> do
            index <- newPoint (StatementPoint construct) loc
            mName <- ST.gets currentModule
            let cArray = CVar (statementsArrayName mName) (CTArray flagType (dec 0 @: size_t))
                cFlag = CIndexOf cArray (dec index @: size_t) flagType
            return [pre_cr ((cFlag @= dec 1 @: uint8_t) |>> loc) |>> loc]

-- | A call to the witness of a decision or a condition, with its row.
witness :: Integer -> Location -> CExpression -> CGenerator CExpression
witness index loc cValue = do
    mName <- ST.gets currentModule
    let cArray = CVar (witnessesArrayName mName) (CTArray witnessRowType (dec 0 @: size_t)) :: CObject
        cRow = CIndexOf cArray (dec index @: size_t) witnessRowType :: CObject
        cWitness = witnessName @: CTFunction (CTBool noqual) [witnessRowType, CTBool noqual]
    return $ cWitness @@ [cRow @: witnessRowType, cValue] |>> loc

-- | The arrays of the flags of an instrumented module, after TERMINA__COVERAGE__PLACEMENT.
coverageItems :: CGenerator [CFileItem]
coverageItems = do
    st <- ST.gets coverageState
    mName <- ST.gets currentModule
    case st of
        Nothing -> return []
        Just cov | nextStatement cov == 0 && nextWitness cov == 0 -> return []
        Just cov -> do
            let placed name cType =
                    CExtDecl (CEDMacroVariable placementMacro (CDecl (CTypeSpec cType) (Just name) Nothing))
                        (internalAnn (CDeclarationAnn False))
                statements = placed (statementsArrayName mName)
                    (CTArray flagType (dec (nextStatement cov) @: size_t))
                witnesses = placed (witnessesArrayName mName)
                    (CTArray witnessRowType (dec (nextWitness cov) @: size_t))
            return $ [pre_cr statements | nextStatement cov > 0] ++ [no_cr witnesses | nextWitness cov > 0]

    where

        placementMacro :: Ident
        placementMacro = "TERMINA__COVERAGE__PLACEMENT"

-- | The map of a module, given the version of termina, its source and the
-- SHA-256 of it: what each flag records, and the stamp of all of it.
renderCoverageMap :: String -> FilePath -> String -> QualifiedName -> [CoveragePoint] -> T.Text
renderCoverageMap terminaVersion source sourceHash mName points =
    let statements = length [() | CoveragePoint _ (StatementPoint _) _ _ <- points]
        witnesses = length [() | CoveragePoint _ kind _ _ <- points, isWitness kind]
        content = [ "termina" .= terminaVersion
                  , "source" .= source
                  , "source-sha256" .= sourceHash
                  , "statements" .= statements
                  , "witnesses" .= witnesses
                  , "points" .= map point points ]
        stamp = sha256Hex (encode (object content))
        keys = ["module", "stamp", "termina", "source", "source-sha256", "statements", "witnesses", "points",
                "index", "kind", "construct", "decision", "after", "statement", "function", "start", "end"]
    in
    TE.decodeUtf8 . BL.toStrict $ encodePretty' defConfig { confCompare = keyOrder keys }
        (object (("module" .= mName) : ("stamp" .= ("sha256:" ++ stamp)) : content))

    where

        isWitness kind = case kind of
            DecisionPoint _ -> True
            ConditionPoint _ -> True
            _ -> False

        point (CoveragePoint index kind loc fname) = object $
            [ "index" .= index | index >= 0 ] ++ [ "function" .= fname ]
            ++ case kind of
                StatementPoint construct -> ("kind" .= ("statement" :: T.Text)) : maybe [] (\c -> ["construct" .= c]) construct
                DecisionPoint construct -> ("kind" .= ("decision" :: T.Text)) : maybe [] (\c -> ["construct" .= c]) construct
                ConditionPoint decision -> ["kind" .= ("condition" :: T.Text), "decision" .= decision]
                CasePoint (AfterCase decision) -> ["kind" .= ("case" :: T.Text), "after" .= decision]
                CasePoint (WithStatement statement) -> ["kind" .= ("case" :: T.Text), "statement" .= statement]
            ++ position loc

        -- | Line and column where the construct starts and ends
        position (Position _ start end) =
            [ "start" .= [sourceLine start, sourceColumn start]
            , "end" .= [sourceLine end, sourceColumn end] ]
        position _ = []
