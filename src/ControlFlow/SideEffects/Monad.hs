module ControlFlow.SideEffects.Monad where
import Configuration.Platform (Platform)
import Control.Monad.Except
import ControlFlow.Dataflow
import ControlFlow.SideEffects.Errors
import Semantic.Utils
import Core.AST (Identifier)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (find)
import Utils.Annotations

data SideEffectsEnv = SideEffectsEnv
  {
    -- | Current list of mutable references
    mutRefMap :: M.Map AccessPath Location
    -- | Current deployment platform
  , targetPlatform :: Platform
    -- | Names of the methods of the class currently being checked that take a
    -- @&mut self@ receiver.
  , mutableSelfMethods :: S.Set Identifier
    -- | The functions whose body carries an effect that outlives the call,
    -- with the effect that made it so. It grows as the modules are checked in
    -- dependency order, so a call resolves against a function of a module this
    -- one imports.
  , effectfulFunctions :: M.Map Identifier Effect
    -- | The same for the members of the class currently being checked. A
    -- member is only called by its own class, and a class sees its members in
    -- the order they are defined, so the map is complete by the time the
    -- caller is reached.
  , effectfulMembers :: M.Map Identifier Effect
    -- | The effect found so far in the body of the function or member being
    -- checked, which is what gets recorded for it when the body ends.
  , effectOfCurrent :: Maybe Effect
  }

-- | Nothing this pass knows depends on the path taken: the aliasing map is
-- cleared at every full expression, so its state is all of the program's.
type SideEffectsMonad = DataflowM () SideEffectsEnv SideEffectsError

-- | Run a side-effects computation on a given target platform, starting from an
-- empty aliasing map and from the functions the modules already checked left
-- behind. Returns the error raised, if any, and the environment it ends with,
-- which is where the caller reads what this run learned.
runSideEffects :: Platform -> M.Map Identifier Effect -> SideEffectsMonad a
  -> (Either SideEffectsError a, SideEffectsEnv)
runSideEffects plt functions c =
  let (result, final) =
        runDataflow (DFState () (SideEffectsEnv M.empty plt S.empty functions M.empty Nothing)) c
  in (result, globalState final)

-- | Record the mutable-self methods of the class about to be checked.
setMutableSelfMethods :: S.Set Identifier -> SideEffectsMonad ()
setMutableSelfMethods ms = modifyGlobal (\s -> s { mutableSelfMethods = ms })

-- | Whether the named method borrows @self@ mutably.
isMutableSelfMethod :: Identifier -> SideEffectsMonad Bool
isMutableSelfMethod m = S.member m . mutableSelfMethods <$> getGlobal

-- | The mutable-self methods of the class currently being checked.
getMutableSelfMethods :: SideEffectsMonad (S.Set Identifier)
getMutableSelfMethods = mutableSelfMethods <$> getGlobal

-- | The functions and the members of the current class that are known to carry
-- an effect, which is what a call to one of them carries in turn.
getEffectful :: SideEffectsMonad (M.Map Identifier Effect, M.Map Identifier Effect)
getEffectful = do
  env <- getGlobal
  return (effectfulFunctions env, effectfulMembers env)

-- | Start a class, which brings its own members.
startClass :: S.Set Identifier -> SideEffectsMonad ()
startClass ms = modifyGlobal (\s -> s { mutableSelfMethods = ms, effectfulMembers = M.empty })

-- | Start the body of a function or of a member, with nothing found in it yet.
startCallable :: SideEffectsMonad ()
startCallable = modifyGlobal (\s -> s { effectOfCurrent = Nothing })

-- | Record the first effect found in the body being checked.
noteEffect :: Effect -> SideEffectsMonad ()
noteEffect effect = modifyGlobal $ \s ->
  case effectOfCurrent s of
    Just _  -> s
    Nothing -> s { effectOfCurrent = Just effect }

-- | Close the body of a function, keeping what it carries for its callers.
endFunction :: Identifier -> SideEffectsMonad ()
endFunction ident = modifyGlobal $ \s ->
  case effectOfCurrent s of
    Nothing     -> s
    Just effect -> s { effectfulFunctions = M.insert ident effect (effectfulFunctions s) }

-- | Close the body of a member, keeping what it carries for the other members
-- of its class.
endMember :: Identifier -> SideEffectsMonad ()
endMember ident = modifyGlobal $ \s ->
  case effectOfCurrent s of
    Nothing     -> s
    Just effect -> s { effectfulMembers = M.insert ident effect (effectfulMembers s) }

insertMutableReference :: AccessPath -> Location -> SideEffectsMonad ()
insertMutableReference ap loc = do
  prevMap <- mutRefMap <$> getGlobal
  case find (mayAlias ap . fst) (M.toList prevMap) of
    Just (_, prevLoc) -> throwError $ annotateError loc (EPreviousMutableBorrow prevLoc)
    Nothing -> modifyGlobal (\s -> s { mutRefMap = M.insert ap loc (mutRefMap s)})

-- | Clear the mutable-reference map. The no-aliasing rule is scoped to a single
-- full expression.
resetMutableReferences :: SideEffectsMonad ()
resetMutableReferences = modifyGlobal (\s -> s { mutRefMap = M.empty })
