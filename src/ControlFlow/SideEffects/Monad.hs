module ControlFlow.SideEffects.Monad where
import Configuration.Platform (Platform)
import Control.Monad.Except
import qualified Control.Monad.State as ST
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
  }

type SideEffectsMonad = ExceptT SideEffectsError (ST.State SideEffectsEnv)

-- | Run a side-effects computation on a given target platform, starting from an
-- empty aliasing map. Returns the error raised, if any.
runSideEffects :: Platform -> SideEffectsMonad a -> Either SideEffectsError a
runSideEffects plt c =
  fst $ ST.runState (runExceptT c) (SideEffectsEnv M.empty plt S.empty)

-- | Record the mutable-self methods of the class about to be checked.
setMutableSelfMethods :: S.Set Identifier -> SideEffectsMonad ()
setMutableSelfMethods ms = ST.modify (\s -> s { mutableSelfMethods = ms })

-- | Whether the named method borrows @self@ mutably.
isMutableSelfMethod :: Identifier -> SideEffectsMonad Bool
isMutableSelfMethod m = ST.gets (S.member m . mutableSelfMethods)

-- | The mutable-self methods of the class currently being checked.
getMutableSelfMethods :: SideEffectsMonad (S.Set Identifier)
getMutableSelfMethods = ST.gets mutableSelfMethods

insertMutableReference :: AccessPath -> Location -> SideEffectsMonad ()
insertMutableReference ap loc = do
  prevMap <- ST.gets mutRefMap
  case find (mayAlias ap . fst) (M.toList prevMap) of
    Just (_, prevLoc) -> throwError $ annotateError loc (EPreviousMutableBorrow prevLoc)
    Nothing -> ST.modify (\s -> s { mutRefMap = M.insert ap loc (mutRefMap s)})

-- | Clear the mutable-reference map. The no-aliasing rule is scoped to a single
-- full expression.
resetMutableReferences :: SideEffectsMonad ()
resetMutableReferences = ST.modify (\s -> s { mutRefMap = M.empty })
