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
  }

-- | Nothing this pass knows depends on the path taken: the aliasing map is
-- cleared at every full expression, so its state is all of the program's.
type SideEffectsMonad = DataflowM () SideEffectsEnv SideEffectsError

-- | Run a side-effects computation on a given target platform, starting from an
-- empty aliasing map. Returns the error raised, if any.
runSideEffects :: Platform -> SideEffectsMonad a -> Either SideEffectsError a
runSideEffects plt c =
  fst $ runDataflow (DFState () (SideEffectsEnv M.empty plt S.empty)) c

-- | Record the mutable-self methods of the class about to be checked.
setMutableSelfMethods :: S.Set Identifier -> SideEffectsMonad ()
setMutableSelfMethods ms = modifyGlobal (\s -> s { mutableSelfMethods = ms })

-- | Whether the named method borrows @self@ mutably.
isMutableSelfMethod :: Identifier -> SideEffectsMonad Bool
isMutableSelfMethod m = S.member m . mutableSelfMethods <$> getGlobal

-- | The mutable-self methods of the class currently being checked.
getMutableSelfMethods :: SideEffectsMonad (S.Set Identifier)
getMutableSelfMethods = mutableSelfMethods <$> getGlobal

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
