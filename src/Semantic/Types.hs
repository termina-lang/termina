-- | Semantic interpretation of types

module Semantic.Types where

import           AST

----------------------------------------
-- Semantic interpretation of types.
-- Termina types are the same through out all the transpilation process.
-- We only have weird ints going around.
----------------------------------------

-- | Task type information
data SemTask a = STask
  { taskArgs :: [Parameter]
  , taskRet  :: TypeSpecifier
  , taskAnns :: a
  }
  deriving Show

-- type SemTask = SemTask' SemAnn

----------------------------------------

-- | Functions type information
data SemFunction a = SFunction
  { funArgs :: [Parameter]
  , funRet  :: TypeSpecifier
  , funAnns :: a
  }
  deriving Show

-- type SemFunction = SemFunction' SemAnn

----------------------------------------

-- | Handler type information
data SemHandler a = SemHandler
  { handlerArgs :: [Parameter]
  , handlerRet  :: TypeSpecifier
  , handlerAnns :: a
  }
  deriving Show

-- type SemHandler = SemHandler' SemAnn

----------------------------------------

-- | Global type information
data SemGlobal a
  = SVolatile TypeSpecifier a
  | SStatic TypeSpecifier a
  | SShared TypeSpecifier a
  | SConst TypeSpecifier a
  deriving Show

-- type SemGlobal = SemGlobal' SemAnn

getTySemGlobal :: SemGlobal a -> TypeSpecifier
getTySemGlobal (SVolatile ty _) = ty
getTySemGlobal (SStatic ty _)   = ty
getTySemGlobal (SShared ty _)   = ty
getTySemGlobal (SConst ty _)    = ty

----------------------------------------

-- | General global entities
data GEntry a
  = GFun (SemFunction a)
  -- ^ Functions
  | GTask (SemTask a)
  -- ^ Tasks
  | GHand (SemHandler a)
  -- ^ Handlers
  | GGlob (SemGlobal a)
  -- ^ Globals
  | GType (TypeDef a)
  -- ^ Types
  deriving Show

-- type GEntry = GEntry' SemAnn
