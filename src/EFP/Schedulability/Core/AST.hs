module EFP.Schedulability.Core.AST
    (module EFP.Schedulability.Core.AST
    , module Core.AST) where

import Core.AST
import Utils.Annotations

data ConstFieldAssignment a = 
    ConstFieldAssignment {
        cfaField :: Identifier -- ^ Field name
        , cfaValue :: ConstExpression a -- ^ Value
    }
    deriving Show

data ConstExpression a
    = ConstInt TInteger a
    | ConstObject Identifier a
    | ConstBinOp Op (ConstExpression a) (ConstExpression a) a
    | ConstStructInitializer
        [ConstFieldAssignment a] -- ^ Initial value of each field identifier
        a
    deriving Show

instance Annotated ConstExpression where
  getAnnotation (ConstInt _ a)         = a
  getAnnotation (ConstObject _ a)      = a
  getAnnotation (ConstBinOp _ _ _ a)   = a
  getAnnotation (ConstStructInitializer _ a) = a

  updateAnnotation (ConstInt v _)          = ConstInt v
  updateAnnotation (ConstObject ident _)    = ConstObject ident
  updateAnnotation (ConstBinOp op e1 e2 _)  = ConstBinOp op e1 e2
  updateAnnotation (ConstStructInitializer fAs _) = ConstStructInitializer fAs
