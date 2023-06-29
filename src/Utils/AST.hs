-- | Utility AST functions

module Utils.AST where

import           AST

-- Ground Type equiality?
groundTyEq :: TypeSpecifier -> TypeSpecifier -> Bool
groundTyEq  UInt8  UInt8 = True
groundTyEq  UInt16  UInt16 = True
groundTyEq  UInt32  UInt32 = True
groundTyEq  UInt64  UInt64 = True
groundTyEq  Int8  Int8 = True
groundTyEq  Int16  Int16 = True
groundTyEq  Int32  Int32 = True
groundTyEq  Int64  Int64 = True
groundTyEq  Bool  Bool = True
groundTyEq  (Option tyspecl) (Option tyspecr) = groundTyEq tyspecl tyspecr
groundTyEq  (Reference tyspecl) (Reference tyspecr) = groundTyEq tyspecl tyspecr
groundTyEq  (DynamicSubtype tyspecl) (DynamicSubtype tyspecr) = groundTyEq tyspecl tyspecr
groundTyEq  _ _ = False

----------------------------------------
-- Annotations helpers

forgetAnnotations :: AnnotatedProgram a -> Program
forgetAnnotations = map (fmap (const ()))

-- | First annotation level.
getAnnotations :: Expression a -> a
getAnnotations (Variable _ a)                           = a
getAnnotations (Constant _ a)                           = a
getAnnotations (BinOp _ _ _ a)                          = a
getAnnotations (ReferenceExpression _ a)                = a
getAnnotations (Casting _ _ a)                          = a
getAnnotations (FunctionExpression _ _ a)               = a
getAnnotations (FieldValuesAssignmentsExpression _ _ a) = a
getAnnotations (EnumVariantExpression _ _ _ a)          = a
getAnnotations (VectorIndexExpression _ _ a)            = a
getAnnotations (VectorInitExpression _ _ a)             = a
