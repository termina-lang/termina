-- | Utility AST functions

module Utils.AST where

import           AST
import           Utils.CoreAST

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

-- forgetAnnotations :: AnnotatedProgram a -> Program
-- forgetAnnotations = map (fmap (const ()))

-- | First annotation level.
getAnnotations :: Expression a -> a
getAnnotations (AccessObject (RHS obj))                 = getObjectAnnotations obj
getAnnotations (Constant _ a)                           = a
getAnnotations (BinOp _ _ _ a)                          = a
getAnnotations (ReferenceExpression _ a)                = a
getAnnotations (Casting _ _ a)                          = a
getAnnotations (FunctionExpression _ _ a)               = a
getAnnotations (FieldValuesAssignmentsExpression _ _ a) = a
getAnnotations (EnumVariantExpression _ _ _ a)          = a
getAnnotations (VectorInitExpression _ _ a)             = a

getAnnotationGlobal :: Global a -> a
getAnnotationGlobal (Volatile _ _ _ _ a) = a
getAnnotationGlobal (Static _ _ _ _ a)   = a
getAnnotationGlobal (Shared _ _ _ _ a)   = a
getAnnotationGlobal (Const _ _ _ _ a)    = a

getAnnotationType :: TypeDef a -> a
getAnnotationType (Struct _ _ _ a) = a
getAnnotationType (Union _ _ _ a)  = a
getAnnotationType (Enum _ _ _ a)   = a
getAnnotationType (Class _ _ _ a)  = a

getAnnotationsAST :: AnnASTElement a -> a
getAnnotationsAST (Task _ _ _ _ _ a) = a
getAnnotationsAST (Function _ _ _ _ _ a) = a
getAnnotationsAST (Handler _ _ _ _ _ a) = a
getAnnotationsAST (GlobalDeclaration glb) =  getAnnotationGlobal glb
getAnnotationsAST (TypeDefinition type_def) =  getAnnotationType type_def
getAnnotationsAST (ModuleInclusion {}) =  error "Module Inclusion not defined yet "
