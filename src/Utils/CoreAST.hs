-- | Util functions related to CoreAST

module Utils.CoreAST where

import           CoreAST

-- traversalAnnotatedExp
--   -- AccessObject
--   :: (rho a -> b)
--   -- Constants
--   -> (Const -> a -> b)
--   -- ParensExpression
--   -> (b -> a -> b)
--   -- BinOp
--   -> (op -> b -> b -> a -> b)
--   -- Reference
--   -> (rho a -> a -> b)
--   -- DereferenceExpression
--   -> (b -> a -> b)
--   -- Casting
--   -> (b -> TypeSpecifier -> a -> b)
--   -- Function Expression
--   -> (Identifier -> [b] -> a -> b)
--   -- VectorInit
--   -> (b -> ConstExpression -> a -> b)
--   -- FieldValue
--   -> (Identifier -> [FieldValueAssignment' _b a])
--   Expression' rho a -> b
