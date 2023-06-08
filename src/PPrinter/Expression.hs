module PPrinter.Expression where

import Prettyprinter

import AST
import PPrinter.Common
import Semantic.Monad


ppBinaryOperator :: Op -> DocStyle
ppBinaryOperator MemberAccess = pretty "."
ppBinaryOperator Multiplication = pretty "*"
ppBinaryOperator Division = space <> pretty "/" <> space
ppBinaryOperator Addition = space <> pretty "+" <> space
ppBinaryOperator Substraction = space <> pretty  "-" <> space
ppBinaryOperator BitwiseLeftShift = space <> pretty  "<<" <> space
ppBinaryOperator BitwiseRightShift = space <> pretty  ">>" <> space
ppBinaryOperator RelationalLT = space <> pretty  "<" <> space
ppBinaryOperator RelationalLTE = space <> pretty  "<=" <> space
ppBinaryOperator RelationalGT = space <> pretty  ">" <> space
ppBinaryOperator RelationalGTE = space <> pretty  ">=" <> space
ppBinaryOperator RelationalEqual = space <> pretty  "==" <> space
ppBinaryOperator RelationalNotEqual = space <> pretty  "!=" <> space
ppBinaryOperator BitwiseAnd = space <> pretty  "&" <> space
ppBinaryOperator BitwiseOr = space <> pretty  "|" <> space
ppBinaryOperator BitwiseXor = space <> pretty  "^" <> space
ppBinaryOperator LogicalAnd = space <> pretty  "&&" <> space
ppBinaryOperator LogicalOr = space <> pretty  "||" <> space

ppRelationalEqualExpression :: Expression SemanticAnns -> Expression SemanticAnns -> DocStyle
ppRelationalEqualExpression lhs rhs =
    let lhsType = getExpType lhs
        rhsType = getExpType rhs in
    case (lhsType, rhsType) of
        -- If both are dynamic subtypes, we need to compare the types
        (DynamicSubtype ts, DynamicSubtype _) -> 
            (case ts of 
                -- | If the type is a structured type, we must call the equality function
                DefinedType identifier -> 
                    ppCFunctionCall (typeDefEqFunctionName identifier)
                        [ppExpression lhs, -- No need for a reference
                         ppExpression rhs] -- No need for a reference
                -- | Else, we can just compare the values dereferencing the expressions
                _ -> ppCDereferenceExpression (ppExpression lhs) <> 
                        ppBinaryOperator RelationalEqual <> ppCDereferenceExpression (ppExpression rhs))
        -- If the type of the LHS is a structured type and the right hand side is a dynamic subtype, 
        -- we need to call the equality function but only referencing the left hand side
        (DefinedType identifier, DynamicSubtype _) -> 
            ppCFunctionCall (typeDefEqFunctionName identifier)
                [ppCReferenceExpression (ppExpression lhs),
                 ppExpression rhs]
        -- If the left hand side is a dynamic subtype and the type of the RHS is a structured type, 
        -- we need to call the equality function but only referencing the right hand side
        (DynamicSubtype (DefinedType identifier), DefinedType _) -> 
            ppCFunctionCall (typeDefEqFunctionName identifier)
                [ppExpression lhs,
                 ppCReferenceExpression (ppExpression rhs)]
        -- Else, we can just compare the values referencing the expressions
        (DefinedType identifier, _) -> 
            ppCFunctionCall (typeDefEqFunctionName identifier)
                [ppCReferenceExpression (ppExpression lhs),
                 ppCReferenceExpression (ppExpression rhs)]
        _ -> ppExpression lhs <> ppBinaryOperator RelationalEqual <> ppExpression rhs

-- | Expression pretty printer
-- This printer can only be used simple expressions that are
-- not part of complex statements (i.e., match or assignments).
ppExpression :: Expression SemanticAnns -> DocStyle
ppExpression (Variable identifier _) = pretty identifier
ppExpression (Constant constant _) = case constant of
  B b -> if b then pretty "1" else pretty "0"
  I ts integer -> parens (ppRootType ts) <> pretty integer
  C char -> pretty "'" <> pretty char <> pretty "'"
ppExpression (BinOp RelationalEqual lhs rhs _) = ppRelationalEqualExpression lhs rhs
ppExpression (BinOp op expr1 expr2 _) = ppExpression expr1 <> ppBinaryOperator op <> ppExpression expr2
ppExpression (ReferenceExpression expr _) = ppCReferenceExpression (ppExpression expr)
ppExpression (DereferenceExpression expr _) = ppCDereferenceExpression (ppExpression expr)
ppExpression (ParensExpression expr _) = parens (ppExpression expr)
ppExpression (Casting expr ts _) = parens (ppRootType ts) <> ppExpression expr
ppExpression (FunctionExpression identifier params _) = 
    ppCFunctionCall (pretty identifier) (map ppExpression params)
ppExpression (VectorIndexExpression vector index _) =
    ppExpression vector <> brackets (ppExpression index)
ppExpression _ = error "unsupported expression"
