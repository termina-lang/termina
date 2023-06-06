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

ppSimpleExpression :: Expression SemanticAnns -> DocStyle
ppSimpleExpression (Variable identifier _) = pretty identifier
ppSimpleExpression (Constant constant _) = ppConst constant
ppSimpleExpression (BinOp RelationalEqual lhs rhs _) = 
    let (lhsLocation, lhsTS) = (getLocation lhs, getExpType lhs) in
    let (rhsLocation, rhsTS) = (getLocation rhs, getExpType rhs) in
    case getExpType lhs of
        DefinedType identifier -> 
            ppFunctionCall (pretty (typeDefEqFunctionName identifier))
                [ppSimpleExpression (ReferenceExpression lhs (SemAnn lhsLocation (Reference lhsTS))),
                 ppSimpleExpression (ReferenceExpression rhs (SemAnn rhsLocation (Reference rhsTS)))]
        _ -> ppSimpleExpression lhs <> ppBinaryOperator RelationalEqual <> ppSimpleExpression rhs
ppSimpleExpression (BinOp op expr1 expr2 _) = ppSimpleExpression expr1 <> ppBinaryOperator op <> ppSimpleExpression expr2
ppSimpleExpression _ = error "unsupported expression"
