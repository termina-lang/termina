module PPrinter.Expression where

import Prettyprinter

import AST
import PPrinter.Common
import Semantic.Monad
import PPrinter.TypeDef


ppBinaryOperator :: Op -> DocStyle
ppBinaryOperator MemberAccess = pretty "."
ppBinaryOperator Multiplication = space <> pretty "*" <> space
ppBinaryOperator Division = space <> pretty "/" <> space
ppBinaryOperator Addition = space <> pretty "+" <> space
ppBinaryOperator Subtraction = space <> pretty  "-" <> space
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

-- | Pretty prints the casting expression of a dynamic subtype
ppDynamicSubtypeCast :: TypeSpecifier -> DocStyle
ppDynamicSubtypeCast (Vector ts (KC size)) =
    case ts of
        (Vector _ _) -> ppDynamicSubtypeCast' ts <> brackets (ppConst size)
        _ -> ppPrimitiveType ts <+> pretty "*"
ppDynamicSubtypeCast ts = ppPrimitiveType ts <+> pretty "*"

ppDynamicSubtypeCast' :: TypeSpecifier -> DocStyle
ppDynamicSubtypeCast' (Vector ts (KC size)) =
    case ts of
        (Vector _ _) -> ppDynamicSubtypeCast' ts <> brackets (ppConst size)
        _ -> ppPrimitiveType ts <+> parens (pretty "*")
ppDynamicSubtypeCast' ts = error $ "unsupported type" ++ show ts

-- | Pretty prints the address of the object corresponding to the dynamic subtype
-- This function assumes that the expression is a dynamic subtype
ppDynamicSubtypeObjectAddress :: Expression SemanticAnns -> DocStyle
ppDynamicSubtypeObjectAddress expr =
    case getExpType expr of
        DynamicSubtype ts ->
            parens (ppDynamicSubtypeCast ts) <> ppExpression' expr <> pretty ".datum"
        _ -> error "unsupported expression"

ppRefDynamicSubtypeObjectAddress :: Expression SemanticAnns -> DocStyle
ppRefDynamicSubtypeObjectAddress expr =
    case getExpType expr of
        Reference (DynamicSubtype ts) ->
            parens (ppDynamicSubtypeCast ts) <> (
                case expr of 
                    (ReferenceExpression _ _) ->  parens (ppExpression expr)
                    _ -> ppExpression expr
            ) <> pretty "->datum"
        _ -> error "unsupported expression"

ppDynamicSubtypeObject :: Expression SemanticAnns -> DocStyle
ppDynamicSubtypeObject expr = ppCDereferenceExpression (ppDynamicSubtypeObjectAddress expr)

ppRefDynamicSubtypeObject :: Expression SemanticAnns -> DocStyle
ppRefDynamicSubtypeObject expr = ppCDereferenceExpression (ppRefDynamicSubtypeObjectAddress expr)

ppMemberAccessExpression :: Expression SemanticAnns -> Expression SemanticAnns -> DocStyle
-- | If the right hand side is a function, then it is a method call
ppMemberAccessExpression lhs (FunctionExpression methodId params _) =
    case getExpType lhs of
        (Reference ts) ->
            case ts of
                -- | If the left hand size is a class:
                (DefinedType classId) ->
                    ppCFunctionCall
                        (classMethodName classId methodId)
                        (ppExpression lhs : map ppRootExpression params)
                -- | If the left hand side is a pool:
                (Pool _ _) ->
                    ppCFunctionCall
                        (poolMethodName methodId)
                        (ppExpression lhs : map ppRootExpression params)
                -- | If the left hand side is a message queue:
                (MsgQueue _ _) ->
                    ppCFunctionCall
                        (msgQueueMethodName methodId)
                        (ppCReferenceExpression (ppExpression lhs) : map ppRootExpression params)
                -- | Anything else should not happen
                _ -> error "unsupported expression"
        -- | If the left hand size is a class:
        (DefinedType classId) ->
            ppCFunctionCall
                (classMethodName classId methodId)
                (ppCReferenceExpression (ppExpression lhs) : map ppRootExpression params)
        -- | If the left hand side is a pool:
        (Pool _ _) ->
            ppCFunctionCall
                (poolMethodName methodId)
                (ppCReferenceExpression (ppExpression lhs) : map ppRootExpression params)
        -- | If the left hand side is a message queue:
        (MsgQueue _ _) ->
            ppCFunctionCall
                (msgQueueMethodName methodId)
                (ppCReferenceExpression (ppExpression lhs) : map ppRootExpression params)
        -- | Anything else should not happen
        _ -> error "unsupported expression"
-- | If the right hand side is not a function, then it is a field
ppMemberAccessExpression lhs rhs = ppExpression lhs <> ppBinaryOperator MemberAccess <> ppExpression rhs

ppRelationalNotEqualExpression :: Expression SemanticAnns -> Expression SemanticAnns -> DocStyle
ppRelationalNotEqualExpression lhs rhs =
    let lhsType = getExpType lhs
        rhsType = getExpType rhs in
    case (lhsType, rhsType) of
        -- If both are dynamic subtypes
        (DynamicSubtype ts', DynamicSubtype _) ->
            case ts' of
                (DefinedType identifier) ->
                    parens (hsep [ppCFunctionCall (typeDefEqFunctionName identifier)
                    [ppDynamicSubtypeObjectAddress lhs,
                    ppDynamicSubtypeObjectAddress rhs], pretty "==", pretty "0"])
                _ -> ppDynamicSubtypeObject lhs <> ppBinaryOperator RelationalNotEqual <> ppDynamicSubtypeObject rhs
        -- If the left hand side is a dynamic subtype and the right hand side is a defined type
        (DynamicSubtype _, DefinedType identifier) ->
            parens (hsep [ppCFunctionCall (typeDefEqFunctionName identifier)
                [ppDynamicSubtypeObjectAddress lhs,
                 ppCReferenceExpression (ppExpression rhs)], pretty "==", pretty "0"])
        -- If the left hand side is a dynamic subtype and the right hand side is another pritimive type
        (DynamicSubtype _, _) ->
            ppDynamicSubtypeObject lhs <> ppBinaryOperator RelationalNotEqual <> ppExpression rhs
        -- If the right hand side is a dynamic subtype and the left hand side is a defined type
        (DefinedType identifier, DynamicSubtype _) ->
            parens (hsep [ppCFunctionCall (typeDefEqFunctionName identifier)
                [ppCReferenceExpression (ppExpression lhs),
                 ppDynamicSubtypeObjectAddress rhs], pretty "==", pretty "0"])
        -- If the right hand side is a dynamic subtype and the left hand side is another primitive type
        (_, DynamicSubtype _) ->
            ppExpression lhs <> ppBinaryOperator RelationalNotEqual <> ppDynamicSubtypeObject rhs
        -- If both are defined types, we need to call the equality function
        (DefinedType identifier, DefinedType _) ->
            parens (hsep [ppCFunctionCall (typeDefEqFunctionName identifier)
                [ppCReferenceExpression (ppExpression lhs),
                 ppCReferenceExpression (ppExpression rhs)], pretty "==", pretty "0"])
        -- Else, we can just compare the values        
        _ -> ppExpression lhs <> ppBinaryOperator RelationalNotEqual <> ppExpression rhs

ppRelationalEqualExpression :: Expression SemanticAnns -> Expression SemanticAnns -> DocStyle
ppRelationalEqualExpression lhs rhs =
    let lhsType = getExpType lhs
        rhsType = getExpType rhs in
    case (lhsType, rhsType) of
        -- If both are dynamic subtypes
        (DynamicSubtype ts', DynamicSubtype _) ->
            case ts' of
                (DefinedType identifier) ->
                    parens (hsep [ppCFunctionCall (typeDefEqFunctionName identifier)
                    [ppDynamicSubtypeObjectAddress lhs,
                    ppDynamicSubtypeObjectAddress rhs], pretty "==", pretty "1"])
                _ -> ppDynamicSubtypeObject lhs <> ppBinaryOperator RelationalEqual <> ppDynamicSubtypeObject rhs
        -- If the left hand side is a dynamic subtype and the right hand side is a defined type
        (DynamicSubtype _, DefinedType identifier) ->
            parens (hsep [ppCFunctionCall (typeDefEqFunctionName identifier)
                [ppDynamicSubtypeObjectAddress lhs,
                 ppCReferenceExpression (ppExpression rhs)], pretty "==", pretty "1"])
        -- If the left hand side is a dynamic subtype and the right hand side is another pritimive type
        (DynamicSubtype _, _) ->
            ppDynamicSubtypeObject lhs <> ppBinaryOperator RelationalEqual <> ppExpression rhs
        -- If the right hand side is a dynamic subtype and the left hand side is a defined type
        (DefinedType identifier, DynamicSubtype _) ->
            parens (hsep [ppCFunctionCall (typeDefEqFunctionName identifier)
                [ppCReferenceExpression (ppExpression lhs),
                 ppDynamicSubtypeObjectAddress rhs], pretty "==", pretty "1"])
        -- If the right hand side is a dynamic subtype and the left hand side is another primitive type
        (_, DynamicSubtype _) ->
            ppExpression lhs <> ppBinaryOperator RelationalEqual <> ppDynamicSubtypeObject rhs
        -- If both are defined types, we need to call the equality function
        (DefinedType identifier, DefinedType _) ->
            parens (hsep [ppCFunctionCall (typeDefEqFunctionName identifier)
                [ppCReferenceExpression (ppExpression lhs),
                 ppCReferenceExpression (ppExpression rhs)], pretty "==", pretty "1"])
        -- Else, we can just compare the values        
        _ -> ppExpression lhs <> ppBinaryOperator RelationalEqual <> ppExpression rhs

ppExpression' :: Expression SemanticAnns -> DocStyle
ppExpression' (Variable identifier _) = pretty identifier
ppExpression' (Constant constant _) =
    case constant of
        B b -> if b then pretty "1" else pretty "0"
        I ts' integer -> parens (ppPrimitiveType ts') <> pretty integer
        C char -> squotes (pretty char)
ppExpression' (BinOp RelationalEqual lhs rhs _) = ppRelationalEqualExpression lhs rhs
ppExpression' (BinOp RelationalNotEqual lhs rhs _) = ppRelationalNotEqualExpression lhs rhs
ppExpression' (BinOp MemberAccess lhs rhs _) = ppMemberAccessExpression lhs rhs
ppExpression' (BinOp op expr1 expr2 _) =
    ppExpression expr1 <> ppBinaryOperator op <> ppExpression expr2
ppExpression' (Casting expr' ts' _) = 
    case expr' of
        (Constant (I _ integer) _) -> parens (ppPrimitiveType ts') <> pretty integer
        _ -> parens (ppPrimitiveType ts') <> ppExpression expr'
ppExpression' (FunctionExpression identifier params _) =
    ppCFunctionCall (pretty identifier) (map ppRootExpression params)
ppExpression' (VectorIndexExpression vector index _) =
    ppExpression vector <> brackets (ppExpression index)
ppExpression' _ = error "unsupported expression"

-- | Expression pretty printer
ppExpression :: Expression SemanticAnns -> DocStyle
ppExpression (ParensExpression expr _) = parens (ppExpression expr)
-- | If the expresssion is a referece, we need to check if it is to a dynamic subtype
ppExpression (ReferenceExpression expr _) =
    case getExpType expr of
        -- | A reference to a dynamic subtype is the address of the datum
        (DynamicSubtype _) -> ppDynamicSubtypeObjectAddress expr
        -- | A reference to a vector is the vector itself  
        (Vector _ _) -> ppExpression expr
        _ -> ppCReferenceExpression (ppExpression expr)
ppExpression (DereferenceExpression expr _) =
    case getExpType expr of
        (Reference (Vector _ _)) -> ppExpression expr
        _ -> ppCDereferenceExpression (ppExpression expr)
ppExpression expr =
    case getExpType expr of
        (DynamicSubtype (Vector _ _)) -> parens (ppDynamicSubtypeObjectAddress expr)
        (DynamicSubtype _) -> ppDynamicSubtypeObject expr
        _ -> ppExpression' expr

-- | Pretty printer for a root expression
-- This printer is used to print the parameters of a function call or to print
-- single expression statements.
ppRootExpression :: Expression SemanticAnns -> DocStyle
ppRootExpression (ParensExpression expr _) = parens (ppRootExpression expr)
ppRootExpression expr =
    case expr of
        (ReferenceExpression _ _) -> ppExpression expr
        (DereferenceExpression _ _) -> ppExpression expr
        _ -> ppExpression' expr
