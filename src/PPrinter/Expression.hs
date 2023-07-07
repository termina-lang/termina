module PPrinter.Expression where

import Prettyprinter

import SemanAST
import PPrinter.Common
import Semantic.Monad
import PPrinter.TypeDef
import Data.Map

type Substitutions = Map Identifier DocStyle

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
ppDynamicSubtypeCast (Vector ts _) =
    case ts of
        (Vector _ _) -> ppPrimitiveType ts <+> parens (pretty "*") <> ppDynamicSubtypeCast' ts
        _ -> ppPrimitiveType ts <+> pretty "*"
ppDynamicSubtypeCast ts = ppPrimitiveType ts <+> pretty "*"

ppDynamicSubtypeCast' :: TypeSpecifier -> DocStyle
ppDynamicSubtypeCast' (Vector ts (KC vs)) =
    case ts of
        (Vector _ _) -> brackets (ppConst vs) <> ppDynamicSubtypeCast' ts
        _ -> brackets (ppConst vs)
ppDynamicSubtypeCast' ts = error $ "unsupported type" ++ show ts

-- | Pretty prints the address of the object corresponding to the dynamic subtype
-- This function assumes that the expression is a dynamic subtype
ppDynamicSubtypeObjectAddress :: Substitutions -> Expression SemanticAnns -> DocStyle
ppDynamicSubtypeObjectAddress subs expr =
    case getType expr of
        DynamicSubtype ts ->
            parens (ppDynamicSubtypeCast ts) <> ppExpression subs expr <> pretty ".datum"
        _ -> error "unsupported expression"

ppRefDynamicSubtypeObjectAddress :: Substitutions -> Expression SemanticAnns -> DocStyle
ppRefDynamicSubtypeObjectAddress subs expr =
    case getType expr of
        Reference (DynamicSubtype ts) ->
            parens (ppDynamicSubtypeCast ts) <> (
                case expr of 
                    (ReferenceExpression _ _) ->  parens (ppExpression subs expr)
                    _ -> ppExpression subs expr
            ) <> pretty "->datum"
        _ -> error "unsupported expression"

ppDynamicSubtypeObject :: Substitutions -> Expression SemanticAnns -> DocStyle
ppDynamicSubtypeObject subs expr = ppCDereferenceExpression (ppDynamicSubtypeObjectAddress subs expr)

ppRefDynamicSubtypeObject :: Substitutions -> Expression SemanticAnns -> DocStyle
ppRefDynamicSubtypeObject subs expr = ppCDereferenceExpression (ppRefDynamicSubtypeObjectAddress subs expr)

ppMemberAccessExpression :: Substitutions -> Expression SemanticAnns -> Expression SemanticAnns -> DocStyle
-- | If the right hand side is a function, then it is a method call
ppMemberAccessExpression subs lhs (FunctionExpression methodId params _) =
    case getType lhs of
        (Reference ts) ->
            case ts of
                -- | If the left hand size is a class:
                (DefinedType classId) ->
                    ppCFunctionCall
                        (classMethodName classId methodId)
                        (ppExpression subs lhs : (ppExpression subs <$> params))
                -- | If the left hand side is a pool:
                (Pool _ _) ->
                    ppCFunctionCall
                        (poolMethodName methodId)
                        (ppExpression subs lhs : (ppExpression subs <$> params))
                -- | If the left hand side is a message queue:
                (MsgQueue _ _) ->
                    ppCFunctionCall
                        (msgQueueMethodName methodId)
                        (ppCReferenceExpression (ppExpression subs lhs) : (ppExpression subs <$> params))
                -- | Anything else should not happen
                _ -> error "unsupported expression"
        -- | If the left hand size is a class:
        (DefinedType classId) ->
            ppCFunctionCall
                (classMethodName classId methodId)
                (ppCReferenceExpression (ppExpression subs lhs) : (ppExpression subs <$> params))
        -- | If the left hand side is a pool:
        (Pool _ _) ->
            ppCFunctionCall
                (poolMethodName methodId)
                (ppCReferenceExpression (ppExpression subs lhs) : (ppExpression subs <$> params))
        -- | If the left hand side is a message queue:
        (MsgQueue _ _) ->
            ppCFunctionCall
                (msgQueueMethodName methodId)
                (ppCReferenceExpression (ppExpression subs lhs) : (ppExpression subs <$> params))
        -- | Anything else should not happen
        _ -> error "unsupported expression"
-- | If the right hand side is not a function, then it is a field
ppMemberAccessExpression subs lhs rhs = ppExpression subs lhs <> ppBinaryOperator MemberAccess <> ppExpression subs rhs

ppRelationalNotEqualExpression :: Substitutions -> Expression SemanticAnns -> Expression SemanticAnns -> DocStyle
ppRelationalNotEqualExpression subs lhs rhs =
    let lhsType = getType lhs
        rhsType = getType rhs in
    case (lhsType, rhsType) of
        -- If both are dynamic subtypes
        (DefinedType identifier, DefinedType _) ->
            case (lhs, rhs) of
                (Undyn lhs' _, Undyn rhs' _) ->
                    parens (hsep [ppCFunctionCall (typeDefEqFunctionName identifier)
                    [ppDynamicSubtypeObjectAddress subs lhs',
                    ppDynamicSubtypeObjectAddress subs rhs'], pretty "==", pretty "0"])
                (Undyn lhs' _, _) ->
                    parens (hsep [ppCFunctionCall (typeDefEqFunctionName identifier)
                    [ppDynamicSubtypeObjectAddress subs lhs',
                    ppCReferenceExpression (ppExpression subs rhs)], pretty "==", pretty "0"])
                (_,  Undyn rhs' _) ->
                    parens (hsep [ppCFunctionCall (typeDefEqFunctionName identifier)
                    [ppCReferenceExpression (ppExpression subs lhs),
                    ppDynamicSubtypeObjectAddress subs rhs'], pretty "==", pretty "0"])
                (_, _) ->
                    parens (hsep [ppCFunctionCall (typeDefEqFunctionName identifier)
                    [ppCReferenceExpression (ppExpression subs lhs),
                    ppCReferenceExpression (ppExpression subs rhs)], pretty "==", pretty "0"])
        -- Else, we can just compare the values        
        _ -> ppExpression subs lhs <> ppBinaryOperator RelationalNotEqual <> ppExpression subs rhs

ppRelationalEqualExpression :: Substitutions -> Expression SemanticAnns -> Expression SemanticAnns -> DocStyle
ppRelationalEqualExpression subs lhs rhs =
    let lhsType = getType lhs
        rhsType = getType rhs in
    case (lhsType, rhsType) of
        -- If both are defined types, we need to call the equality function
        (DefinedType identifier, DefinedType _) ->
            case (lhs, rhs) of
                (Undyn lhs' _, Undyn rhs' _) ->
                    parens (hsep [ppCFunctionCall (typeDefEqFunctionName identifier)
                    [ppDynamicSubtypeObjectAddress subs lhs',
                    ppDynamicSubtypeObjectAddress subs rhs'], pretty "==", pretty "1"])
                (Undyn lhs' _, _) ->
                    parens (hsep [ppCFunctionCall (typeDefEqFunctionName identifier)
                    [ppDynamicSubtypeObjectAddress subs lhs',
                    ppCReferenceExpression (ppExpression subs rhs)], pretty "==", pretty "1"])
                (_,  Undyn rhs' _) ->
                    parens (hsep [ppCFunctionCall (typeDefEqFunctionName identifier)
                    [ppCReferenceExpression (ppExpression subs lhs),
                    ppDynamicSubtypeObjectAddress subs rhs'], pretty "==", pretty "1"])
                (_, _) ->
                    parens (hsep [ppCFunctionCall (typeDefEqFunctionName identifier)
                    [ppCReferenceExpression (ppExpression subs lhs),
                    ppCReferenceExpression (ppExpression subs rhs)], pretty "==", pretty "1"])
        -- Else, we can just compare the values        
        _ -> ppExpression subs lhs <> ppBinaryOperator RelationalEqual <> ppExpression subs rhs

-- | Expression pretty printer
ppExpression :: Substitutions -> Expression SemanticAnns -> DocStyle
ppExpression subs (ParensExpression expr _) = parens (ppExpression subs expr)
-- | If the expresssion is a referece, we need to check if it is to a dynamic subtype
ppExpression subs (ReferenceExpression expr _) =
    case getType expr of
        -- | A reference to a dynamic subtype is the address of the datum
        (DynamicSubtype _) -> ppDynamicSubtypeObjectAddress subs expr
        -- | A reference to a vector is the vector itself  
        (Vector _ _) -> ppExpression subs expr
        _ -> ppCReferenceExpression (ppExpression subs expr)
ppExpression subs (DereferenceExpression expr _) =
    case getType expr of
        (Reference (Vector _ _)) -> ppExpression subs expr
        _ -> ppCDereferenceExpression (ppExpression subs expr)
ppExpression subs (Undyn expr _) =
    case getType expr of
        (DynamicSubtype (Vector _ _)) -> parens (ppDynamicSubtypeObjectAddress subs expr)
        (DynamicSubtype _) -> ppDynamicSubtypeObject subs expr
        _ -> error "Unsupported expression"
ppExpression subs (Variable identifier _) = findWithDefault (pretty identifier) identifier subs
ppExpression _ (Constant constant _) =
    case constant of
        B b -> if b then pretty "1" else pretty "0"
        I ts' integer -> parens (ppPrimitiveType ts') <> pretty integer
        C char -> squotes (pretty char)
ppExpression subs (BinOp RelationalEqual lhs rhs _) = ppRelationalEqualExpression subs lhs rhs
ppExpression subs (BinOp RelationalNotEqual lhs rhs _) = ppRelationalNotEqualExpression subs lhs rhs
ppExpression subs (BinOp MemberAccess lhs rhs _) = ppMemberAccessExpression subs lhs rhs
ppExpression subs (BinOp op expr1 expr2 _) =
    ppExpression subs expr1 <> ppBinaryOperator op <> ppExpression subs expr2
ppExpression subs (Casting expr' ts' _) = 
    case expr' of
        (Constant (I _ integer) _) -> parens (ppPrimitiveType ts') <> pretty integer
        _ -> parens (ppPrimitiveType ts') <> ppExpression subs expr'
ppExpression subs (FunctionExpression identifier params _) =
    ppCFunctionCall (pretty identifier) (ppExpression subs <$> params)
ppExpression subs (VectorIndexExpression vector index _) =
    ppExpression subs vector <> brackets (ppExpression subs index)
ppExpression _ _ = error "unsupported expression"
