module PPrinter.Expression where

import Prettyprinter

import SemanAST
import PPrinter.Common
import Semantic.Monad
import PPrinter.TypeDef
import Data.Map

type Substitutions = Map Identifier DocStyle

-- | Type of the pretty printers for statements and expressions
type Printer a =
  Substitutions ->
  a SemanticAnns ->
  DocStyle

ppBinaryOperator :: Op -> DocStyle
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
ppDynamicSubtypeObjectAddress :: Printer Object
ppDynamicSubtypeObjectAddress subs obj =
    case getObjectType obj of
        DynamicSubtype ts ->
            parens (ppDynamicSubtypeCast ts) <> ppObject subs obj <> pretty ".datum"
        _ -> error "unsupported expression"

ppRefDynamicSubtypeObjectAddress :: Printer Expression
ppRefDynamicSubtypeObjectAddress subs expr =
    case getType expr of
        Reference (DynamicSubtype ts) ->
            parens (ppDynamicSubtypeCast ts) <> (
                case expr of
                    (ReferenceExpression _ _) ->  parens (ppExpression subs expr)
                    _ -> ppExpression subs expr
            ) <> pretty "->datum"
        _ -> error "unsupported expression"

ppDynamicSubtypeObject :: Printer Object
ppDynamicSubtypeObject subs obj = ppCDereferenceExpression (ppDynamicSubtypeObjectAddress subs obj)

ppRefDynamicSubtypeObject :: Printer Expression
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
ppMemberAccessExpression subs lhs rhs = ppExpression subs lhs <> pretty "." <> ppExpression subs rhs

ppObject :: Printer Object
ppObject subs (Variable identifier _) = findWithDefault (pretty identifier) identifier subs
-- ppObject subs (IdentifierExpression expr _)  = printer subs expr
ppObject subs (VectorIndexExpression vector index _) = ppObject subs vector <> brackets (ppExpression subs index)
ppObject subs (MemberAccess obj identifier _) = ppObject subs obj <> pretty "." <> pretty identifier
ppObject subs (Dereference obj _) =
        case getObjectType obj of
        -- | A dereference to a vector is printed as the name of the vector
        (Reference (Vector _ _)) -> ppObject subs obj
        _ -> ppCDereferenceExpression (ppObject subs obj)
-- | If the expression is a dynamic subtype treated as its base type, we need to
-- check if it is a vector
ppObject subs (Undyn obj _) =
    case getObjectType obj of
        -- | If it is a vector, we need to print the address of the datum
        (DynamicSubtype (Vector _ _)) -> parens (ppDynamicSubtypeObjectAddress subs obj)
        -- | Else, we print the derefence to the datum
        (DynamicSubtype _) -> ppDynamicSubtypeObject subs obj
        -- | An undyn can only be applied to a dynamic subtype. We are not
        -- supposed to reach here. If we are here, it means that the semantic
        -- analysis is wrong.
        _ -> error "Unsupported expression"

-- | Expression pretty printer
ppExpression :: Printer Expression
ppExpression subs (AccessObject obj) = ppObject subs obj
ppExpression subs (ParensExpression expr _) = parens (ppExpression subs expr)
-- | If the expresssion is a referece, we need to check if it is to a dynamic subtype
ppExpression subs (ReferenceExpression obj _) =
    case getObjectType obj of
        -- | A reference to a dynamic subtype is the address stored in the object
        (DynamicSubtype _) -> ppDynamicSubtypeObjectAddress subs obj
        -- | A reference to a vector is the vector itself (no need to create a reference
        -- to it)
        (Vector _ _) -> ppObject subs obj
        _ -> ppCReferenceExpression (ppObject subs obj)
-- | If the expression is a dereference, we need to check if it is to a vector
ppExpression subs (DereferenceExpression expr _) =
    case getType expr of
        -- | A dereference to a vector is printed as the name of the vector
        (Reference (Vector _ _)) -> ppExpression subs expr
        _ -> ppCDereferenceExpression (ppExpression subs expr)
ppExpression _ (Constant constant _) =
    case constant of
        B b -> if b then pretty "1" else pretty "0"
        I ts' integer -> parens (ppPrimitiveType ts') <> pretty integer
        C char -> squotes (pretty char)
ppExpression subs (BinOp op expr1 expr2 _) =
    ppExpression subs expr1 <> ppBinaryOperator op <> ppExpression subs expr2
ppExpression subs (Casting expr' ts' _) =
    case expr' of
        (Constant (I _ integer) _) -> parens (ppPrimitiveType ts') <> pretty integer
        _ -> parens (ppPrimitiveType ts') <> ppExpression subs expr'
ppExpression subs expr@(FunctionExpression identifier params _) =
    let
        paramAnns = getParameters expr
        ins = zipWith
            (\p (Parameter pid ts) ->
                case (p, ts) of
                    (AccessObject (Variable {}), Vector {}) ->
                        ppCDereferenceExpression
                            (parens (ppParameterVectorValueStructure (pretty identifier) (pretty pid) <+> pretty "*") <> ppExpression subs p)
                    (_, Vector {}) ->
                        ppCDereferenceExpression
                            (parens (ppParameterVectorValueStructure (pretty identifier) (pretty pid) <+> pretty "*") <> parens (ppExpression subs p))
                    (_, _) -> ppExpression subs p) params paramAnns
    in
    ppCFunctionCall (pretty identifier) ins
ppExpression subs (MemberMethodAccess obj methodId params _) =
    case getObjectType obj of
        (Reference ts) ->
            case ts of
                -- | If the left hand size is a class:
                (DefinedType classId) ->
                    ppCFunctionCall
                        (classMethodName classId methodId)
                        (ppObject subs obj : (ppExpression subs <$> params))
                -- | If the left hand side is a pool:
                (Pool _ _) ->
                    ppCFunctionCall
                        (poolMethodName methodId)
                        (ppObject subs obj : (ppExpression subs <$> params))
                -- | If the left hand side is a message queue:
                (MsgQueue _ _) ->
                    ppCFunctionCall
                        (msgQueueMethodName methodId)
                        (ppCReferenceExpression (ppObject subs obj) : (ppExpression subs <$> params))
                -- | Anything else should not happen
                _ -> error "unsupported expression"
        -- | If the left hand size is a class:
        (DefinedType classId) ->
            ppCFunctionCall
                (classMethodName classId methodId)
                (ppCReferenceExpression (ppObject subs obj) : (ppExpression subs <$> params))
        -- | If the left hand side is a pool:
        (Pool _ _) ->
            ppCFunctionCall
                (poolMethodName methodId)
                (ppCReferenceExpression (ppObject subs obj) : (ppExpression subs <$> params))
        -- | If the left hand side is a message queue:
        (MsgQueue _ _) ->
            ppCFunctionCall
                (msgQueueMethodName methodId)
                (ppCReferenceExpression (ppObject subs obj) : (ppExpression subs <$> params))
        -- | Anything else should not happen
        _ -> error "unsupported expression"
ppExpression _ _ = error "unsupported expression"
