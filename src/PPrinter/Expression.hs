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
ppBinaryOperator Multiplication = pretty "*"
ppBinaryOperator Division = pretty "/"
ppBinaryOperator Addition = pretty "+"
ppBinaryOperator Subtraction = pretty  "-"
ppBinaryOperator BitwiseLeftShift = pretty  "<<"
ppBinaryOperator BitwiseRightShift = pretty  ">>"
ppBinaryOperator RelationalLT = pretty  "<"
ppBinaryOperator RelationalLTE = pretty  "<="
ppBinaryOperator RelationalGT = pretty  ">"
ppBinaryOperator RelationalGTE = pretty  ">="
ppBinaryOperator RelationalEqual = pretty  "=="
ppBinaryOperator RelationalNotEqual = pretty  "!="
ppBinaryOperator BitwiseAnd = pretty  "&"
ppBinaryOperator BitwiseOr = pretty  "|"
ppBinaryOperator BitwiseXor = pretty  "^"
ppBinaryOperator LogicalAnd = pretty  "&&"
ppBinaryOperator LogicalOr = pretty  "||"

-- | Pretty prints the casting expression of a dynamic subtype
ppDynamicSubtypeCast :: TypeSpecifier -> DocStyle
ppDynamicSubtypeCast (Vector ts _) =
    case ts of
        (Vector _ _) -> ppTypeSpecifier ts <+> parens (pretty "*") <> ppDynamicSubtypeCast' ts
        _ -> ppTypeSpecifier ts <+> pretty "*"
ppDynamicSubtypeCast ts = ppTypeSpecifier ts <+> pretty "*"

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
            parens (ppDynamicSubtypeCast ts) <> ppObject subs obj <> pretty ".data"
        _ -> error "unsupported expression"

ppRefDynamicSubtypeObjectAddress :: Printer Expression
ppRefDynamicSubtypeObjectAddress subs expr =
    case getType expr of
        Reference (DynamicSubtype ts) ->
            parens (ppDynamicSubtypeCast ts) <> (
                -- If the expression has a precedence lower than the cast (2), we need
                -- to add parenthesis
                if getExpPrecedence expr > 2 then
                    parens (ppExpression subs expr)
                else
                    ppExpression subs expr
            ) <> pretty "->data"
        _ -> error "unsupported expression"

ppDynamicSubtypeObject :: Printer Object
ppDynamicSubtypeObject subs obj = ppCDereferenceExpression $ parens (ppDynamicSubtypeObjectAddress subs obj)

ppRefDynamicSubtypeObject :: Printer Expression
ppRefDynamicSubtypeObject subs expr = ppCDereferenceExpression $ parens (ppRefDynamicSubtypeObjectAddress subs expr)

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
ppObject subs (VectorSliceExpression vector lower _ _) = 
    case lower of
        KC (I lowTy lowInteger) ->
            parens $ ppCReferenceExpression (ppObject subs vector <> brackets (parens (ppTypeSpecifier lowTy) <> pretty lowInteger))
        _ -> error $ "Invalid constant expression: " ++ show lower
ppObject subs (MemberAccess obj identifier _) = ppObject subs obj <> pretty "." <> pretty identifier
ppObject subs (Dereference obj _) =
        case getObjectType obj of
        -- | A dereference to a vector is printed as the name of the vector
        (Reference (Vector _ _)) -> ppObject subs obj
        _ -> 
            if getObjPrecedence obj > 2 then
                ppCDereferenceExpression $ parens (ppObject subs obj)
            else
                ppCDereferenceExpression (ppObject subs obj)
-- | If the expression is a dynamic subtype treated as its base type, we need to
-- check if it is a vector
ppObject subs (Undyn obj _) =
    case getObjectType obj of
        -- | If it is a vector, we need to print the address of the data
        (DynamicSubtype (Vector _ _)) -> parens (ppDynamicSubtypeObjectAddress subs obj)
        -- | Else, we print the derefence to the data
        (DynamicSubtype _) -> ppDynamicSubtypeObject subs obj
        -- | An undyn can only be applied to a dynamic subtype. We are not
        -- supposed to reach here. If we are here, it means that the semantic
        -- analysis is wrong.
        _ -> error "Unsupported expression"

-- | Pretty prints binary expressions casted to the type of the expression
-- This function is only used for subexpressions of binary expressions.
-- The reason for this is that we want to disable integer promotion at C level.
ppExpression' :: Printer Expression
ppExpression' subs expr@(BinOp Addition expr1 expr2 _) =
    let ts = getType expr in
        parens (ppTypeSpecifier ts) <> parens (ppExpression' subs expr1 <+> ppBinaryOperator Addition <+> ppExpression' subs expr2)
ppExpression' subs expr@(BinOp Subtraction expr1 expr2 _) =
    let ts = getType expr in
        parens (ppTypeSpecifier ts) <> parens (ppExpression' subs expr1 <+> ppBinaryOperator Subtraction <+> ppExpression' subs expr2)
ppExpression' subs expr@(BinOp Multiplication expr1 expr2 _) =
    let ts = getType expr in
        parens (ppTypeSpecifier ts) <> parens (ppExpression' subs expr1 <+> ppBinaryOperator Multiplication <+> ppExpression' subs expr2)
ppExpression' subs expr@(BinOp Division expr1 expr2 _) =
    let ts = getType expr in
        parens (ppTypeSpecifier ts) <> parens (ppExpression' subs expr1 <+> ppBinaryOperator Division <+> ppExpression' subs expr2)
ppExpression' subs expr@(BinOp BitwiseLeftShift expr1 expr2 _) =
    let ts = getType expr in
        parens (ppTypeSpecifier ts) <> parens (ppExpression' subs expr1 <+> ppBinaryOperator BitwiseLeftShift <+> ppExpression' subs expr2)
ppExpression' subs expr@(BinOp BitwiseRightShift expr1 expr2 _) =
    let ts = getType expr in
        parens (ppTypeSpecifier ts) <> parens (ppExpression' subs expr1 <+> ppBinaryOperator BitwiseRightShift <+> ppExpression' subs expr2)
ppExpression' subs expr@(BinOp BitwiseAnd expr1 expr2 _) =
    let ts = getType expr in
        parens (ppTypeSpecifier ts) <> parens (ppExpression' subs expr1 <+> ppBinaryOperator BitwiseAnd <+> ppExpression' subs expr2)
ppExpression' subs expr@(BinOp BitwiseOr expr1 expr2 _) =
    let ts = getType expr in
        parens (ppTypeSpecifier ts) <> parens (ppExpression' subs expr1 <+> ppBinaryOperator BitwiseOr <+> ppExpression' subs expr2)
ppExpression' subs expr@(BinOp BitwiseXor expr1 expr2 _) =
    let ts = getType expr in
        parens (ppTypeSpecifier ts) <> parens (ppExpression' subs expr1 <+> ppBinaryOperator BitwiseXor <+> ppExpression' subs expr2)
ppExpression' subs (BinOp op expr1 expr2 _) =
    ppExpression' subs expr1 <+> ppBinaryOperator op <+> ppExpression' subs expr2
ppExpression' subs expr = ppExpression subs expr

-- | Expression pretty printer
ppExpression :: Printer Expression
ppExpression subs (AccessObject obj) = ppObject subs obj
-- | If the expresssion is a referece, we need to check if it is to a dynamic subtype
ppExpression subs (ReferenceExpression obj _) =
    case getObjectType obj of
        -- | A reference to a dynamic subtype is the address stored in the object
        (DynamicSubtype _) -> ppDynamicSubtypeObjectAddress subs obj
        -- | A reference to a vector is the vector itself (no need to create a reference
        -- to it)
        (Vector _ _) -> ppObject subs obj
        _ -> ppCReferenceExpression (ppObject subs obj)
ppExpression _ (Constant constant _) =
    case constant of
        B b -> if b then pretty "1" else pretty "0"
        I ts' integer -> parens (ppTypeSpecifier ts') <> pretty integer
        C char -> squotes (pretty char)
ppExpression subs (BinOp op expr1 expr2 _) =
    ppExpression' subs expr1 <+> ppBinaryOperator op <+> ppExpression' subs expr2
ppExpression subs (Casting expr' ts' _) =
    case expr' of
        (Constant (I _ integer) _) -> parens (ppTypeSpecifier ts') <> pretty integer
        _ -> parens (ppTypeSpecifier ts') <> ppExpression subs expr'
ppExpression subs expr@(FunctionExpression identifier params _) =
    let
        paramAnns = getParameters expr
        ins = zipWith
            (\p (Parameter pid ts) ->
                case (p, ts) of
                    (_, Vector {}) ->
                        if getExpPrecedence p > 2 then
                            ppCReferenceExpression $ parens
                                (parens (ppParameterVectorValueStructure (pretty identifier) (pretty pid) <+> pretty "*") <> parens (ppExpression subs p))
                        else
                            ppCDereferenceExpression $ parens
                                (parens (ppParameterVectorValueStructure (pretty identifier) (pretty pid) <+> pretty "*") <> ppExpression subs p)
                    (_, _) -> ppExpression subs p) params paramAnns
    in
        case getType expr of
            Vector {} -> ppCFunctionCall (pretty identifier) ins <> pretty ".array"
            _ -> ppCFunctionCall (pretty identifier) ins
ppExpression subs (MemberMethodAccess obj methodId params _) =
    case getObjectType obj of
        (Reference ts) ->
            case ts of
                -- | If the left hand size is a class:
                (DefinedType classId) ->
                    ppCFunctionCall
                        (classMethodNameAOp classId methodId)
                        (ppObject subs obj : (ppExpression subs <$> params))
                -- | If the left hand side is a pool:
                (Pool _ _) ->
                    ppCFunctionCall
                        (poolMethodNameAOp methodId)
                        (ppObject subs obj : (ppExpression subs <$> params))
                -- | If the left hand side is a message queue:
                (MsgQueue _ _) ->
                    ppCFunctionCall
                        (msgQueueMethodNameAOp methodId)
                        (ppCReferenceExpression (ppObject subs obj) : (ppExpression subs <$> params))
                -- | Anything else should not happen
                _ -> error "unsupported expression"
        -- | If the left hand size is a class:
        (DefinedType classId) ->
            ppCFunctionCall
                (classMethodNameAOp classId methodId)
                (ppCReferenceExpression (ppObject subs obj) : (ppExpression subs <$> params))
        -- | If the left hand side is a pool:
        (Pool _ _) ->
            ppCFunctionCall
                (poolMethodNameAOp methodId)
                (ppCReferenceExpression (ppObject subs obj) : (ppExpression subs <$> params))
        -- | If the left hand side is a message queue:
        (MsgQueue _ _) ->
            ppCFunctionCall
                (msgQueueMethodNameAOp methodId)
                (ppCReferenceExpression (ppObject subs obj) : (ppExpression subs <$> params))
        -- | Anything else should not happen
        _ -> error "unsupported expression"
ppExpression _ expr = error $  "unsupported expression" ++ show expr
