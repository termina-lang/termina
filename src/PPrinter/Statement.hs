module PPrinter.Statement where

import PPrinter.Common
import PPrinter.Expression
import Prettyprinter
import SemanAST
import Semantic.Monad

import PPrinter.Statement.VariableInitialization

ppDeclareAndInitialize :: 
    (DocStyle -> Expression SemanticAnns -> DocStyle)
    -> DocStyle
    -> TypeSpecifier
    -> Expression SemanticAnns -> DocStyle
ppDeclareAndInitialize initializer identifier ts expr =
    vsep
        [ 
            ppPrimitiveType ts <+> identifier <> ppDimension ts <> semi,
            emptyDoc,
            braces' $ (indentTab . align) $ initializer identifier expr
        ]

ppStatement :: Statement SemanticAnns -> DocStyle
ppStatement (Declaration identifier ts expr _) =
  case ts of
    Vector _ _ -> ppDeclareAndInitialize (ppInitializeVector 0) (pretty identifier) ts expr
    _ -> case expr of
        (FieldValuesAssignmentsExpression {}) ->
            ppDeclareAndInitialize ppInitializeStruct (pretty identifier) ts expr
        (OptionVariantExpression {}) ->
            ppDeclareAndInitialize ppInitializeOption (pretty identifier) ts expr
        (EnumVariantExpression {}) ->
            ppDeclareAndInitialize ppInitializeEnum (pretty identifier) ts expr
        _ -> ppPrimitiveType ts <+> pretty identifier <+> pretty "=" <+> ppExpression expr <> semi         
ppStatement (AssignmentStmt identifier expr _) =
    let ts = getType expr in
    case ts of
        Vector _ _ ->
            braces' $ (indentTab . align) $ ppInitializeVector 0 (pretty identifier) expr
        _ -> case expr of
            (FieldValuesAssignmentsExpression {}) ->
                braces' $ (indentTab . align) $ ppInitializeStruct (pretty identifier) expr
            (OptionVariantExpression {}) ->
                braces' $ (indentTab . align) $ ppInitializeOption (pretty identifier) expr
            (EnumVariantExpression {}) ->
                braces' $ (indentTab . align) $ ppInitializeEnum (pretty identifier) expr
            _ -> pretty identifier <+> pretty "=" <+> ppExpression expr <> semi
ppStatement _ = error "Not implemented yet"
