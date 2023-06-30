module PPrinter.Statement where

import Prettyprinter

import AST
import PPrinter.Common
import Semantic.Monad
import PPrinter.Expression



ppInitializeVectorFromExpression ::
    Integer
    -> DocStyle
    -> DocStyle
    -> TypeSpecifier -> DocStyle
ppInitializeVectorFromExpression level target source ts =
    let iterator = namefy ("l" ++ show level) in
    case ts of
        -- | If the initializer is a vector, we must iterate
        (Vector ts' (KC (I indexTS size))) ->
            let initExpr =
                    ppCForLoopInitExpression
                        (ppPrimitiveType indexTS) (pretty iterator) (pretty (show (0 :: Integer))) in
            let condExpr =
                    pretty iterator <+> pretty  "<" <+> parens (ppPrimitiveType indexTS) <> pretty size in
            let incrExpr =
                    ppCForLoopIncrExpression (pretty iterator)
                        (parens (ppPrimitiveType indexTS) <> pretty (show (1 :: Integer))) in
            ppCForLoop initExpr condExpr incrExpr (ppInitializeVectorFromExpression (level + 1) (target <> brackets (pretty iterator)) (source <> brackets (pretty iterator)) ts')
        _ -> target <+> pretty "=" <+> source <> semi

ppInitializeVector ::
    -- | Current vector nesting level. This argument is used to
    -- generate the name of the iterator variable.
    Integer
    -- | Name of the target object. This name will contain the previous
    -- indexing expressions as needed.
    -> DocStyle
    -- | The initialization expression
    -> Expression SemanticAnns -> DocStyle
ppInitializeVector level target expr =
    let iterator = namefy ("l" ++ show level) in
    case expr of
        (VectorInitExpression expr' (KC (I indexTS size)) _) ->
            let initExpr =
                    ppCForLoopInitExpression
                        (ppPrimitiveType indexTS) (pretty iterator) (pretty (show (0 :: Integer))) in
            let condExpr =
                    pretty iterator <+> pretty  "<" <+> parens (ppPrimitiveType indexTS) <> pretty size in
            let incrExpr =
                    ppCForLoopIncrExpression (pretty iterator)
                        (parens (ppPrimitiveType indexTS) <> pretty (show (1 :: Integer))) in
            ppCForLoop initExpr condExpr incrExpr (
                ppInitializeVector (level + 1) (target <> brackets (pretty iterator)) expr'
            )
        (FieldValuesAssignmentsExpression {}) -> error "Unsupported"
        _ -> ppInitializeVectorFromExpression level target (ppRootExpression expr) (getType expr)

ppInititalizeStructField :: Identifier -> FieldValueAssignment SemanticAnns -> [DocStyle]
ppInititalizeStructField identifier (FieldValueAssignment field expr) =
    case expr of
        (FieldValuesAssignmentsExpression {}) ->
            [ppCReferenceExpression (pretty (identifier ++ "_" ++ field))]
        _ -> [ppRootExpression expr]

ppInitializeStruct :: Identifier -> Expression SemanticAnns -> DocStyle
ppInitializeStruct target expr =
    case expr of
        (FieldValuesAssignmentsExpression identifier vas _) ->
            ppCFunctionCall (structAssignAnonymFunctionName identifier)
                    (ppCReferenceExpression (pretty target) : concatMap (ppInititalizeStructField (namefy target)) vas)
        _ -> error "Incorrect expression"

ppStatement :: Statement SemanticAnns -> DocStyle
ppStatement (Declaration identifier ts expr _) =
    case ts of
        Vector _ _ ->
            vsep [
                ppPrimitiveType ts <+> pretty identifier <> ppDimension ts <> semi,
                emptyDoc,
                braces' ((indentTab . align) (ppInitializeVector 0 (pretty identifier) expr))
            ]
        DefinedType _ ->
            case expr of
                (FieldValuesAssignmentsExpression _ vas _) ->
                    let initializers = reverse (concatMap (\(FieldValueAssignment i e) -> findFieldValuesAssignmentsExpressions (identifier ++ "_" ++ i) e) vas) in
                    vsep [
                        ppPrimitiveType ts <+> pretty identifier <> ppDimension ts <> semi,
                        emptyDoc,
                        braces' $ (indentTab . align) $ vsep $ 
                                concatMap (\(i, e) ->
                                    case e of
                                        (FieldValuesAssignmentsExpression ts' _ _) ->
                                            [pretty ts' <+> pretty i <> semi, ppInitializeStruct i e <> semi, emptyDoc]
                                        _ -> error "unsupported expression") initializers 
                                ++ [ppInitializeStruct identifier expr <> semi, emptyDoc]
                    ]
                _ -> ppPrimitiveType ts <+> pretty identifier <+> pretty "=" <+> ppRootExpression expr <> semi
        _ -> ppPrimitiveType ts <+> pretty identifier <> ppDimension ts <> semi
ppStatement _ = error "Not implemented yet"
