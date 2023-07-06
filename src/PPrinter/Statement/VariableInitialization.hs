module PPrinter.Statement.VariableInitialization where

import PPrinter.Common
import PPrinter.Expression
import Prettyprinter
import SemanAST
import Semantic.Monad

ppInitializeVectorFromExpression ::
  Integer ->
  DocStyle ->
  DocStyle ->
  TypeSpecifier ->
  DocStyle
ppInitializeVectorFromExpression level target source ts =
  let iterator = namefy ("i" ++ show level)
   in case ts of
        -- \| If the initializer is a vector, we must iterate
        (Vector ts' (KC (I indexTS size))) ->
          let initExpr =
                ppCForLoopInitExpression
                  (ppPrimitiveType indexTS)
                  (pretty iterator)
                  (pretty (show (0 :: Integer)))
           in let condExpr =
                    pretty iterator <+> pretty "<" <+> parens (ppPrimitiveType indexTS) <> pretty size
               in let incrExpr =
                        ppCForLoopIncrExpression
                          (pretty iterator)
                          (parens (ppPrimitiveType indexTS) <> pretty (show (1 :: Integer)))
                   in ppCForLoop initExpr condExpr incrExpr (ppInitializeVectorFromExpression (level + 1) (target <> brackets (pretty iterator)) (source <> brackets (pretty iterator)) ts')
        _ -> target <+> pretty "=" <+> source <> semi

ppInitializeVector ::
  -- | Current vector nesting level. This argument is used to
  -- generate the name of the iterator variable.
  Integer ->
  -- |  Name of the target object. This name will contain the previous
  --  indexing expressions as needed.
  DocStyle ->
  -- |  The initialization expression
  Expression SemanticAnns ->
  DocStyle
ppInitializeVector level target expr =
  let iterator = namefy ("i" ++ show level)
   in case expr of
        (FieldValuesAssignmentsExpression {}) -> ppInitializeStruct target expr
        (OptionVariantExpression {}) -> ppInitializeOption target expr
        (VectorInitExpression expr' (KC (I indexTS size)) _) ->
          let initExpr =
                ppCForLoopInitExpression
                  (ppPrimitiveType indexTS)
                  (pretty iterator)
                  (pretty (show (0 :: Integer)))
           in let condExpr =
                    pretty iterator <+> pretty "<" <+> parens (ppPrimitiveType indexTS) <> pretty size
               in let incrExpr =
                        ppCForLoopIncrExpression
                          (pretty iterator)
                          (parens (ppPrimitiveType indexTS) <> pretty (show (1 :: Integer)))
                   in ppCForLoop
                        initExpr
                        condExpr
                        incrExpr
                        ( ppInitializeVector (level + 1) (target <> brackets (pretty iterator)) expr'
                        )
        (EnumVariantExpression {}) -> ppInitializeEnum target expr
        _ -> ppInitializeVectorFromExpression level target (ppExpression expr) (getType expr)

ppFieldInitializer :: DocStyle -> DocStyle -> Expression SemanticAnns -> DocStyle
ppFieldInitializer identifier field expr =
  case expr of
    (FieldValuesAssignmentsExpression {}) ->
      ppInitializeStruct (identifier <> pretty "." <> field) expr
    (OptionVariantExpression {}) ->
      ppInitializeOption (identifier <> pretty "." <> field) expr
    (VectorInitExpression {}) ->
      ppInitializeVector 0 (identifier <> pretty "." <> field) expr
    (EnumVariantExpression {}) ->
      ppInitializeEnum (identifier <> pretty "." <> field) expr
    _ -> identifier <> pretty "." <> field <+> pretty "=" <+> ppExpression expr <> semi

-- | This function initializes a struct using a field values assignments expression
-- as initializer. The name of sthe struct is passed as argument.
ppInitializeStruct ::
  -- | Name of the struct
  DocStyle ->
  -- |  The initialization expression
  Expression SemanticAnns ->
  DocStyle
ppInitializeStruct target expr =
  case expr of
    -- \| This function can only be called with a field values assignments expressions
    (FieldValuesAssignmentsExpression _ vas _) ->
        vsep $
            map (\(FieldValueAssignment field expr') -> ppFieldInitializer target (pretty field) expr') vas
    _ -> error "Incorrect expression"

ppInitializeEnum ::
  -- | Name of the enum
  DocStyle ->
  -- |  The initialization expression
  Expression SemanticAnns ->
  DocStyle
ppInitializeEnum target expr =
  case expr of
    -- \| This function can only be called with a field values assignments expressions
    (EnumVariantExpression _ variant params _) ->
        vsep $
            (target <> pretty "." <> enumVariantsField <+> pretty "=" <+> pretty variant <> semi) : zipWith
            (\e index -> ppFieldInitializer target (pretty (namefy variant) <> pretty "." <> pretty (namefy (show (index :: Integer)))) e) params [0..]
    _ -> error "Incorrect expression"

ppInitializeOption ::
    -- | Name of the option
    DocStyle ->
    -- |  The initialization expression
    Expression SemanticAnns ->
    DocStyle
ppInitializeOption target expr =
    case expr of
        (OptionVariantExpression (Some e) _) ->
            vsep [target <> pretty "." <> enumVariantsField <+> pretty "=" <+> optionSomeVariant <> semi,
                ppFieldInitializer target optionSomeField e]
        (OptionVariantExpression None _) ->
            target <> pretty "." <> enumVariantsField <+> pretty "=" <+> optionNoneVariant <> semi
        _ -> error "Incorrect expression"