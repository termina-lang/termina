module PPrinter.Statement.VariableInitialization where

import PPrinter.Common
import PPrinter.Expression
import Prettyprinter
import AST.Seman
import Semantic.Monad


ppInitializeVectorFromExpression ::
  Integer ->
  DocStyle ->
  DocStyle ->
  TypeSpecifier ->
  DocStyle
ppInitializeVectorFromExpression level target source ts =
  let iterator = namefy $ pretty ("i" ++ show level)
   in case ts of
        -- \| If the initializer is a vector, we must iterate
        (Vector ts' (K size)) ->
          let initExpr =
                ppCForLoopInitExpression
                  (ppTypeSpecifier USize)
                  iterator
                  (pretty (show (0 :: Integer)))
              condExpr =
                  iterator <+> pretty "<" <+> pretty size
              incrExpr =
                  ppCForLoopIncrExpression
                  iterator
                  (pretty (show (1 :: Integer)))
              in ppCForLoop initExpr condExpr incrExpr (ppInitializeVectorFromExpression (level + 1) (target <> brackets iterator) (source <> brackets iterator) ts')
        _ -> target <+> pretty "=" <+> source <> semi

ppInitializeVector ::
  Substitutions ->
  -- | Current vector nesting level. This argument is used to
  -- generate the name of the iterator variable.
  Integer ->
  -- |  Name of the target object. This name will contain the previous
  --  indexing expressions as needed.
  DocStyle ->
  -- |  The initialization expression
  Expression SemanticAnns ->
  DocStyle
ppInitializeVector subs level target expr =
  let iterator = namefy $ pretty ("i" ++ show level)
   in case expr of
        (FieldAssignmentsExpression {}) -> ppInitializeStruct subs level target expr
        (OptionVariantExpression {}) -> ppInitializeOption subs level target expr
        (VectorInitExpression expr' (K size) _) ->
          let initExpr =
                ppCForLoopInitExpression
                  (ppTypeSpecifier USize)
                  iterator
                  (pretty (show (0 :: Integer)))
           in let condExpr =
                    iterator <+> pretty "<" <+> pretty size
               in let incrExpr =
                        ppCForLoopIncrExpression
                          iterator
                          (pretty (show (1 :: Integer)))
                   in ppCForLoop
                        initExpr
                        condExpr
                        incrExpr
                        ( ppInitializeVector subs (level + 1) (target <> brackets iterator) expr'
                        )
        (EnumVariantExpression {}) -> ppInitializeEnum subs level target expr
        (FunctionExpression identifier _ _) ->
          -- Here we are assuming that the "target" expression will always have a precedence greater than
          -- the precedence of the casting. This should be true, since the target will always be either
          -- a variable, a member access, or a vector index objet.
          ppCDereferenceExpression (parens
            (parens (ppReturnVectorValueStructure (pretty identifier) <+> pretty "*") <> target))
            <+> pretty "=" <+> 
            (if getExpPrecedence expr > 2 then
              ppCDereferenceExpression (parens (parens (ppReturnVectorValueStructure (pretty identifier) <+> pretty "*") <> parens (ppExpression subs expr)))
            else
              ppCDereferenceExpression (parens (parens (ppReturnVectorValueStructure (pretty identifier) <+> pretty "*") <> ppExpression subs expr))
            ) <> semi
        _ -> ppInitializeVectorFromExpression level target (ppExpression subs expr) (getType expr)

ppFieldInitializer :: Substitutions ->  Integer -> DocStyle -> DocStyle -> Expression SemanticAnns -> DocStyle
ppFieldInitializer subs level identifier field expr@(FieldAssignmentsExpression {}) =
  ppInitializeStruct subs level (identifier <> pretty "." <> field) expr
ppFieldInitializer subs level identifier field expr@(OptionVariantExpression {}) =
  ppInitializeOption subs level (identifier <> pretty "." <> field) expr
ppFieldInitializer subs level identifier field expr@(VectorInitExpression {}) =
  ppInitializeVector subs level (identifier <> pretty "." <> field) expr
ppFieldInitializer subs level identifier field expr@(EnumVariantExpression {}) =
  ppInitializeEnum subs level (identifier <> pretty "." <> field) expr
ppFieldInitializer subs level identifier field expr =
    let ts = getType expr in
    case ts of
        Vector _ _ -> ppInitializeVector subs level (identifier <> pretty "." <> field) expr
        _ -> identifier <> pretty "." <> field <+> pretty "=" <+> ppExpression subs expr <> semi

-- | This function initializes a struct using a field values assignments expression
-- as initializer. The name of sthe struct is passed as argument.
ppInitializeStruct ::
  Substitutions ->
  -- | Current nesting level. This argument is used to
  -- generate the name of the iterator variables in case we need to initialize
  -- a vector.
  Integer ->
  -- | Name of the struct
  DocStyle ->
  -- |  The initialization expression
  Expression SemanticAnns ->
  DocStyle
ppInitializeStruct subs level target expr =
  case expr of
    -- \| This function can only be called with a field values assignments expressions
    (FieldAssignmentsExpression _ vas _) ->
        vsep $
            foldr (
              \assignment acc ->
                case assignment of
                  FieldValueAssignment field expr' _ -> ppFieldInitializer subs level target (pretty field) expr' : acc
                  FieldAddressAssignment field addr (SemAnn _ (ETy (SimpleType ts))) -> 
                    target <> pretty "." <> pretty field <+> pretty "=" <+> parens (ppTypeSpecifier ts) <> pretty addr <> semi : acc
                  -- | Regular access port connection  
                  FieldPortConnection AccessPortConnection field resource (SemAnn _ (CTy (APConnTy rts procedures))) -> 
                    vsep (
                      target <> pretty "." <> pretty field <> pretty "." <> ppInterfaceThatField <+> pretty "=" <+> ppCReferenceExpression (pretty resource) <> semi
                      : (ppProcedureAssignment field rts <$> procedures)
                    ) : acc
                  -- | Pool port connections
                  FieldPortConnection AccessPortConnection field res _ -> 
                    target <> pretty "." <> pretty field <+> pretty "=" <+> ppCReferenceExpression (pretty res) <> semi : acc
                  _ -> acc
            ) [] vas
    _ -> error $ "Incorrect initialization expression: " ++ show expr
  where
    ppProcedureAssignment :: Identifier -> TypeSpecifier -> SemanProcedure -> DocStyle
    ppProcedureAssignment field res (SemanProcedure procid) =
      target <> pretty "." <> pretty field <> pretty "." <> pretty procid <+> pretty "=" <+> classFunctionName (ppTypeSpecifier res) (pretty procid) <> semi

ppInitializeEnum ::
  Substitutions ->
  Integer ->
  -- | Name of the enum
  DocStyle ->
  -- |  The initialization expression
  Expression SemanticAnns ->
  DocStyle
ppInitializeEnum subs level target expr =
  case expr of
    -- \| This function can only be called with a field values assignments expressions
    (EnumVariantExpression ts variant params _) ->
        vsep $
            (target <> pretty "." <> enumVariantsField <+> pretty "=" <+> (pretty ts <::> pretty variant) <> semi) : zipWith
            (\e index -> ppFieldInitializer subs level target (pretty variant <> pretty "." <> namefy (pretty (show (index :: Integer)))) e) params [0..]
    _ -> error "Incorrect expression"

ppInitializeOption ::
    Substitutions ->
    Integer ->
    -- | Name of the option
    DocStyle ->
    -- |  The initialization expression
    Expression SemanticAnns ->
    DocStyle
ppInitializeOption subs level target expr =
    case expr of
        (OptionVariantExpression (Some e) _) ->
            vsep [target <> pretty "." <> enumVariantsField <+> pretty "=" <+> optionSomeVariant <> semi,
                ppFieldInitializer subs level target optionSomeField e]
        (OptionVariantExpression None _) ->
            target <> pretty "." <> enumVariantsField <+> pretty "=" <+> optionNoneVariant <> semi
        _ -> error "Incorrect expression"
