module PPrinter.Statement where

import PPrinter.Common
import Prettyprinter
import SemanAST
import Semantic.Monad
import Data.Map (union, fromList, empty)
import PPrinter.Expression
import PPrinter.Statement.VariableInitialization


ppDeclareAndInitialize ::
    (DocStyle -> Expression SemanticAnns -> DocStyle)
    -> DocStyle
    -> TypeSpecifier
    -> Expression SemanticAnns -> DocStyle
ppDeclareAndInitialize initializer identifier ts expr =
    vsep
        [
            ppTypeSpecifier ts <+> identifier <> ppDimension ts <> semi,
            emptyDoc,
            braces' $ (indentTab . align) $ initializer identifier expr
        ]

ppMatchCase :: Substitutions -> DocStyle -> MatchCase SemanticAnns -> DocStyle
ppMatchCase subs symbol (MatchCase identifier params body _) =
    let newKeyVals = zipWith
            (\sym index -> (sym, symbol <> pretty "." <> pretty (namefy identifier) <> pretty "." <> pretty (namefy (show (index :: Integer))))) params [0..]
        newSubs = union subs (fromList newKeyVals)
    in
        vsep [ppStatement newSubs s <> line | s <- body]

ppReturnStmt :: DocStyle -> ReturnStmt SemanticAnns -> DocStyle
ppReturnStmt identifier (ReturnStmt (Just expr) _) =
    case getType expr of
        (Vector {}) -> returnC <+> 
            ppCDereferenceExpression (parens (ppReturnVectorValueStructure identifier <+> pretty "*") <> ppExpression empty expr) <> semi
        _ -> returnC <+> ppExpression empty expr <> semi
ppReturnStmt _ (ReturnStmt Nothing _) = returnC <> semi

ppStatement :: Substitutions -> Statement SemanticAnns -> DocStyle
ppStatement subs (Free obj _) =
    ppCFunctionCall poolFree [ppObject subs obj] <> semi
ppStatement subs (Declaration identifier ts expr _) =
  case ts of
    Vector _ _ -> 
        ppDeclareAndInitialize (ppInitializeVector subs 0) (pretty identifier) ts expr
    _ -> case expr of
        (FieldValuesAssignmentsExpression {}) ->
            ppDeclareAndInitialize (ppInitializeStruct subs 0) (pretty identifier) ts expr
        (OptionVariantExpression {}) ->
            ppDeclareAndInitialize (ppInitializeOption subs 0) (pretty identifier) ts expr
        (EnumVariantExpression {}) ->
            ppDeclareAndInitialize (ppInitializeEnum subs 0) (pretty identifier) ts expr
        _ -> ppTypeSpecifier ts <+> pretty identifier <+> pretty "=" <+> ppExpression subs expr <> semi
ppStatement subs (AssignmentStmt obj expr  _) =
    let ts = getObjectType obj in
    case ts of
        Vector _ _ ->
            case expr of 
                (FunctionExpression identifier _ _) -> 
                    -- Here we are assuming that the target object will always have a precedence greater than
                    -- the precedence of the casting. This should be true, since the target will always be either
                    -- a variable, a member access, or a vector index objet.
                    ppCDereferenceExpression
                            (parens (ppReturnVectorValueStructure (pretty identifier) <+> pretty "*") <> ppObject subs obj)
                    <+> pretty "=" <+> 
                    -- Here we are also assuming that the function call will alwayys have a precendence greater than
                    -- the casting.
                    ppCDereferenceExpression
                            (parens (ppReturnVectorValueStructure (pretty identifier) <+> pretty "*") <> ppExpression subs expr)
                    <> semi
                _ -> braces' $ (indentTab . align) $ ppInitializeVector subs 0 (ppObject subs obj) expr
        _ -> case expr of
            (FieldValuesAssignmentsExpression {}) ->
                braces' $ (indentTab . align) $ ppInitializeStruct subs 0 (ppObject subs obj) expr
            (OptionVariantExpression {}) ->
                braces' $ (indentTab . align) $ ppInitializeOption subs 0 (ppObject subs obj) expr
            (EnumVariantExpression {}) ->
                braces' $ (indentTab . align) $ ppInitializeEnum subs 0 (ppObject subs obj) expr
            _ -> ppObject subs obj <+> pretty "=" <+> ppExpression subs expr <> semi
-- | Print if-else-if statement
ppStatement subs (IfElseStmt cond ifBody elifs elseBody _) =
    ppCIfBlock (ppExpression subs cond) (vsep [ppStatement subs s <> line | s <- ifBody])
        <> foldr (\(ElseIf c b _) acc -> acc <+> ppCElseIfBlock (ppExpression subs c) (vsep [ppStatement subs s <> line | s <- b])) emptyDoc elifs
        <> if null elseBody then emptyDoc else space <> ppCElseBlock (vsep [ppStatement subs s <> line | s <- elseBody])
ppStatement subs (ForLoopStmt iterator initValue endValue breakCond body _ ) =
    let startSymbol = "__start"
        endSymbol = "__end"
        iteratorTS = getType initValue
        initExpr = ppCForLoopInitExpression
            (ppTypeSpecifier iteratorTS)
            (pretty iterator)
            (pretty startSymbol)
        condExpr = pretty iterator <+> pretty "<" <+> pretty endSymbol <>
            maybe emptyDoc (\e -> emptyDoc <+> pretty "&&" <+> parens (ppExpression subs e)) breakCond
        incrExpr = ppCForLoopIncrExpression
            (pretty iterator)
            (parens (ppTypeSpecifier iteratorTS) <> pretty (show (1 :: Integer)))
    in
    braces' $ (indentTab . align) $ vsep [
        ppStatement subs (Declaration startSymbol iteratorTS initValue undefined),
        ppStatement subs (Declaration endSymbol iteratorTS endValue undefined),
        emptyDoc,
        ppCForLoop initExpr condExpr incrExpr (line <> vsep [ppStatement subs s <> line | s <- body])
    ]
-- | Print a match in which the expression is a
-- single variable (i.e., we do not need to evaluate it)
ppStatement subs (MatchStmt expr matchCases _) =
    let ppMatchCaseOthers symbol cls =
            case cls of
                [c] -> space <> ppCElseBlock (ppMatchCase subs symbol c)
                c : cs -> 
                    case c of (MatchCase identifier _ _ _) -> 
                                space <> ppCElseIfBlock 
                                    (symbol <> pretty "." <> enumVariantsField <+> pretty "==" <+> pretty identifier) 
                                    (ppMatchCase subs symbol c) <> ppMatchCaseOthers symbol cs
                [] -> emptyDoc
        ppMatchCases symbol cls =
            case cls of 
                -- | Single case, we do not need to evaluate it, just print the case
                [c] -> ppMatchCase subs symbol c
                c : cs -> 
                    case c of (MatchCase identifier _ _ _) -> 
                                ppCIfBlock 
                                    (symbol <> pretty "." <> enumVariantsField <+> pretty "==" <+> pretty identifier) 
                                    (ppMatchCase subs symbol c) <> ppMatchCaseOthers symbol cs
                [] -> error "empty case list!"
    in
        case expr of
            (AccessObject (Variable {})) ->
                let symbol = ppExpression subs expr in
                    ppMatchCases symbol matchCases
            _ ->
                let symbol = "__match" in
                    braces' $ (indentTab . align) $ vsep [
                        ppStatement subs (Declaration symbol (getType expr) expr undefined),
                        emptyDoc,
                        ppMatchCases (pretty symbol) matchCases
                    ]
ppStatement subs (SingleExpStmt expr _) =
    ppExpression subs expr <> semi
