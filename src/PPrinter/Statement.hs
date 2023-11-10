module PPrinter.Statement where

import PPrinter.Common
import Prettyprinter
import AST.Seman
import Semantic.Monad
import Data.Map (union, fromList, empty)
import PPrinter.Expression
import PPrinter.Statement.VariableInitialization

-- | Prints the declation and initialization statements of a complex object.
-- This includes the initialization of structs, options, and enums.
ppDeclareAndInitialize ::
    -- | Initializer function. This function will be called to initialize the object.
    -- The function may print one or more statements that will be included, between braces,
    -- after the declaration of the objet. The function receives the initialization expression
    -- as an argument and returns the statements that will be printed.
    (DocStyle -> Expression SemanticAnns -> DocStyle)
    -- | Identifier of the object
    -> DocStyle
    -- | Type specifier of the object
    -> TypeSpecifier
    -- | Initialization expression
    -> Expression SemanticAnns -> DocStyle
ppDeclareAndInitialize initializer identifier ts expr =
    vsep
        [
            -- | Declaration of the object
            ppTypeSpecifier ts <+> identifier <> ppDimension ts <> semi,
            -- | Empty line
            emptyDoc,
            -- | Initialization of the object between braces
            initializer identifier expr
        ]

data MatchSource = Annonymous | Named DocStyle

-- | Prints the statements of a match case.
-- Within the match case, we have to substitute the symbols of the
-- parameters of the match case. Each symbol must be substituted by the
-- corresponding field of the enumeration variant's struct.
ppMatchCase :: Substitutions -> MatchSource -> TypeSpecifier -> MatchCase SemanticAnns -> DocStyle
ppMatchCase subs _ (Option {}) (MatchCase "None" _ body _) =
    vsep [ ppStatement subs s <> line | s <- body ]
ppMatchCase subs Annonymous (Option {}) (MatchCase "Some" [sym] body _) =
    let variable = namefy (pretty "match") <> pretty "." <> optionSomeField
        -- | New map of substitutions. This map contains the substitutions
        -- of the parameters of the match case. It maps each parameter
        -- identifier to the corresponding field of the enumeration variant's
        -- struct.
        newSubs = subs `union` fromList [(sym, variable)]
    in
    vsep $ [ ppStatement newSubs s <> line | s <- body ]
ppMatchCase subs (Named obj) (Option {}) (MatchCase "Some" [sym] body _) =
    let variable = namefy $ obj <::> optionSomeVariant
        -- | New map of substitutions. This map contains the substitutions
        -- of the parameters of the match case. It maps each parameter
        -- identifier to the corresponding field of the enumeration variant's
        -- struct.
        newSubs = subs `union` fromList [(sym, variable)]
    in
    vsep $
    [
        optionDyn <+> variable <+> pretty "=" <+> obj <> pretty "." <> optionSomeField <> semi,
        emptyDoc
    ] ++
    [
        ppStatement newSubs s <> line | s <- body
    ]
ppMatchCase subs _ (DefinedType _) (MatchCase _ [] body _) =
    vsep $ [ ppStatement subs s <> line | s <- body ]
ppMatchCase subs Annonymous (DefinedType _) (MatchCase variant params body _) =
    let variable = namefy $ pretty ("match." ++ variant)
        -- | New map of substitutions. This map contains the substitutions
        -- of the parameters of the match case. It maps each parameter
        -- identifier to the corresponding field of the enumeration variant's
        -- struct.
        newKeyVals = zipWith
            (\sym index -> (sym, variable <> pretty "." <> namefy (pretty (show (index :: Integer))))) params [0..]
        -- | Union of the previous map with the new one. This allows the nesting
        -- of match statements.
        newSubs = subs `union` fromList newKeyVals
    in
    vsep $ [ ppStatement newSubs s <> line | s <- body ]
ppMatchCase subs (Named obj) (DefinedType identifier) (MatchCase variant params body _) =
    let variable = namefy $ obj <::> pretty variant
        -- | New map of substitutions. This map contains the substitutions
        -- of the parameters of the match case. It maps each parameter
        -- identifier to the corresponding field of the enumeration variant's
        -- struct.
        newKeyVals = zipWith
            (\sym index -> (sym, variable <> pretty "." <> namefy (pretty (show (index :: Integer))))) params [0..]
        -- | Union of the previous map with the new one. This allows the nesting
        -- of match statements.
        newSubs = subs `union` fromList newKeyVals
    in
    vsep $
    [
        enumIdentifier (pretty identifier <::> pretty variant <> pretty "_params") <+>
            variable <+> pretty "=" <+> obj <> pretty "." <> pretty variant <> semi,
        emptyDoc
    ] ++
    [
        ppStatement newSubs s <> line | s <- body
    ]
ppMatchCase _ _ _ mc = error $ "invalid match case: " ++ show mc

-- | Prints a return statement
ppReturnStmt :: 
    -- | Identifier of the function. This parameter is used in case the return
    -- value is an array. In that case, the value is casted to a struct so that
    -- it can be returned by value.
    DocStyle
    -- | Return statement.
    -> ReturnStmt SemanticAnns -> DocStyle
-- | If the statements returns a value, then we must check its type
ppReturnStmt identifier (ReturnStmt (Just expr) _) =
    case getType expr of
        -- | If the return type is an array, then we must cast it to a struct
        -- so that it can be returned by value.
        (Vector {}) -> returnC <+>
            ppCDereferenceExpression (parens (
                parens (ppReturnVectorValueStructure identifier <+> pretty "*") <> ppExpression empty expr
            )) <> semi
        -- | If the value is of a type different from a vector, then it can be returned
        -- directly using a C return statement.
        _ -> returnC <+> ppExpression empty expr <> semi
-- If the statement does not return a value, then we can use an empty C return statement.
ppReturnStmt _ (ReturnStmt Nothing _) = returnC <> semi

-- | Prints a statement
ppStatement ::
    -- | Map of substitutions. This map contains the substitutions
    -- that must be made of the symbols or identifiers of the statement.
    -- This map is used to substitute the symbols of the parameters of
    -- a match case and also the symbols of the parameters of a function
    -- when they are arrays.
    Substitutions 
    -- | Statement to be printed
    -> Statement SemanticAnns -> DocStyle
-- | Prints a free statement. These statements are used to free a
-- dynamically allocated object. They are of the form "free(obj)".
-- The printer prints the corresponding free() function call.
ppStatement subs (Free obj _) =
    ppCFunctionCall poolFree [ppObject subs obj] <> semi
ppStatement subs (Declaration identifier _ ts expr _) =
  case ts of
    Vector _ _ ->
        ppDeclareAndInitialize (ppInitializeVector subs 0) (pretty identifier) ts expr
    _ -> case expr of
        (FieldAssignmentsExpression {}) ->
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
                    ppCDereferenceExpression (parens
                            (parens (ppReturnVectorValueStructure (pretty identifier) <+> pretty "*") <> ppObject subs obj))
                    <+> pretty "=" <+>
                    -- Here we are also assuming that the function call will alwayys have a precendence greater than
                    -- the casting.
                    ppCDereferenceExpression (parens
                            (parens (ppReturnVectorValueStructure (pretty identifier) <+> pretty "*") <> ppExpression subs expr))
                    <> semi
                _ -> ppInitializeVector subs 0 (
                    -- | If we are here, it means that we will be assigning the array to another array 
                    -- using a for loop. Since we are going to use a vector index expression on the assignments
                    -- we need to first check that the precedence of the target object is greater than 1.
                    -- If not, we must surround it in parenthesis.
                        if getObjPrecedence obj > 1 then 
                            parens (ppObject subs obj)
                        else
                            ppObject subs obj
                    ) expr
        (Location _) -> 
            if getObjPrecedence obj > 2 then
                ppCDereferenceExpression $ parens (ppObject subs obj)
            else
                ppCDereferenceExpression (ppObject subs obj)
        _ -> case expr of
            (FieldAssignmentsExpression {}) ->
                ppInitializeStruct subs 0 (ppObject subs obj) expr
            (OptionVariantExpression {}) ->
                ppInitializeOption subs 0 (ppObject subs obj) expr
            (EnumVariantExpression {}) ->
                ppInitializeEnum subs 0 (ppObject subs obj) expr
            _ -> ppObject subs obj <+> pretty "=" <+> ppExpression subs expr <> semi
-- | Print if-else-if statement
ppStatement subs (IfElseStmt cond ifBody elifs elseBody _) =
    ppCIfBlock (ppExpression subs cond) (vsep [ppStatement subs s <> line | s <- ifBody])
        <> foldr (\(ElseIf c b _) acc -> acc <+> ppCElseIfBlock (ppExpression subs c) (vsep [ppStatement subs s <> line | s <- b])) emptyDoc elifs
        <> if null elseBody then emptyDoc else space <> ppCElseBlock (vsep [ppStatement subs s <> line | s <- elseBody])
ppStatement subs (ForLoopStmt iterator iteratorTS initValue endValue breakCond body _ ) =
    braces' $ (indentTab . align) $ vsep [
        ppStatement subs (Declaration startSymbol Immutable iteratorTS initValue undefined),
        ppStatement subs (Declaration endSymbol Immutable iteratorTS endValue undefined),
        emptyDoc,
        ppCForLoop initExpr condExpr incrExpr (line <> vsep [ppStatement subs s <> line | s <- body])
    ]
    where
        startSymbol = "__start"
        endSymbol = "__end"
        initExpr = ppCForLoopInitExpression
            (ppTypeSpecifier iteratorTS)
            (pretty iterator)
            (pretty startSymbol)
        condExpr = pretty iterator <+> pretty "<" <+> pretty endSymbol <>
            maybe emptyDoc (\e -> emptyDoc <+> pretty "&&" <+> parens (ppExpression subs e)) breakCond
        incrExpr = ppCForLoopIncrExpression
            (pretty iterator)
            (pretty (show (1 :: Integer)))
ppStatement subs (MatchStmt expr matchCases _) =
    let matchSource = case expr of
            (AccessObject {}) -> Named (ppExpression subs expr)
            _ -> Annonymous
        symbol = case matchSource of
            Annonymous -> namefy $ pretty "match"
            Named obj -> obj
        ts = getType expr
        casePrefix = 
            case ts of
                -- | If the expression is an enumeration, the case identifier must 
                -- be prefixed with the enumeration identifier.
                (DefinedType enumId) -> (<::>) (pretty enumId) . pretty
                _ -> pretty
        ppMatchCaseOthers cls =
            case cls of
                [c] -> space <> ppCElseBlock (ppMatchCase subs matchSource ts c)
                c : cs ->
                    case c of (MatchCase caseId _ _ _) ->
                                space <> ppCElseIfBlock
                                    (symbol <> pretty "." <> enumVariantsField <+> pretty "==" <+> casePrefix caseId)
                                    (ppMatchCase subs matchSource ts c) <> ppMatchCaseOthers cs
                [] -> emptyDoc
        ppMatchCases cls =
            case cls of
                -- | Single case, we do not need to evaluate it, just print the case
                [c] -> ppMatchCase subs matchSource ts c
                c : cs ->
                    case c of (MatchCase caseId _ _ _) ->
                                ppCIfBlock
                                    (symbol <> pretty "." <> enumVariantsField <+> pretty "==" <+> casePrefix caseId)
                                    (ppMatchCase subs matchSource ts c) <> ppMatchCaseOthers cs
                [] -> error "empty case list!"
    in
        case expr of
            -- | If the expression is an object, we can use it directly
            (AccessObject {}) ->
                ppMatchCases matchCases
            -- | If the expression is a complex expression, we have to evaluate it first
            -- and then use the result of the evaluation when evaluating the different
            -- if-elseif clauses of the match statement.
            _ ->
                braces' $ (indentTab . align) $ vsep [
                    ppStatement subs (Declaration "__match" Immutable (getType expr) expr undefined),
                    emptyDoc,
                    ppMatchCases matchCases
                ]
ppStatement subs (SingleExpStmt expr _) =
    ppExpression subs expr <> semi

ppBlockRet :: Substitutions -> DocStyle -> BlockRet SemanticAnns -> DocStyle
ppBlockRet _ identifier (BlockRet [] ret) =
  braces' (line <> 
    (indentTab . align $ ppReturnStmt identifier ret <> line)
  ) <> line
ppBlockRet subs identifier (BlockRet body ret) =
  braces' (line <> 
    (indentTab . align $
      vsep [ppStatement subs s <> line | s <- body]
      <> line <> ppReturnStmt identifier ret <> line)
  ) <> line

ppParameterSubstitutions :: [Parameter] -> Substitutions
ppParameterSubstitutions parameters =
  fromList [(pid, pretty pid <> pretty ".array") | (Parameter pid (Vector {})) <- parameters]