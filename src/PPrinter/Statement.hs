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
            braces' $ (indentTab . align) $ initializer identifier expr
        ]

-- | Prints the statements of a match case.
-- Within the match case, we have to substitute the symbols of the
-- parameters of the match case. Each symbol must be substituted by the
-- corresponding field of the enumeration variant's struct.
ppMatchCase :: Substitutions -> DocStyle -> MatchCase SemanticAnns -> DocStyle
ppMatchCase subs symbol (MatchCase identifier params body _) =
    vsep [
        ppStatement newSubs s <> line | s <- body
    ]
    where
        -- | New map of substitutions. This map contains the substitutions
        -- of the parameters of the match case. It maps each parameter
        -- identifier to the corresponding field of the enumeration variant's
        -- struct.
        newKeyVals = zipWith
            (\sym index -> (sym, symbol <> pretty "." <> pretty (namefy identifier) <> pretty "." <> pretty (namefy (show (index :: Integer))))) params [0..]
        -- | Union of the previous map with the new one. This allows the nesting
        -- of match statements.
        newSubs = subs `union` fromList newKeyVals

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
                _ -> braces' $ (indentTab . align) $ ppInitializeVector subs 0 (
                    -- | If we are here, it means that we will be assigning the array to another array 
                    -- using a for loop. Since we are going to use a vector index expression on the assignments
                    -- we need to first check that the precedence of the target object is greater than 1.
                    -- If not, we must surround it in parenthesis.
                        if getObjPrecedence obj > 1 then 
                            parens (ppObject subs obj)
                        else
                            ppObject subs obj
                    ) expr
        _ -> case expr of
            (FieldAssignmentsExpression {}) ->
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
            -- | If the expression is a variable, we can use it directly
            (AccessObject (Variable {})) ->
                let symbol = ppExpression subs expr in
                    ppMatchCases symbol matchCases
            -- | If the expression is a complex expression, we have to evaluate it first
            -- and then use the result of the evaluation when evaluating the different
            -- if-elseif clauses of the match statement.
            _ ->
                let symbol = "__match" in
                    braces' $ (indentTab . align) $ vsep [
                        ppStatement subs (Declaration symbol Immutable (getType expr) expr undefined),
                        emptyDoc,
                        ppMatchCases (pretty symbol) matchCases
                    ]
ppStatement subs (SingleExpStmt expr _) =
    ppExpression subs expr <> semi
