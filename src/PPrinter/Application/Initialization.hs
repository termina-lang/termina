module PPrinter.Application.Initialization where

import Semantic.Monad (SemanticAnns)
import AST.Seman
import PPrinter.Common
import Prettyprinter
import Data.Map (empty)
import PPrinter.Statement.VariableInitialization
import Modules.Modules
import Modules.Printing


ppInitializeObj :: Global SemanticAnns -> DocStyle
-- Print only the elements that are not nothing
ppInitializeObj (Resource identifier _ (Just expr) _ _) = 
    ppInitializeStruct empty 0 (pretty identifier) expr
ppInitializeObj (Task identifier _ (Just expr) _ _) = 
    ppInitializeStruct empty 0 (pretty identifier) expr
ppInitializeObj (Handler identifier _ (Just expr) _ _) = 
    ppInitializeStruct empty 0 (pretty identifier) expr
ppInitializeObj decl = error $ "unsupported global declaration: " ++ show decl

ppInitFile :: [(ModuleName, [Global SemanticAnns])] -> DocStyle
ppInitFile globals = 
    -- | Set of the first elements of the globals
    vsep $ [
        pretty "#include <termina.h>",
        emptyDoc
    ] ++
    map (\nm -> pretty "#include" <+> dquotes(ppModuleName nm <> pretty ".h")) (map fst glbs) ++ [emptyDoc] ++
    [
        ppCFunctionPrototype (pretty "__initialize_globals") [] Nothing <+> 
        braces' (line <>
            (indentTab . align $
                vsep (
                    concatMap (map (\o -> ppInitializeObj o)) [objs | (_, objs) <- globals]
                )
            ) <> line
        ) <> line
    ]
    where
        glbs = filter (\(_, objs) -> not (null objs)) globals