module PPrinter.Application.Initialization where

import Semantic.Monad (SemanticAnns)
import AST.Seman
import PPrinter.Common
import Prettyprinter
import qualified Data.Map as M
import PPrinter.Statement.VariableInitialization
import Modules.Modules
import Modules.Printing
import qualified AST.Seman as SAST


ppInitializeObj :: Global SemanticAnns -> DocStyle
-- Print only the elements that are not nothing
ppInitializeObj (Resource identifier _ (Just expr) _ _) =
    ppInitializeStruct M.empty 0 (pretty identifier) expr
ppInitializeObj (Task identifier _ (Just expr) _ _) =
    ppInitializeStruct M.empty 0 (pretty identifier) expr
ppInitializeObj (Handler identifier _ (Just expr) _ _) =
    ppInitializeStruct M.empty 0 (pretty identifier) expr
ppInitializeObj _ = emptyDoc

ppInitFile :: [(ModuleName, ModuleMode, SAST.AnnotatedProgram SemanticAnns)] -> DocStyle
ppInitFile prjprogs =
    -- | Set of the first elements of the globals
    vsep $ [
        pretty "#include <termina.h>"
    ] ++
    [includes incs] ++
    [
        ppCFunctionPrototype (namefy $ pretty "termina_app" <::> pretty "init_globals") [] Nothing <+>
        braces' (line <>
            (indentTab . align $
                vsep (
                    concatMap (map ppInitializeObj) [objs | (_, _, objs) <- globals]
                )
            ) <> line
        ) <> line
    ]
    where
        globals = map (\(mn, mm, elems) -> (mn, mm, [g | (SAST.GlobalDeclaration g) <- elems])) prjprogs
        modsWithGlobals = filter (\(_, _, objs) -> not (null objs)) globals
        incs = map (\(nm, mm, _) -> (nm, mm)) modsWithGlobals
