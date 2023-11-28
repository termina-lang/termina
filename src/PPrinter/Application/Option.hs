module PPrinter.Application.Option where

import PPrinter.Common
import AST.Seman
import Prettyprinter
import qualified Data.Map as M
import qualified Data.Set as S
import Semantic.Option (OptionMap)

ppSimpleOptionDefinition :: TypeSpecifier -> DocStyle
ppSimpleOptionDefinition (Option ts) = vsep [
        ppOptionSomeParameterStruct ts,
        emptyDoc,
        ppOptionStruct ts,
        emptyDoc
    ]
ppSimpleOptionDefinition ts = error $ "invalid non-primitive type specifier: " ++ show ts

ppSimpleOptionTypesFile :: OptionMap -> DocStyle
ppSimpleOptionTypesFile opts =
    vsep $ [
        pretty "#ifndef __OPTION_H__",
        pretty "#define __OPTION_H__",
        emptyDoc,
        pretty "#include <termina.h>"
    ] ++
    concat ((map (\s -> emptyDoc : (map ppSimpleOptionDefinition (S.toList s))) $ M.elems opts)) ++
    [
        pretty "#endif // __OPTION_H__"
    ]
