module PPrinter.Global where

import Prettyprinter

import SemanAST
import PPrinter.Common
import Semantic.Monad (SemanticAnns)

ppGlobalDeclaration :: Global SemanticAnns -> DocStyle
ppGlobalDeclaration (Shared identifier ts _ _ _) = externC <+> ppTypeSpecifier ts <+> pretty identifier <> ppDimension ts <> semi <> line
ppGlobalDeclaration decl = error $ "unsupported global declaration: " ++ show decl