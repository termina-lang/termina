module PPrinter.Global where

import Prettyprinter

import AST.Seman
import PPrinter.Common
import Semantic.Monad (SemanticAnns)

ppGlobalDeclaration :: Global SemanticAnns -> DocStyle
ppGlobalDeclaration (Resource identifier ts@(DefinedType {}) _ _ _) = externC <+> ppTypeSpecifier ts <+> pretty identifier <> semi <> line
ppGlobalDeclaration (Resource identifier (Pool ts (K size)) _ _ _) = vsep
    [
        externC <+> ppTypeSpecifier ts <+> poolMemoryArea (pretty identifier) <> brackets (pretty (show size)) <> semi,
        externC <+> pool <+> pretty identifier <> semi,
        emptyDoc
    ]
ppGlobalDeclaration (Resource identifier (MsgQueue {}) _ _ _) = externC <+> msgQueue <+> pretty identifier <> semi <> line
ppGlobalDeclaration (Task identifier ts _ _ _) = externC <+> ppTypeSpecifier ts <+> pretty identifier <> ppDimension ts <> semi <> line
ppGlobalDeclaration (Handler identifier ts _ _ _) = externC <+> ppTypeSpecifier ts <+> pretty identifier <> ppDimension ts <> semi <> line
ppGlobalDeclaration decl = error $ "unsupported global declaration: " ++ show decl

ppGlobalDefinition :: Global SemanticAnns -> DocStyle
ppGlobalDefinition (Resource identifier ts@(DefinedType {}) _ _ _) = ppTypeSpecifier ts <+> pretty identifier <> semi <> line
ppGlobalDefinition (Resource identifier (Pool ts (K size)) _ _ _) = vsep
    [
        ppTypeSpecifier ts <+> poolMemoryArea (pretty identifier) <> brackets (pretty (show size)) <> semi,
        pool <+> pretty identifier <> semi,
        emptyDoc
    ]
ppGlobalDefinition (Resource identifier (MsgQueue {}) _ _ _) = msgQueue <+> pretty identifier <> semi <> line
ppGlobalDefinition (Task identifier ts _ _ _) = ppTypeSpecifier ts <+> pretty identifier <> ppDimension ts <> semi <> line
ppGlobalDefinition (Handler identifier ts _ _ _) = ppTypeSpecifier ts <+> pretty identifier <> ppDimension ts <> semi <> line
ppGlobalDefinition decl = error $ "unsupported global declaration: " ++ show decl
