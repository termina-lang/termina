module PPrinter.Global where

import Prettyprinter

import AST.Seman
import PPrinter.Common
import Semantic.Monad (SemanticAnns)

ppGlobalDeclaration :: Global SemanticAnns -> DocStyle
ppGlobalDeclaration (Resource identifier ts@(DefinedType {}) _ _ _) = externC <+> ppTypeSpecifier ts <+> pretty identifier <> semi <> line
ppGlobalDeclaration (Resource identifier (Pool {}) _ _ _) = vsep
    [
        externC <+> pool <+> pretty identifier <> semi,
        emptyDoc
    ]
ppGlobalDeclaration (Channel identifier (MsgQueue {}) _ _ _) = externC <+> msgQueue <+> pretty identifier <> semi <> line
ppGlobalDeclaration (Task identifier ts _ _ _) = externC <+> ppTypeSpecifier ts <+> pretty identifier <> ppDimension ts <> semi <> line
ppGlobalDeclaration (Handler identifier ts _ _ _) = externC <+> ppTypeSpecifier ts <+> pretty identifier <> ppDimension ts <> semi <> line
ppGlobalDeclaration (Emitter identifier (DefinedType "PeriodicTimer") _ _ _) = externC <+> periodicTimer <+> pretty identifier <> semi <> line
ppGlobalDeclaration decl = error $ "unsupported global declaration: " ++ show decl

ppGlobalDefinition :: Global SemanticAnns -> DocStyle
ppGlobalDefinition (Resource identifier ts@(DefinedType {}) _ _ _) = ppTypeSpecifier ts <+> pretty identifier <> semi <> line
ppGlobalDefinition (Resource identifier (Pool {}) _ _ _) = vsep
    [
        pool <+> pretty identifier <> semi,
        emptyDoc
    ]
ppGlobalDefinition (Channel identifier (MsgQueue {}) _ _ _) = msgQueue <+> pretty identifier <> semi <> line
ppGlobalDefinition (Task identifier ts _ _ _) = ppTypeSpecifier ts <+> pretty identifier <> ppDimension ts <> semi <> line
ppGlobalDefinition (Handler identifier ts _ _ _) = ppTypeSpecifier ts <+> pretty identifier <> ppDimension ts <> semi <> line
ppGlobalDefinition (Emitter identifier (DefinedType "PeriodicTimer") _ _ _) = periodicTimer <+> pretty identifier <> semi <> line
ppGlobalDefinition decl = error $ "unsupported global declaration: " ++ show decl
