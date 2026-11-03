{-# LANGUAGE OverloadedStrings #-}
module LSP.Utils  where

import LSP.Monad
import Data.Text
import qualified Language.LSP.Protocol.Types as LSP
import qualified Data.Text as T
import Utils.Annotations
import qualified Data.Map.Strict as M
import Utils.Errors (diagnosticSource)
import Text.Parsec
import qualified Language.LSP.Server as LSP
import qualified Language.LSP.Diagnostics as LSP
import qualified Data.SortedList as SL
import Parser.AST
import qualified Semantic.AST as SAST
import Semantic.Types

emitDiagnostics :: QualifiedName -> [LSP.Diagnostic] -> HandlerM ()
emitDiagnostics filePath mdiag = do
    let maxDiags = 100
        version = Just 0
        normUri = LSP.toNormalizedUri . LSP.filePathToUri $ filePath
    case mdiag of
      [] -> LSP.publishDiagnostics maxDiags normUri version
              (M.singleton diagnosticSource (SL.toSortedList []))
      diags -> LSP.publishDiagnostics maxDiags normUri version (LSP.partitionBySource diags)

-- | The symbols of a module, each class and struct holding its own members, so
-- the outline of the editor and its breadcrumb read as the source is written.
-- The flat SymbolInformation the protocol started with is deprecated.
getDocumentSymbols :: SAST.AnnotatedProgram SemanticAnn -> [LSP.DocumentSymbol]
getDocumentSymbols = Prelude.concatMap toDocumentSymbol

  where

    getRange :: Location -> Maybe LSP.Range
    getRange (Position _ sourceStart sourceEnd) =
      let startLine = fromIntegral $ sourceLine sourceStart - 1
          startCol = fromIntegral $ sourceColumn sourceStart - 1
          endLine = fromIntegral $ sourceLine sourceEnd - 1
          endCol = fromIntegral $ sourceColumn sourceEnd - 1
      in  Just $ LSP.Range (LSP.Position startLine startCol) (LSP.Position endLine endCol)
    getRange _ = Nothing

    -- | A symbol with its children, which the editor nests under it.
    symbol :: Text -> LSP.SymbolKind -> Location -> [LSP.DocumentSymbol] -> [LSP.DocumentSymbol]
    symbol name kind loc children =
      case getRange loc of
        Just range@(LSP.Range from _) ->
          [LSP.DocumentSymbol name Nothing kind Nothing Nothing range (LSP.Range from from)
            (if Prelude.null children then Nothing else Just children)]
        Nothing -> []

    toFieldSymbol :: SAST.FieldDefinition SemanticAnn -> [LSP.DocumentSymbol]
    toFieldSymbol (SAST.FieldDefinition name _ ann) =
      symbol (T.pack name) LSP.SymbolKind_Field (getLocation ann) []

    toClassMemberSymbol :: SAST.ClassMember SemanticAnn -> [LSP.DocumentSymbol]
    toClassMemberSymbol (SAST.ClassField fieldDef) = toFieldSymbol fieldDef
    toClassMemberSymbol (SAST.ClassMethod _ name _ _ _ ann) =
      symbol (T.pack name) LSP.SymbolKind_Method (getLocation ann) []
    toClassMemberSymbol (SAST.ClassProcedure _ name _ _ ann) =
      symbol (T.pack name) LSP.SymbolKind_Method (getLocation ann) []
    toClassMemberSymbol (SAST.ClassViewer name _ _ _ ann) =
      symbol (T.pack name) LSP.SymbolKind_Method (getLocation ann) []
    toClassMemberSymbol (SAST.ClassAction _ name _ _ ann) =
      symbol (T.pack name) LSP.SymbolKind_Method (getLocation ann) []
    toClassMemberSymbol (SAST.ClassCase _ name _ _ ann) =
      symbol (T.pack name) LSP.SymbolKind_Method (getLocation ann) []

    toInterfaceMemberSymbol :: SAST.InterfaceMember SemanticAnn -> [LSP.DocumentSymbol]
    toInterfaceMemberSymbol (SAST.InterfaceProcedure _ name _ _ ann) =
      symbol (T.pack name) LSP.SymbolKind_Method (getLocation ann) []

    toGlobalSymbol :: SAST.Global SemanticAnn -> [LSP.DocumentSymbol]
    toGlobalSymbol (SAST.Task name _ _ _ ann) = objectSymbol name ann
    toGlobalSymbol (SAST.Resource name _ _ _ ann) = objectSymbol name ann
    toGlobalSymbol (SAST.Handler name _ _ _ ann) = objectSymbol name ann
    toGlobalSymbol (SAST.Channel name _ _ _ ann) = objectSymbol name ann
    toGlobalSymbol (SAST.Emitter name _ _ _ ann) = objectSymbol name ann
    toGlobalSymbol (SAST.Test name _ _ _ ann) = objectSymbol name ann
    toGlobalSymbol (SAST.Const name _ _ _ ann) =
      symbol (T.pack name) LSP.SymbolKind_Constant (getLocation ann) []
    toGlobalSymbol (SAST.ConstExpr name _ _ _ ann) =
      symbol (T.pack name) LSP.SymbolKind_Constant (getLocation ann) []

    objectSymbol name ann = symbol (T.pack name) LSP.SymbolKind_Variable (getLocation ann) []

    toDocumentSymbol :: SAST.AnnASTElement SemanticAnn -> [LSP.DocumentSymbol]
    toDocumentSymbol (SAST.Function name _ _ _ _ ann) =
      symbol (T.pack name) LSP.SymbolKind_Function (getLocation ann) []
    toDocumentSymbol (SAST.ExternFunction name _ _ _ _ ann) =
      symbol (T.pack name) LSP.SymbolKind_Function (getLocation ann) []
    toDocumentSymbol (SAST.TypeDefinition (Class _ name members _ _) ann) =
      symbol (T.pack name) LSP.SymbolKind_Class (getLocation ann)
        (Prelude.concatMap toClassMemberSymbol members)
    toDocumentSymbol (SAST.TypeDefinition (Struct name fields _) ann) =
      symbol (T.pack name) LSP.SymbolKind_Struct (getLocation ann)
        (Prelude.concatMap toFieldSymbol fields)
    toDocumentSymbol (SAST.TypeDefinition (Enum name variants _) ann) =
      symbol (T.pack name) LSP.SymbolKind_Enum (getLocation ann)
        [ v | SAST.EnumVariant variantName _ <- variants
            , v <- symbol (T.pack variantName) LSP.SymbolKind_EnumMember (getLocation ann) [] ]
    toDocumentSymbol (SAST.TypeDefinition (Interface _ name _ members _) ann) =
      symbol (T.pack name) LSP.SymbolKind_Interface (getLocation ann)
        (Prelude.concatMap toInterfaceMemberSymbol members)
    toDocumentSymbol (SAST.GlobalDeclaration glb) = toGlobalSymbol glb
