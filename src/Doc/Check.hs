-- | The checks of the documentation comments that need the semantic
-- information. Each comment is checked against the signature of its own
-- element; what a procedure inherits from its interface is checked on the
-- interface.
module Doc.Check (runDocCheck) where

import Data.Maybe (listToMaybe)
import Text.Parsec.Pos (incSourceColumn)

import Core.AST
import Doc.Errors
import Doc.Model
import Parser.Types (ParserAnn(..))
import Semantic.AST (AnnotatedProgram)
import Semantic.Types (SemanticAnn, sourceOf)
import Utils.Annotations

-- | The first documentation error of a typed module, if any.
runDocCheck :: AnnotatedProgram SemanticAnn -> Maybe DocCheckError
runDocCheck = listToMaybe . concatMap declaration

  where

    declaration (Function name ps _ _ _ ann) = params name ps ann
    declaration (ExternFunction name ps _ _ _ ann) = params name ps ann
    declaration (TypeDefinition (Class _ cls members _ _) _) = concatMap (member cls) members
    declaration (TypeDefinition (Interface _ iface _ procs _) _) =
      concat [ params (iface ++ "::" ++ name) ps ann | InterfaceProcedure _ name ps _ ann <- procs ]
    declaration _ = []

    member cls m = case m of
      ClassMethod _ name ps _ _ ann -> params (qualified name) ps ann
      ClassProcedure _ name ps _ ann -> params (qualified name) ps ann
      ClassViewer name ps _ _ ann -> params (qualified name) ps ann
      ClassAction _ name p _ ann -> params (qualified name) (maybe [] pure p) ann
      ClassCase _ name ps _ ann -> params (qualified name) ps ann
      ClassField _ -> []

      where

        qualified name = cls ++ "::" ++ name

-- | The @\@param@ tags of the comment of an element that name no parameter of
-- its signature.
params :: Identifier -> [Parameter' ty SemanticAnn] -> SemanticAnn -> [DocCheckError]
params name ps ann =
  case sourceOf ann of
    ParserAnn _ (Just doc) | Right model <- docModel doc ->
      [ annotateError (tagLocation doc tag) (EUnknownDocParam name param names)
      | tag@(DocTag _ (TagParam _ param) _) <- docTags model, param `notElem` names ]
    _ -> []

  where

    names = map paramIdentifier ps

-- | The location of a @\@param@ tag, from its @\@@ to the end of its name.
tagLocation :: DocComment -> DocTag a -> Location
tagLocation doc (DocTag pos kind _) =
  case docLocation doc of
    Position qname _ _ -> Position qname pos (incSourceColumn pos (tagLength kind))
    other -> other

  where

    tagLength (TagParam ParamOut param) = length ("@param[out] " ++ param)
    tagLength (TagParam ParamPlain param) = length ("@param " ++ param)
    tagLength _ = 1
