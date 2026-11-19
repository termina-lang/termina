{-# LANGUAGE OverloadedStrings #-}
-- | The documentation comments of the C: the model of the comment of a
-- Termina element translated into the Doxygen comment of its C declaration.
module Generator.CodeGen.Doc
  ( genDoc
  , generatedDoc
  ) where

import Control.Monad.Except (throwError)
import Data.List (intercalate)
import qualified Data.Text as T

import Core.AST (AccessKind(..), Parameter'(..), TerminaType'(..))
import Doc.Model
import Generator.CodeGen.Common
import Generator.LanguageC.AST
import Parser.Types (ParserAnn(..))
import Semantic.AST (Parameter)
import Semantic.Types (SemanticAnn, sourceOf)

-- | The Doxygen comment of the C declaration of an element, from the comment
-- of the element, given the parameters of its signature, if it has any.
genDoc :: SemanticAnn -> [Parameter SemanticAnn] -> CGenerator (Maybe CDoc)
genDoc ann params =
    case sourceOf ann of
        ParserAnn _ (Just doc) -> case docModel doc of
            Right model -> return (Just (cDoc params model))
            Left _ -> throwError $ InternalError "A documentation comment with an error in its tags reached the generator"
        _ -> return Nothing

-- | A brief written by the generator, for what has no element of Termina
-- behind it: a text, a name as code and a text after it.
generatedDoc :: String -> String -> String -> CDoc
generatedDoc before name after = CDoc [DText (T.pack before), DCode (T.pack name), DText (T.pack after)] [] []

-- | The comment of a model as the comment of a C declaration. The tags of
-- the errors go together in a section, the code blocks in Termina are left
-- out, and the links name the C identifiers of the names of Termina.
cDoc :: [Parameter SemanticAnn] -> DocModel -> CDoc
cDoc params (DocModel summary body tags _) =
    CDoc (map (fmap cName) summary) (blocks body) (concatMap tag tags ++ errors)

    where

        blocks = map (fmap cName) . filter (not . terminaCode)

        terminaCode (DCodeBlock "termina" _) = True
        terminaCode _ = False

        tag (DocTag _ kind text) = case kind of
            TagParam mode name -> [CDocParam (direction mode name) (genParameterIdentifier name) (blocks text)]
            TagReturn -> [CDocReturn (blocks text)]
            TagPre -> [CDocPre (blocks text)]
            TagPost -> [CDocPost (blocks text)]
            TagInvariant -> [CDocInvariant (blocks text)]
            TagNote -> [CDocNote (blocks text)]
            TagWarning -> [CDocWarning (blocks text)]
            TagSee -> [CDocSee (blocks text)]
            TagDeprecated -> [CDocDeprecated (blocks text)]
            TagSince -> [CDocSince (blocks text)]
            TagRange -> [CDocSection "Range" (blocks text)]
            TagUnit -> [CDocSection "Unit" (blocks text)]
            -- The errors go in their own section, and the requirements and
            -- the examples are not printed yet.
            TagError _ -> []
            TagSatisfies _ -> []
            TagVerifies _ -> []
            TagExample -> []

        errors = case [ item name (blocks text) | DocTag _ (TagError name) text <- tags ] of
            [] -> []
            items -> [CDocSection "Errors" [DList DBullets items]]

        -- A variant and its text, on the line of the item.
        item name (DPara is : rest) = DPara (DCode (variantName name) : DText ": " : is) : rest
        item name rest = DPara [DCode (variantName name)] : rest

        variantName = T.intercalate "__" . T.splitOn "::" . T.pack

        -- A parameter is an input, except a &mut, which is also an output,
        -- or only an output when the comment says so.
        direction ParamOut _ = CParamOut
        direction ParamPlain name =
            case [ ts | Parameter pid ts _ <- params, pid == name ] of
                TReference Mutable _ : _ -> CParamInOut
                _ -> CParamIn

        cName (DocRef parts) = intercalate "__" (map T.unpack parts)
