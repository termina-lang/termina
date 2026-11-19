{-# LANGUAGE OverloadedStrings #-}
-- | The documentation comments of the C: the model of the comment of a
-- Termina element translated into the Doxygen comment of its C declaration.
module Generator.CodeGen.Doc
  ( genDoc
  , genProcedureDoc
  , genBriefDoc
  , generatedDoc
  , withGeneratedParams
  , withProvides
  , eventParamDoc
  , instanceParamDoc
  ) where

import Control.Monad.Except (throwError)
import Data.List (intercalate)
import qualified Data.Text as T

import Core.AST (AccessKind(..), DocComment, Parameter'(..), TerminaType'(..))
import Doc.Model
import Generator.CodeGen.Common
import Generator.LanguageC.AST
import Parser.Types (ParserAnn(..))
import Semantic.AST (Parameter)
import Semantic.Types (GlobalDecl, SemanticAnn, globalDeclName, sourceOf)
import Utils.Annotations (Annotated(..))

-- | The Doxygen comment of the C declaration of an element, from the comment
-- of the element, given the parameters of its signature, if it has any.
genDoc :: SemanticAnn -> [Parameter SemanticAnn] -> CGenerator (Maybe CDoc)
genDoc ann params = fmap (cDoc params) <$> docModelOf ann

-- | The Doxygen comment of a procedure of a class, given its parameters and
-- the procedure of the interface it implements with its parameters, from
-- which it inherits what its own comment does not write.
genProcedureDoc :: SemanticAnn -> [Parameter SemanticAnn] -> (SemanticAnn, [Parameter SemanticAnn]) -> CGenerator (Maybe CDoc)
genProcedureDoc ann params (ifaceAnn, ifaceParams) = do
    own <- docModelOf ann
    inherited <- docModelOf ifaceAnn
    return $ cDoc params <$> inheritDoc (names params) (names ifaceParams) own inherited

    where

        names = map paramIdentifier

-- | The brief of the Doxygen comment of an element, alone.
genBriefDoc :: SemanticAnn -> CGenerator (Maybe CDoc)
genBriefDoc ann = fmap (\doc -> doc { cdocBody = [], cdocTags = [] }) <$> genDoc ann []

-- | The model of the comment of an element, if it has one.
docModelOf :: SemanticAnn -> CGenerator (Maybe DocModel)
docModelOf = traverse modelOf . parsedDoc . sourceOf

-- | The model of a comment. Its tags were checked when it was parsed.
modelOf :: DocComment -> CGenerator DocModel
modelOf comment = case docModel comment of
    Right model -> return model
    Left _ -> throwError $ InternalError "A documentation comment with an error in its tags reached the generator"

-- | The comment of a resource class with a section of the interfaces it
-- provides, each one with its summary. A class without a comment stays
-- without one.
withProvides :: [GlobalDecl SemanticAnn] -> Maybe CDoc -> CGenerator (Maybe CDoc)
withProvides _ Nothing = return Nothing
withProvides [] doc = return doc
withProvides ifaces (Just doc) = do
    items <- mapM item ifaces
    return $ Just doc { cdocTags = cdocTags doc ++ [CDocSection "Provides" [DList DBullets items]] }

    where

        item iface = do
            summary <- maybe [] (map (fmap cName) . docSummary) <$> docModelOf (getAnnotation iface)
            return [DPara (DCode (T.pack (globalDeclName iface)) : if null summary then [] else DText ": " : summary)]

-- | A comment with the parameters that the generator adds, before the ones
-- of the author. An element without a comment stays without one.
withGeneratedParams :: [CDocTag] -> Maybe CDoc -> Maybe CDoc
withGeneratedParams tags = fmap (\doc -> doc { cdocTags = tags ++ cdocTags doc })

-- | The parameter of the event that every member function of a class takes.
eventParamDoc :: CDocTag
eventParamDoc = CDocParam CParamIn eventParam
    [DPara [ DText "Event that started the current activation. It carries the task or handler that processes it, on whose behalf resources are locked." ]]

-- | The parameter of the instance of a class on which a member function runs,
-- given its name, its direction, the class and the text after the class.
instanceParamDoc :: Ident -> CParamDirection -> Ident -> String -> CDocTag
instanceParamDoc name dir cls after = CDocParam dir name
    [DPara [DText "Instance of ", DCode (T.pack cls), DText (T.pack after)]]

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

-- | The C identifier of a name of Termina, not resolved yet.
cName :: DocRef -> Ident
cName (DocRef parts) = intercalate "__" (map T.unpack parts)
