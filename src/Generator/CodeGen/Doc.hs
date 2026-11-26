{-# LANGUAGE OverloadedStrings #-}
-- | The documentation comments of the C: the model of the comment of a
-- Termina element translated into the Doxygen comment of its C declaration.
module Generator.CodeGen.Doc
  ( genDoc
  , genProcedureDoc
  , genInstanceDoc
  , genFileDoc
  , genSourceFileDoc
  , moduleNamespace
  , namespaceGroup
  , statementComment
  , genBriefDoc
  , generatedDoc
  , textInline
  , codeInline
  , withFacts
  , withGeneratedParams
  , withProvides
  , eventParamDoc
  , instanceParamDoc
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.State (gets)
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import ControlFlow.Architecture.Types (ResourceLock(..))
import Core.AST (AccessKind(..), ClassKind(..), Const'(..), DocComment, FieldAssignment'(..), Identifier, Parameter'(..), TInteger(..), TerminaType'(..))
import Doc.Facts
import Doc.Model
import Generator.CodeGen.Common
import Generator.LanguageC.AST
import Parser.Types (ParserAnn(..))
import Semantic.AST (Expression(..), Parameter)
import Semantic.Types (GlobalDecl, SemanticAnn, globalDeclName, sourceOf)
import System.FilePath (splitDirectories)
import Utils.Annotations (Annotated(..), QualifiedName)
import Utils.Printer (ShowText(..))

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

-- | The Doxygen comment of the declaration of an instance of the deployment:
-- the comment of the author and the facts of the instance.
genInstanceDoc :: Identifier -> SemanticAnn -> CGenerator (Maybe CDoc)
genInstanceDoc identifier ann = do
    doc <- genDoc ann []
    facts <- gets (M.findWithDefault [] identifier . deploymentFacts)
    return $ withFacts (map factInlines facts) doc

-- | The comment of a C file of a module: @\@file@, which Doxygen needs to list
-- the members of the file, the group of the namespace of the module, and the
-- comment of the module, if it has one.
genFileDoc :: QualifiedName -> Maybe DocComment -> CGenerator CDoc
genFileDoc mName Nothing = return (CDoc [] [] [CDocFile, CDocInGroup (namespaceGroup (moduleNamespace mName))])
genFileDoc mName (Just comment) = do
    doc <- cDoc [] <$> modelOf comment
    return doc { cdocTags = CDocFile : CDocInGroup (namespaceGroup (moduleNamespace mName)) : cdocTags doc }

-- | The comment of the source file of a module: @\@file@, the group of the
-- namespace of the module, and a brief that names its header, with the path
-- with which the header is included, which Doxygen turns into a link.
genSourceFileDoc :: QualifiedName -> CDoc
genSourceFileDoc mName =
    CDoc [DText (T.pack ("Implementation of the functions declared in " ++ mName ++ ".h."))] []
        [CDocFile, CDocInGroup (namespaceGroup (moduleNamespace mName))]

-- | The namespace of a module, the folders of its name: [\"icu\"] for
-- \"icu/mb_store\", and none for a module at the root.
moduleNamespace :: QualifiedName -> [String]
moduleNamespace = init . splitDirectories

-- | The label of the Doxygen group of a namespace. The modules at the root go
-- in the group of the project.
namespaceGroup :: [String] -> String
namespaceGroup [] = "termina__project"
namespaceGroup segments = "termina__ns__" ++ intercalate "__" segments

-- | The comment of a statement, of a branch of an @if@ or of a case of a
-- @match@: its lines as the author wrote them.
statementComment :: SemanticAnn -> Maybe CComment
statementComment ann = CComment . map (T.pack . lineText) . docLines <$> parsedDoc (sourceOf ann)

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

-- | A fact of an instance of the deployment as a line of the comment of its
-- declaration.
factInlines :: DocFact SemanticAnn -> [DocInline' Ident]
factInlines fact = case fact of
    InstanceOf kind cls -> [text ("Instance of " ++ classKind kind ++ " class "), code cls, text "."]
    Priority p -> [text ("Priority " ++ integer p ++ ".")]
    StackSize s -> [text ("Stack of " ++ integer s ++ " bytes.")]
    ConnectedTo port (InterruptComponent vector) target ->
        [text "Port ", code port, text " connected to emitter ", code target, text (" (interrupt vector " ++ show vector ++ ").")]
    ConnectedTo port kind target -> [text "Port ", code port, text (" connected to " ++ componentKind kind ++ " "), code target, text "."]
    ProtectedBy lock -> [text (protection lock)]
    UsedBy users -> text "Used by " : endpoints users ++ [text "."]
    Reaches resources -> text "Reaches " : commas [ [code r] | r <- resources ] ++ [text " through other resources."]
    ChannelOf ty size -> [text "Channel of ", typeCode ty, text (", capacity " ++ expression size ++ ".")]
    SentBy senders -> text "Sent by " : endpoints senders ++ [text "."]
    ReceivedBy receiver -> text "Received by " : endpoints [receiver] ++ [text "."]
    PoolOf ty size -> [text ("Pool of " ++ expression size ++ " boxes of "), typeCode ty, text "."]
    AtomicOf ty Nothing -> [text "Atomic of ", typeCode ty, text "."]
    AtomicOf ty (Just size) -> [text ("Atomic array of " ++ expression size ++ " values of "), typeCode ty, text "."]
    DequeOf ty capacity -> [text "Deque of boxes of ", typeCode ty, text (", capacity " ++ expression capacity ++ ".")]
    PeriodicTimerOf period -> [text ("Periodic timer, period " ++ timeVal period ++ ".")]
    InterruptOf vector -> [text ("Interrupt vector " ++ show vector ++ ".")]
    Triggers (Endpoint component port) -> [text "Triggers port ", code port, text " of ", code component, text "."]

    where

        text = textInline
        code = codeInline
        typeCode = DCode . showText

        classKind TaskClass = "task"
        classKind HandlerClass = "handler"
        classKind ResourceClass = "resource"
        classKind EmitterClass = "emitter"
        classKind ChannelClass = "channel"
        classKind TestClass = "test"

        componentKind EmitterComponent = "emitter"
        componentKind (InterruptComponent _) = "emitter"
        componentKind ChannelComponent = "channel"
        componentKind ResourceComponent = "resource"
        componentKind PoolComponent = "pool"
        componentKind AtomicComponent = "atomic"
        componentKind AtomicArrayComponent = "atomic array"
        componentKind DequeComponent = "deque"

        protection ResourceLockNone = "Lock: none."
        protection (ResourceLockMutex prio) = "Lock: mutex with priority ceiling " ++ integer prio ++ "."
        protection ResourceLockIrq = "Lock: interrupt masking."

        endpoints eps = commas [ [code component, text " (port ", code port, text ")"] | Endpoint component port <- eps ]

        integer (TInteger n _) = show n

        -- A constant as its value, any other expression as Termina writes it
        expression (Constant (I n _) _) = integer n
        expression e = T.unpack (showText e)

        -- A TimeVal in seconds and microseconds
        timeVal (StructInitializer fields _)
            | Just secs <- field "tv_sec" fields, Just usecs <- field "tv_usec" fields =
                secs ++ " s" ++ (if usecs == "0" then "" else " " ++ usecs ++ " us")
        timeVal e = expression e

        field name fields = case [ expression e | FieldValueAssignment f e _ <- fields, f == name ] of
            value : _ -> Just value
            [] -> Nothing

-- | Inline elements separated by commas, each one made of several.
commas :: [[DocInline' Ident]] -> [DocInline' Ident]
commas = intercalate [textInline ", "]

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
-- behind it.
generatedDoc :: [DocInline' Ident] -> CDoc
generatedDoc brief = CDoc brief [] []

-- | A piece of the text that the generator writes, and a name in it as code.
textInline, codeInline :: String -> DocInline' Ident
textInline = DText . T.pack
codeInline = DCode . T.pack

-- | A comment with the facts that the transpiler derives from the program,
-- one per item, in a section of their own after the rest. An element without
-- a comment gets one with the facts alone.
withFacts :: [[DocInline' Ident]] -> Maybe CDoc -> Maybe CDoc
withFacts [] doc = doc
withFacts facts doc = Just base { cdocTags = cdocTags base ++ [factsSection] }

    where

        base = fromMaybe (CDoc [] [] []) doc
        factsSection = CDocSection "Generated by the transpiler" [DList DBullets [ [DPara fact] | fact <- facts ]]

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

        -- The direction written in the comment, or else an input, except a
        -- &mut, which is also an output.
        direction ParamIn _ = CParamIn
        direction ParamOut _ = CParamOut
        direction ParamInOut _ = CParamInOut
        direction ParamPlain name =
            case [ ts | Parameter pid ts _ <- params, pid == name ] of
                TReference Mutable _ : _ -> CParamInOut
                _ -> CParamIn

-- | The C identifier of a name of Termina, not resolved yet.
cName :: DocRef -> Ident
cName (DocRef parts) = intercalate "__" (map T.unpack parts)
