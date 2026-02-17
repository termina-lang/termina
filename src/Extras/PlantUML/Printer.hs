module Extras.PlantUML.Printer where

import Prettyprinter
import qualified Data.Text as T
import Utils.Printer
import Extras.PlantUML.AST

class PlantUMLPrint a where
  pprint :: a -> DocStyle

indentTab :: DocStyle -> DocStyle
indentTab = indent 4

braces' :: DocStyle -> DocStyle
braces' b = braces (line <> b <> line)

instance PlantUMLPrint PlantUMLDiagram where
  pprint (PlantUMLSeqDiag participans elements) = 
    pretty "@startuml" <> line <>
    vsep (map pprint participans) <> line <>
    vsep (map pprint elements) <> line <>
    pretty "@enduml"
  pprint (PlantUMLCmpDiag participants elements) =
    pretty "@startuml" <> line <>
    pretty "skinparam linetype ortho" <> line <>
    pretty "skinparam nodesep 10" <> line <>
    pretty "skinparam ranksep 100" <> line <>
    vsep (map pprint participants) <> line <>
    vsep (map pprint elements) <> line <>
    pretty "@enduml"
  
instance PlantUMLPrint PlantUMLCmpParticipant where
  pprint (PlantUMLCmpComponent name stereotype color) =
    let base = pretty "rectangle" <+> pretty name
        aliasDoc = case stereotype of
          Just a -> pretty "<<" <> pretty a <> pretty ">>"
          Nothing -> emptyDoc
        colorDoc = case color of
          Just (PlantUMLColorHex hex) -> pretty "#" <> pretty hex
          Just (PlantUMLColorName cname) -> pretty "#" <> pretty cname
          Nothing -> emptyDoc
    in base <+> aliasDoc <+> colorDoc
  pprint (PlantUMLCmpAction name stereotype color) =
    let base = pretty "action" <+> pretty name
        aliasDoc = case stereotype of
          Just a -> pretty "<<" <> pretty a <> pretty ">>"
          Nothing -> emptyDoc
        colorDoc = case color of
          Just (PlantUMLColorHex hex) -> pretty "#" <> pretty hex
          Just (PlantUMLColorName cname) -> pretty "#" <> pretty cname
          Nothing -> emptyDoc
    in base <+> aliasDoc <+> colorDoc
  pprint (PlantUMLCmpBoundary name stereotype color) =
    let base = pretty "boundary" <+> pretty name
        aliasDoc = case stereotype of
          Just a -> pretty "<<" <> pretty a <> pretty ">>"
          Nothing -> emptyDoc
        colorDoc = case color of
          Just (PlantUMLColorHex hex) -> pretty "#" <> pretty hex
          Just (PlantUMLColorName cname) -> pretty "#" <> pretty cname
          Nothing -> emptyDoc
    in base <+> aliasDoc <+> colorDoc
  pprint (PlantUMLCmpChannel name color) =
    let base = pretty "queue" <+> pretty name
        colorDoc = case color of
          Just (PlantUMLColorHex hex) -> pretty "#" <> pretty hex
          Just (PlantUMLColorName cname) -> pretty "#" <> pretty cname
          Nothing -> emptyDoc
    in base <+> colorDoc
  pprint (PlantUMLCmpDatabase name stereotype color) =
    let base = pretty "database" <+> pretty name
        aliasDoc = case stereotype of
          Just a -> pretty "<<" <> pretty a <> pretty ">>"
          Nothing -> emptyDoc
        colorDoc = case color of
          Just (PlantUMLColorHex hex) -> pretty "#" <> pretty hex
          Just (PlantUMLColorName cname) -> pretty "#" <> pretty cname
          Nothing -> emptyDoc
    in base <+> aliasDoc <+> colorDoc
  pprint (PlantUMLCmpInterface name alias) =
    let base = pretty "interface" <+> pretty name
        aliasDoc = case alias of
          Just a -> pretty "as" <+> pretty a
          Nothing -> emptyDoc
    in base <+> aliasDoc

instance PlantUMLPrint PlantUMLCmpDiagramConnection where
  pprint (PlantUMLEventConnection source target) =
    pretty source <+> pretty "==>" <+> pretty target
  pprint (PlantUMLChannelConnection source target) =
    pretty source <+> pretty "==>" <+> pretty target
  pprint (PlantUMLInterfaceConnection requirer provider) =
    pretty provider <+> pretty "-u0)-" <+> pretty requirer
  
instance PlantUMLPrint PlantUMLSeqDiagParticipant where
  pprint (PlantUMLSeqDiagParticipant name order) =
    pretty "participant" <+> pretty name <+> pretty "order" <+> pretty order
  pprint (PlantUMLDatabase name order) =
    pretty "database" <+> pretty name <+> pretty "order" <+> pretty order
  pprint (PlantUMLActor name order) =
    pretty "actor" <+> pretty name <+> pretty "order" <+> pretty order

instance PlantUMLPrint PlantUMLSeqDiagStatement where
  pprint (PlantUMLSeqMessage sender receiver Nothing) =
    pretty sender <+> pretty "->" <+> pretty receiver
  pprint (PlantUMLSeqMessage sender receiver (Just msg)) =
    pretty sender <+> pretty "->" <+> pretty receiver <+> pretty ":" <+> pretty msg
  pprint (PlantUMLSeqActivationStart participant) =
    pretty "activate" <+> pretty participant
  pprint (PlantUMLSeqActivationEnd participant) =
    pretty "deactivate" <+> pretty participant
  pprint (PlantUMLGroupStart title Nothing) =
    pretty "group" <+> pretty title
  pprint (PlantUMLGroupStart title (Just secondaryTitle)) =
    pretty "group" <+> pretty title <+> pretty "[" <> pretty secondaryTitle <> pretty "]"
  pprint PlantUMLGroupEnd =
    pretty "end"
  pprint (PlantUMLAltStart title) =
    pretty "alt" <+> pretty title
  pprint (PlantUMLAltElse title) =
    pretty "else" <+> pretty title
  pprint PlantUMLAltEnd =
    pretty "end"

runPlantUMLPrinter :: PlantUMLDiagram -> T.Text
runPlantUMLPrinter = render . pprint