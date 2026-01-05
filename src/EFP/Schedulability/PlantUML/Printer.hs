module EFP.Schedulability.PlantUML.Printer where

import Prettyprinter
import qualified Data.Text as T
import Utils.Printer
import EFP.Schedulability.PlantUML.AST


class PlantUMLPrint a where
  pprint :: a -> DocStyle

instance PlantUMLPrint PlantUMLDiagram where
  pprint (PlantUMLSeqDiagram participans elements) = 
    pretty "@startuml" <> line <>
    vsep (map pprint participans) <> line <>
    vsep (map pprint elements) <> line <>
    pretty "@enduml"
  
instance PlantUMLPrint PlantUMLParticipant where
  pprint (PlantUMLParticipant name order) =
    pretty "participant" <+> pretty name <+> pretty "order" <+> pretty order
  pprint (PlantUMLDatabase name order) =
    pretty "database" <+> pretty name <+> pretty "order" <+> pretty order
  pprint (PlantUMLActor name order) =
    pretty "actor" <+> pretty name <+> pretty "order" <+> pretty order

instance PlantUMLPrint PlantUMLSeqDiagramStatement where
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