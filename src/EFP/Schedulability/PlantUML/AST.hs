module EFP.Schedulability.PlantUML.AST where
import EFP.Schedulability.Core.AST
import qualified Data.Text as T

data PlantUMLParticipant = 
    PlantUMLParticipant 
        Identifier -- ^ Participant name
        Integer -- ^ Order index
    | PlantUMLDatabase 
        Identifier -- ^ Participant name
        Integer -- ^ Order index
    | PlantUMLActor 
        Identifier -- ^ Participant name
        Integer -- ^ Order index
    deriving Show

data PlantUMLSeqDiagramStatement =
    PlantUMLSeqMessage
        Identifier -- ^ Sender participant
        Identifier -- ^ Receiver participant
        (Maybe T.Text) -- ^ Message text
    | PlantUMLSeqActivationStart
        Identifier -- ^ Participant
    | PlantUMLSeqActivationEnd
        Identifier -- ^ Participant
    | PlantUMLGroupStart 
        T.Text -- ^ Group title
        (Maybe T.Text) -- ^ Secondary title
    | PlantUMLGroupEnd
    | PlantUMLAltStart
        T.Text -- ^ Alt title
    | PlantUMLAltElse
        T.Text -- ^ Else title
    | PlantUMLAltEnd
    deriving Show

data PlantUMLDiagram = PlantUMLSeqDiagram
    [PlantUMLParticipant] -- ^ Participants
    [PlantUMLSeqDiagramStatement] -- ^ Diagram statements
    deriving Show