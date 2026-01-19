module Extras.PlantUML.AST where
import qualified Data.Text as T
import Core.AST

data PlantUMLColor =
    PlantUMLColorHex T.Text -- ^ Hex color code, e.g. "#FF5733"
    | PlantUMLColorName T.Text -- ^ Named color, e.g. "red", "blue"
    deriving Show

data PlantUMLSeqDiagParticipant = 
    PlantUMLSeqDiagParticipant 
        Identifier -- ^ Participant name
        Integer -- ^ Order index
    | PlantUMLDatabase 
        Identifier -- ^ Participant name
        Integer -- ^ Order index
    | PlantUMLActor 
        Identifier -- ^ Participant name
        Integer -- ^ Order index
    deriving Show

data PlantUMLSeqDiagStatement =
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

data PlantUMLCmpDiagramConnection =
    PlantUMLEventConnection 
        Identifier -- ^ Event source
        Identifier -- ^ Event target
    | PlantUMLChannelConnection 
        Identifier -- ^ Source
        Identifier -- ^ Target
    | PlantUMLInterfaceConnection 
        Identifier -- ^ Requirer
        Identifier -- ^ Provider
    deriving Show

data PlantUMLCmpParticipant = 
    PlantUMLCmpComponent 
        Identifier -- ^ Component name
        (Maybe T.Text) -- ^ Stereotype
        (Maybe PlantUMLColor) -- ^ Color
    | PlantUMLCmpAction 
        Identifier -- ^ Action name
        (Maybe T.Text) -- ^ Stereotype
        (Maybe PlantUMLColor) -- ^ Color
    | PlantUMLCmpBoundary 
        Identifier -- ^ Boundary name
        (Maybe T.Text) -- ^ Stereotype
        (Maybe PlantUMLColor) -- ^ Color
    | PlantUMLCmpChannel 
        Identifier -- ^ Channel name
        (Maybe PlantUMLColor) -- ^ Color
    | PlantUMLCmpDatabase 
        Identifier -- ^ Database name
        (Maybe T.Text) -- ^ Stereotype
        (Maybe PlantUMLColor) -- ^ Color
    | PlantUMLCmpInterface 
        Identifier -- ^ Interface name
        (Maybe T.Text) -- ^ Alias
    deriving Show

data PlantUMLDiagram = 
    PlantUMLSeqDiag
        [PlantUMLSeqDiagParticipant] -- ^ Participants
        [PlantUMLSeqDiagStatement] -- ^ Diagram statements
    | PlantUMLCmpDiag
        [PlantUMLCmpParticipant] -- ^ Participants
        [PlantUMLCmpDiagramConnection] -- ^ Diagram connections
    deriving Show