module DataFlow.Program.Utils where

import AST.Seman
import DataFlow.Program.Types

getEmmiterIdentifier :: TPEmitter a -> Identifier
getEmmiterIdentifier (TPInterruptEmittter ident _) = ident
getEmmiterIdentifier (TPPeriodicTimerEmitter ident _) = ident
getEmmiterIdentifier (TPSystemInitEmitter ident _) = ident

getTriggeredAction :: Identifier -> [ClassMember a] -> Identifier
getTriggeredAction ident [] = error $ "Internal error: no port with identifier " ++ ident
getTriggeredAction ident (member : members) =
  case member of
    ClassField (FieldDefinition fid fty) _ | fid == ident -> getTriggeredAction' fty
    _ -> getTriggeredAction ident members

  where

    getTriggeredAction' :: TypeSpecifier -> Identifier
    getTriggeredAction' (SinkPort _ act) = act
    getTriggeredAction' (InPort _ act) = act
    getTriggeredAction' _ = error $ "Internal error: port " ++ ident ++ " is not a sink port or an in port"

getPortType :: Identifier -> [ClassMember a] -> TypeSpecifier
getPortType ident [] = error $ "Internal error: no port with identifier " ++ ident
getPortType ident (member : members) =
  case member of
    ClassField (FieldDefinition fid fty) _ | fid == ident -> fty
    _ -> getPortType ident members

getClassMembers :: TypeDef a -> [ClassMember a]
getClassMembers (Class _ _ members _ _) = members
getClassMembers _ = error $ "Internal error: getClassMembers called with the non-class type definition"