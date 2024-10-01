-- | Helper functions for types

module Core.Utils where

import Parser.AST
import qualified Data.List as L

-- Primitive Types definition. Assuming |TerminaType| is well-formed.
primitiveTypes :: TerminaType -> Bool
primitiveTypes UInt8           = True
primitiveTypes UInt16          = True
primitiveTypes UInt32          = True
primitiveTypes UInt64          = True
primitiveTypes Int8            = True
primitiveTypes Int16           = True
primitiveTypes Int32           = True
primitiveTypes Int64           = True
primitiveTypes USize           = True
primitiveTypes Bool            = True
primitiveTypes Char            = True
primitiveTypes (DefinedType _) = True
primitiveTypes (Array _ _)     = True
primitiveTypes  _              = False

returnValueTy :: TerminaType -> Bool
returnValueTy UInt8           = True
returnValueTy UInt16          = True
returnValueTy UInt32          = True
returnValueTy UInt64          = True
returnValueTy Int8            = True
returnValueTy Int16           = True
returnValueTy Int32           = True
returnValueTy Int64           = True
returnValueTy USize           = True
returnValueTy Bool            = True
returnValueTy Char            = True
returnValueTy (DefinedType _) = True
returnValueTy _               = False

parameterTy :: TerminaType -> Bool
parameterTy UInt8           = True
parameterTy UInt16          = True
parameterTy UInt32          = True
parameterTy UInt64          = True
parameterTy Int8            = True
parameterTy Int16           = True
parameterTy Int32           = True
parameterTy Int64           = True
parameterTy USize           = True
parameterTy Bool            = True
parameterTy Char            = True
parameterTy (DefinedType _) = True
parameterTy (Reference _ (BoxSubtype _)) = False
parameterTy (Reference _ (Option (BoxSubtype _))) = False
parameterTy (Reference {})  = True
parameterTy (Option (BoxSubtype _)) = False
parameterTy (Option _)      = True
parameterTy _               = False

procedureParamTy :: TerminaType -> Bool
procedureParamTy UInt8           = True
procedureParamTy UInt16          = True
procedureParamTy UInt32          = True
procedureParamTy UInt64          = True
procedureParamTy Int8            = True
procedureParamTy Int16           = True
procedureParamTy Int32           = True
procedureParamTy Int64           = True
procedureParamTy USize           = True
procedureParamTy Bool            = True
procedureParamTy Char            = True
procedureParamTy (DefinedType _) = True
procedureParamTy (Reference {})  = True
procedureParamTy (Option _)      = True
procedureParamTy _               = False

-- | The following function defines what we consider to be simple types.
-- Simple types can be used at variable creation, inside arrays and user defined structures.
-- Also at box object and function returned values.
-- Definition https://hackmd.io/a4CZIjogTi6dXy3RZtyhCA?view#Simple-types
simpleType :: TerminaType -> Bool
simpleType Unit                = False
simpleType (Option (BoxSubtype {})) = False
simpleType (BoxSubtype {}) = False
simpleType (MsgQueue {})       = False
simpleType (Pool {})           = False
simpleType (Reference {})      = False
simpleType (Location {})       = False
simpleType (SinkPort {})       = False
simpleType (AccessPort {})     = False
simpleType _                   = True

classFieldType :: TerminaType -> Bool
classFieldType Unit                         = False
classFieldType (BoxSubtype {})          = False
classFieldType (Option (BoxSubtype {})) = False
classFieldType (MsgQueue {})                = False
classFieldType (Pool {})                    = False
classFieldType (Reference {})               = False
classFieldType _                            = True

boolTy :: TerminaType -> Bool
boolTy Bool = True
boolTy _    = False

-- | Predicate definining when a |TerminaType| is numeric.
numTy :: TerminaType -> Bool
numTy UInt8  = True
numTy UInt16 = True
numTy UInt32 = True
numTy UInt64 = True
numTy Int8   = True
numTy Int16  = True
numTy Int32  = True
numTy Int64  = True
numTy USize  = True
numTy _      = False

posTy :: TerminaType -> Bool
posTy UInt8  = True
posTy UInt16 = True
posTy UInt32 = True
posTy UInt64 = True
posTy USize  = True
posTy _      = False

-- | Predicate defining when a |TerminaType| can be used in a comparison.
equatableTy :: TerminaType -> Bool
equatableTy UInt8  = True
equatableTy UInt16 = True
equatableTy UInt32 = True
equatableTy UInt64 = True
equatableTy Int8   = True
equatableTy Int16  = True
equatableTy Int32  = True
equatableTy Int64  = True
equatableTy USize  = True
equatableTy Bool   = True
equatableTy Char   = True
equatableTy _      = False

memberIntCons :: Integer -> TerminaType -> Bool
memberIntCons i UInt8  = ( 0 <= i ) && ( i <= 255)
memberIntCons i UInt16 = ( 0 <= i ) && ( i <= 65536)
memberIntCons i UInt32 = ( 0 <= i ) && ( i <= 4294967295)
memberIntCons i UInt64 = ( 0 <= i ) && ( i <= 18446744073709551615)
memberIntCons i Int8   = ( -128 <= i ) && ( i <= 127 )
memberIntCons i Int16  = ( -32768 <= i ) && ( i <= 32767 )
memberIntCons i Int32  = ( -2147483648 <= i ) && ( i <= 2147483647 )
memberIntCons i Int64  = ( -9223372036854775808 <= i ) && ( i <= 9223372036854775807 )
-- | TODO: This value depends on the target architecture and shall be selected
-- accordingly. Since we are currently targeting 32-bit systems, we assume that
-- usize is a 32-bit unsigned integer.
memberIntCons i USize  = ( 0 <= i ) && ( i <= 4294967295)
memberIntCons _ _      = False

identifierType :: TypeDef' blk a -> Identifier
identifierType (Struct ident _ _) = ident
identifierType (Enum ident _ _)   = ident
identifierType (Class _ ident _ _ _)  = ident
identifierType (Interface ident _ _) = ident

referenceType :: TerminaType -> Bool
referenceType Unit            = False
referenceType (MsgQueue {})   = False
referenceType (Pool {})       = False
referenceType (Reference {})  = False
referenceType (Location {})   = False
referenceType (SinkPort {})   = False
referenceType (AccessPort {}) = False
referenceType _               = True

----------------------------------------
-- Box Helpers
isBox :: TerminaType -> Maybe TerminaType
isBox (BoxSubtype t) = Just t
isBox _ = Nothing

isNonBoxOption :: TerminaType -> Bool
isNonBoxOption (Option (BoxSubtype _)) = False
isNonBoxOption (Option _) = True
isNonBoxOption _ = False

hasBoxOrDep :: TerminaType -> Either Identifier Bool
hasBoxOrDep (DefinedType ident) = Left ident
hasBoxOrDep UInt8 = Right False
hasBoxOrDep UInt16 = Right False
hasBoxOrDep UInt32 = Right False
hasBoxOrDep UInt64 = Right False
hasBoxOrDep Int8 = Right False
hasBoxOrDep Int16 = Right False
hasBoxOrDep Int32 = Right False
hasBoxOrDep Int64 = Right False
hasBoxOrDep USize = Right False
hasBoxOrDep Bool = Right False
hasBoxOrDep Char = Right False
--
hasBoxOrDep (Array ty _s) = hasBoxOrDep ty
hasBoxOrDep (Option ty) = hasBoxOrDep ty
hasBoxOrDep (Reference _accK ty) = hasBoxOrDep ty
hasBoxOrDep (BoxSubtype _) = Right True
hasBoxOrDep _ = Right False
----------------------------------------

rootType :: TerminaType -> TerminaType
rootType (Option ts) = rootType ts
rootType (Array ts _) = rootType ts
rootType (MsgQueue ts _) = rootType ts
rootType (Pool ts _) = rootType ts
rootType (Reference _ ts) = rootType ts
rootType (BoxSubtype ts) = rootType ts
rootType (Location ts) = rootType ts
rootType t = t

-- Type equality
checkEqTypes :: TerminaType -> TerminaType -> Bool
checkEqTypes  UInt8  UInt8 = True
checkEqTypes  UInt16  UInt16 = True
checkEqTypes  UInt32  UInt32 = True
checkEqTypes  UInt64  UInt64 = True
checkEqTypes  Int8  Int8 = True
checkEqTypes  Int16  Int16 = True
checkEqTypes  Int32  Int32 = True
checkEqTypes  Int64  Int64 = True
checkEqTypes  USize  USize = True
checkEqTypes  Bool  Bool = True
checkEqTypes  Unit Unit = True
checkEqTypes  (Option _) (Option Unit) = True
checkEqTypes  (Option Unit) (Option _) = True
checkEqTypes  (Option tyspecl) (Option tyspecr) = checkEqTypes tyspecl tyspecr
checkEqTypes  (Reference Mutable tyspecl) (Reference Mutable tyspecr) = checkEqTypes tyspecl tyspecr
checkEqTypes  (Reference Immutable tyspecl) (Reference Immutable tyspecr) = checkEqTypes tyspecl tyspecr
checkEqTypes  (BoxSubtype tyspecl) (BoxSubtype tyspecr) = checkEqTypes tyspecl tyspecr
checkEqTypes  (Array typespecl sizel) (Array typespecr sizer) = checkEqTypes typespecl typespecr && (sizel == sizer)
checkEqTypes  (DefinedType idl) (DefinedType idr) = idl == idr
-- Location subtypes
checkEqTypes  (Location tyspecl) (Location tyspecr) = checkEqTypes tyspecl tyspecr
checkEqTypes  (Location tyspecl) tyspecr = checkEqTypes tyspecl tyspecr
checkEqTypes  tyspecl (Location tyspecr) = checkEqTypes tyspecl tyspecr
--
checkEqTypes  _ _ = False

findWith :: (a -> Maybe b) -> [a] -> Maybe b
findWith f = L.foldl' (\acc a -> maybe acc Just (f a)) Nothing

findClassField :: Identifier -> [ ClassMember' blk a ] -> Maybe (TerminaType, a)
findClassField i
  =
  fmap
  (\case { ClassField (FieldDefinition _ t) a -> (t, a);
           _ -> error "Impossible after find"})
  .
  L.find (\case {ClassField (FieldDefinition ident _) _ -> ident == i;
                 _ -> False;})

findClassProcedure :: Identifier -> [ ClassMember' blk a ] -> Maybe ([TerminaType], a)
findClassProcedure i
  = fmap
  (\case {ClassProcedure _ ps _ a -> (map paramTerminaType ps,a)
         ; _ -> error "Impossible after find"})
  .
  L.find (\case{ ClassProcedure ident _ _ _ -> (ident == i)
               ; _ -> False})

findInterfaceProcedure :: Identifier -> [ InterfaceMember a ] -> Maybe ([TerminaType], a)
findInterfaceProcedure i
  = fmap
  (\case {InterfaceProcedure _ ps a -> (map paramTerminaType ps, a)})
  .
  L.find (\case{InterfaceProcedure ident _ _ -> (ident == i)})

findClassViewerOrMethod :: Identifier -> [ ClassMember' blk a ] -> Maybe ([TerminaType], Maybe TerminaType, a)
findClassViewerOrMethod i
  = fmap
  (\case {
    ClassViewer _ ps mty _ a -> (map paramTerminaType ps, mty, a);
    ClassMethod _ ty _ a -> ([], ty, a);
    _ -> error "Impossible after find"
  })
  .
  L.find (
    \case{
      ClassViewer ident _ _ _ _ -> (ident == i);
      ClassMethod ident _ _ _ -> (ident == i);
      _ -> False
    }
  )

findClassAction :: Identifier -> [ ClassMember' blk a ] -> Maybe (TerminaType, TerminaType, a)
findClassAction i
  =  fmap
  (\case {
    ClassAction _ param rty _ a -> (paramTerminaType param, rty, a);
    _ -> error "Impossible after find"
    })
  .
  L.find (\case{
    ClassAction ident _ _ _ _ -> (ident == i);
    _ -> False})

className :: ClassMember' blk a -> Identifier
className (ClassField e _)                = fieldIdentifier e
className (ClassMethod mIdent _ _ _)      = mIdent
className (ClassProcedure pIdent _ _ _) = pIdent
className (ClassViewer vIdent _ _ _ _)  = vIdent
className (ClassAction aIdent _ _ _ _)    = aIdent
----------------------------------------

glbName :: Global' expr a -> Identifier
glbName (Task tId _ _ _ _)     = tId
glbName (Resource tId _ _ _ _) = tId
glbName (Channel tId _ _ _ _)  = tId
glbName (Emitter tId _ _ _ _) = tId
glbName (Handler tId _ _ _ _)  = tId
glbName (Const tId _ _ _ _)    = tId

tyDefName :: TypeDef' blk a -> Identifier
tyDefName (Struct sId _ _ )= sId
tyDefName (Enum sId _ _ )=   sId
tyDefName (Class _ sId _ _ _ )=sId
tyDefName (Interface sId _ _ )=sId

globalsName :: AnnASTElement' blk expr a -> Identifier
globalsName (Function fId _ _ _ _ _) = fId
globalsName (GlobalDeclaration glb)    = glbName glb
globalsName (TypeDefinition tyDef _)   = tyDefName tyDef
