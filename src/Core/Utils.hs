-- | Helper functions for types

module Core.Utils where

import Parser.AST
import qualified Data.List as L

copyTy :: TerminaType -> Bool
copyTy UInt8           = True
copyTy UInt16          = True
copyTy UInt32          = True
copyTy UInt64          = True
copyTy Int8            = True
copyTy Int16           = True
copyTy Int32           = True
copyTy Int64           = True
copyTy USize           = True
copyTy Bool            = True
copyTy Char            = True
copyTy (DefinedType _) = True
copyTy (Option (BoxSubtype {})) = False
copyTy (Option _)      = True
copyTy (Location _)    = True
copyTy _               = False

optionTy :: TerminaType -> Bool
optionTy UInt8           = True
optionTy UInt16          = True
optionTy UInt32          = True
optionTy UInt64          = True
optionTy Int8            = True
optionTy Int16           = True
optionTy Int32           = True
optionTy Int64           = True
optionTy USize           = True
optionTy Bool            = True
optionTy Char            = True
optionTy (DefinedType _) = True
optionTy (BoxSubtype _)  = True
optionTy _               = False 

-- | Predicate defining when a |TerminaType| is a declaration type.
-- This is used to determine if a type can be used in a declaration.
declTy :: TerminaType -> Bool
declTy UInt8           = True
declTy UInt16          = True
declTy UInt32          = True
declTy UInt64          = True
declTy Int8            = True
declTy Int16           = True
declTy Int32           = True
declTy Int64           = True
declTy USize           = True
declTy Bool            = True
declTy Char            = True
declTy (Array {})      = True
declTy (DefinedType _) = True
declTy (Option _)      = True
declTy _               = False 

arrayTy :: TerminaType -> Bool
arrayTy UInt8           = True
arrayTy UInt16          = True
arrayTy UInt32          = True
arrayTy UInt64          = True
arrayTy Int8            = True
arrayTy Int16           = True
arrayTy Int32           = True
arrayTy Int64           = True
arrayTy USize           = True
arrayTy Bool            = True
arrayTy Char            = True
arrayTy (Array ty _)    = arrayTy ty
arrayTy (DefinedType _) = True
arrayTy (Option (BoxSubtype _)) = False
arrayTy (Option _)      = True
arrayTy _               = False 

parameterTy :: TerminaType -> Bool
parameterTy UInt8            = True
parameterTy UInt16           = True
parameterTy UInt32           = True
parameterTy UInt64           = True
parameterTy Int8             = True
parameterTy Int16            = True
parameterTy Int32            = True
parameterTy Int64            = True
parameterTy USize            = True
parameterTy Bool             = True
parameterTy Char             = True
parameterTy (DefinedType _)  = True
parameterTy (Reference _ (Option (BoxSubtype _))) = False
parameterTy (Reference {}) = True
parameterTy (Option (BoxSubtype _)) = False
parameterTy (Option _)      = True
parameterTy _                = False

procedureParamTy :: TerminaType -> Bool
procedureParamTy UInt8             = True
procedureParamTy UInt16            = True
procedureParamTy UInt32            = True
procedureParamTy UInt64            = True
procedureParamTy Int8              = True
procedureParamTy Int16             = True
procedureParamTy Int32             = True
procedureParamTy Int64             = True
procedureParamTy USize             = True
procedureParamTy Bool              = True
procedureParamTy Char              = True
procedureParamTy (DefinedType _)   = True
procedureParamTy (Reference {})    = True
procedureParamTy (Option _)        = True
procedureParamTy _                 = False

classFieldTy :: TerminaType -> Bool
classFieldTy (SinkPort {})       = True
classFieldTy (InPort {})       = True
classFieldTy (OutPort {})      = True
classFieldTy (AccessPort {})   = True
classFieldTy (Location _)      = True
classFieldTy ty                = fieldTy ty

fieldTy :: TerminaType -> Bool
fieldTy = arrayTy

locTy :: TerminaType -> Bool
locTy = fieldTy

boxTy :: TerminaType -> Bool
boxTy UInt8           = True
boxTy UInt16          = True
boxTy UInt32          = True
boxTy UInt64          = True
boxTy Int8            = True
boxTy Int16           = True
boxTy Int32           = True
boxTy Int64           = True
boxTy USize           = True
boxTy Bool            = True
boxTy Char            = True
boxTy (DefinedType _) = True
boxTy _               = False

allocTy :: TerminaType -> Bool
allocTy = boxTy

accessPortTy :: TerminaType -> Bool
accessPortTy (DefinedType _) = True
accessPortTy (Allocator _)  = True
accessPortTy (AtomicAccess _) = True
accessPortTy (AtomicArrayAccess {}) = True
accessPortTy _ = False

msgTy :: TerminaType -> Bool
msgTy UInt8           = True
msgTy UInt16          = True
msgTy UInt32          = True
msgTy UInt64          = True
msgTy Int8            = True
msgTy Int16           = True
msgTy Int32           = True
msgTy Int64           = True
msgTy USize           = True
msgTy Bool            = True
msgTy Char            = True
msgTy (DefinedType _) = True
msgTy (BoxSubtype {}) = True
msgTy _               = False

refTy :: TerminaType -> Bool
refTy UInt8           = True
refTy UInt16          = True
refTy UInt32          = True
refTy UInt64          = True
refTy Int8            = True
refTy Int16           = True
refTy Int32           = True
refTy Int64           = True
refTy USize           = True
refTy Bool            = True
refTy Char            = True
refTy (DefinedType _) = True
refTy (Option _)      = True
refTy (Array {})      = True
refTy _               = False

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
eqTy :: TerminaType -> Bool
eqTy UInt8  = True
eqTy UInt16 = True
eqTy UInt32 = True
eqTy UInt64 = True
eqTy Int8   = True
eqTy Int16  = True
eqTy Int32  = True
eqTy Int64  = True
eqTy USize  = True
eqTy Bool   = True
eqTy Char   = True
eqTy _      = False

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
sameTy :: TerminaType -> TerminaType -> Bool
sameTy  UInt8  UInt8 = True
sameTy  UInt16  UInt16 = True
sameTy  UInt32  UInt32 = True
sameTy  UInt64  UInt64 = True
sameTy  Int8  Int8 = True
sameTy  Int16  Int16 = True
sameTy  Int32  Int32 = True
sameTy  Int64  Int64 = True
sameTy  USize  USize = True
sameTy  Bool  Bool = True
sameTy  Unit Unit = True
sameTy  (Option _) (Option Unit) = True
sameTy  (Option Unit) (Option _) = True
sameTy  (Option tyspecl) (Option tyspecr) = sameTy tyspecl tyspecr
sameTy  (Reference Mutable tyspecl) (Reference Mutable tyspecr) = sameTy tyspecl tyspecr
sameTy  (Reference Immutable tyspecl) (Reference Immutable tyspecr) = sameTy tyspecl tyspecr
sameTy  (BoxSubtype tyspecl) (BoxSubtype tyspecr) = sameTy tyspecl tyspecr
sameTy  (Array typespecl sizel) (Array typespecr sizer) = sameTy typespecl typespecr && (sizel == sizer)
sameTy  (DefinedType idl) (DefinedType idr) = idl == idr
-- Location subtypes
sameTy  (Location tyspecl) (Location tyspecr) = sameTy tyspecl tyspecr
sameTy  (Location tyspecl) tyspecr = sameTy tyspecl tyspecr
sameTy  tyspecl (Location tyspecr) = sameTy tyspecl tyspecr
--
sameTy  _ _ = False

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
