-- | Helper functions for types

module Core.Utils where

import Parser.AST
import qualified Data.List as L

copyTy :: TerminaType -> Bool
copyTy TUInt8           = True
copyTy TUInt16          = True
copyTy TUInt32          = True
copyTy TUInt64          = True
copyTy TInt8            = True
copyTy TInt16           = True
copyTy TInt32           = True
copyTy TInt64           = True
copyTy TUSize           = True
copyTy TBool            = True
copyTy TChar            = True
copyTy (TDefinedType _) = True
copyTy (TOption (TBoxSubtype {})) = False
copyTy (TOption _)      = True
copyTy (TLocation _)    = True
copyTy _               = False

optionTy :: TerminaType -> Bool
optionTy TUInt8           = True
optionTy TUInt16          = True
optionTy TUInt32          = True
optionTy TUInt64          = True
optionTy TInt8            = True
optionTy TInt16           = True
optionTy TInt32           = True
optionTy TInt64           = True
optionTy TUSize           = True
optionTy TBool            = True
optionTy TChar            = True
optionTy (TDefinedType _) = True
optionTy (TBoxSubtype _)  = True
optionTy _               = False 

-- | Predicate defining when a |TerminaType| is a declaration type.
-- This is used to determine if a type can be used in a declaration.
declTy :: TerminaType -> Bool
declTy TUInt8           = True
declTy TUInt16          = True
declTy TUInt32          = True
declTy TUInt64          = True
declTy TInt8            = True
declTy TInt16           = True
declTy TInt32           = True
declTy TInt64           = True
declTy TUSize           = True
declTy TBool            = True
declTy TChar            = True
declTy (TArray {})      = True
declTy (TDefinedType _) = True
declTy (TOption _)      = True
declTy _               = False 

arrayTy :: TerminaType -> Bool
arrayTy TUInt8           = True
arrayTy TUInt16          = True
arrayTy TUInt32          = True
arrayTy TUInt64          = True
arrayTy TInt8            = True
arrayTy TInt16           = True
arrayTy TInt32           = True
arrayTy TInt64           = True
arrayTy TUSize           = True
arrayTy TBool            = True
arrayTy TChar            = True
arrayTy (TArray ty _)    = arrayTy ty
arrayTy (TDefinedType _) = True
arrayTy (TOption (TBoxSubtype _)) = False
arrayTy (TOption _)      = True
arrayTy _               = False 

parameterTy :: TerminaType -> Bool
parameterTy TUInt8            = True
parameterTy TUInt16           = True
parameterTy TUInt32           = True
parameterTy TUInt64           = True
parameterTy TInt8             = True
parameterTy TInt16            = True
parameterTy TInt32            = True
parameterTy TInt64            = True
parameterTy TUSize            = True
parameterTy TBool             = True
parameterTy TChar             = True
parameterTy (TDefinedType _)  = True
parameterTy (TReference _ (TOption (TBoxSubtype _))) = False
parameterTy (TReference {}) = True
parameterTy (TOption (TBoxSubtype _)) = False
parameterTy (TOption _)      = True
parameterTy _                = False

procedureParamTy :: TerminaType -> Bool
procedureParamTy TUInt8             = True
procedureParamTy TUInt16            = True
procedureParamTy TUInt32            = True
procedureParamTy TUInt64            = True
procedureParamTy TInt8              = True
procedureParamTy TInt16             = True
procedureParamTy TInt32             = True
procedureParamTy TInt64             = True
procedureParamTy TUSize             = True
procedureParamTy TBool              = True
procedureParamTy TChar              = True
procedureParamTy (TDefinedType _)   = True
procedureParamTy (TReference {})    = True
procedureParamTy (TOption _)        = True
procedureParamTy _                 = False

classFieldTy :: TerminaType -> Bool
classFieldTy (TSinkPort {})       = True
classFieldTy (TInPort {})       = True
classFieldTy (TOutPort {})      = True
classFieldTy (TAccessPort {})   = True
classFieldTy (TLocation _)      = True
classFieldTy ty                = fieldTy ty

fieldTy :: TerminaType -> Bool
fieldTy = arrayTy

locTy :: TerminaType -> Bool
locTy = fieldTy

boxTy :: TerminaType -> Bool
boxTy TUInt8           = True
boxTy TUInt16          = True
boxTy TUInt32          = True
boxTy TUInt64          = True
boxTy TInt8            = True
boxTy TInt16           = True
boxTy TInt32           = True
boxTy TInt64           = True
boxTy TUSize           = True
boxTy TBool            = True
boxTy TChar            = True
boxTy (TDefinedType _) = True
boxTy _               = False

allocTy :: TerminaType -> Bool
allocTy = boxTy

accessPortTy :: TerminaType -> Bool
accessPortTy (TDefinedType _) = True
accessPortTy (TAllocator _)  = True
accessPortTy (TAtomicAccess _) = True
accessPortTy (TAtomicArrayAccess {}) = True
accessPortTy _ = False

msgTy :: TerminaType -> Bool
msgTy TUInt8           = True
msgTy TUInt16          = True
msgTy TUInt32          = True
msgTy TUInt64          = True
msgTy TInt8            = True
msgTy TInt16           = True
msgTy TInt32           = True
msgTy TInt64           = True
msgTy TUSize           = True
msgTy TBool            = True
msgTy TChar            = True
msgTy (TDefinedType _) = True
msgTy (TBoxSubtype {}) = True
msgTy _               = False

refTy :: TerminaType -> Bool
refTy TUInt8           = True
refTy TUInt16          = True
refTy TUInt32          = True
refTy TUInt64          = True
refTy TInt8            = True
refTy TInt16           = True
refTy TInt32           = True
refTy TInt64           = True
refTy TUSize           = True
refTy TBool            = True
refTy TChar            = True
refTy (TDefinedType _) = True
refTy (TOption _)      = True
refTy (TArray {})      = True
refTy _               = False

boolTy :: TerminaType -> Bool
boolTy TBool = True
boolTy _    = False

-- | Predicate definining when a |TerminaType| is numeric.
numTy :: TerminaType -> Bool
numTy TUInt8  = True
numTy TUInt16 = True
numTy TUInt32 = True
numTy TUInt64 = True
numTy TInt8   = True
numTy TInt16  = True
numTy TInt32  = True
numTy TInt64  = True
numTy TUSize  = True
numTy _      = False

posTy :: TerminaType -> Bool
posTy TUInt8  = True
posTy TUInt16 = True
posTy TUInt32 = True
posTy TUInt64 = True
posTy TUSize  = True
posTy _      = False

-- | Predicate defining when a |TerminaType| can be used in a comparison.
eqTy :: TerminaType -> Bool
eqTy TUInt8  = True
eqTy TUInt16 = True
eqTy TUInt32 = True
eqTy TUInt64 = True
eqTy TInt8   = True
eqTy TInt16  = True
eqTy TInt32  = True
eqTy TInt64  = True
eqTy TUSize  = True
eqTy TBool   = True
eqTy TChar   = True
eqTy _      = False

memberIntCons :: Integer -> TerminaType -> Bool
memberIntCons i TUInt8  = ( 0 <= i ) && ( i <= 255)
memberIntCons i TUInt16 = ( 0 <= i ) && ( i <= 65536)
memberIntCons i TUInt32 = ( 0 <= i ) && ( i <= 4294967295)
memberIntCons i TUInt64 = ( 0 <= i ) && ( i <= 18446744073709551615)
memberIntCons i TInt8   = ( -128 <= i ) && ( i <= 127 )
memberIntCons i TInt16  = ( -32768 <= i ) && ( i <= 32767 )
memberIntCons i TInt32  = ( -2147483648 <= i ) && ( i <= 2147483647 )
memberIntCons i TInt64  = ( -9223372036854775808 <= i ) && ( i <= 9223372036854775807 )
-- | TODO: This value depends on the target architecture and shall be selected
-- accordingly. Since we are currently targeting 32-bit systems, we assume that
-- usize is a 32-bit unsigned integer.
memberIntCons i TUSize  = ( 0 <= i ) && ( i <= 4294967295)
memberIntCons _ _      = False

identifierType :: TypeDef' blk a -> Identifier
identifierType (Struct ident _ _) = ident
identifierType (Enum ident _ _)   = ident
identifierType (Class _ ident _ _ _)  = ident
identifierType (Interface ident _ _) = ident

----------------------------------------
-- Box Helpers
isBox :: TerminaType -> Maybe TerminaType
isBox (TBoxSubtype t) = Just t
isBox _ = Nothing

isNonBoxOption :: TerminaType -> Bool
isNonBoxOption (TOption (TBoxSubtype _)) = False
isNonBoxOption (TOption _) = True
isNonBoxOption _ = False

hasBoxOrDep :: TerminaType -> Either Identifier Bool
hasBoxOrDep (TDefinedType ident) = Left ident
hasBoxOrDep TUInt8 = Right False
hasBoxOrDep TUInt16 = Right False
hasBoxOrDep TUInt32 = Right False
hasBoxOrDep TUInt64 = Right False
hasBoxOrDep TInt8 = Right False
hasBoxOrDep TInt16 = Right False
hasBoxOrDep TInt32 = Right False
hasBoxOrDep TInt64 = Right False
hasBoxOrDep TUSize = Right False
hasBoxOrDep TBool = Right False
hasBoxOrDep TChar = Right False
--
hasBoxOrDep (TArray ty _s) = hasBoxOrDep ty
hasBoxOrDep (TOption ty) = hasBoxOrDep ty
hasBoxOrDep (TReference _accK ty) = hasBoxOrDep ty
hasBoxOrDep (TBoxSubtype _) = Right True
hasBoxOrDep _ = Right False
----------------------------------------

rootType :: TerminaType -> TerminaType
rootType (TOption ts) = rootType ts
rootType (TArray ts _) = rootType ts
rootType (TMsgQueue ts _) = rootType ts
rootType (TPool ts _) = rootType ts
rootType (TReference _ ts) = rootType ts
rootType (TBoxSubtype ts) = rootType ts
rootType (TLocation ts) = rootType ts
rootType t = t

-- Type equality
sameTy :: TerminaType -> TerminaType -> Bool
sameTy  TUInt8  TUInt8 = True
sameTy  TUInt16  TUInt16 = True
sameTy  TUInt32  TUInt32 = True
sameTy  TUInt64  TUInt64 = True
sameTy  TInt8  TInt8 = True
sameTy  TInt16  TInt16 = True
sameTy  TInt32  TInt32 = True
sameTy  TInt64  TInt64 = True
sameTy  TUSize  TUSize = True
sameTy  TBool  TBool = True
sameTy  TUnit TUnit = True
sameTy  (TOption _) (TOption TUnit) = True
sameTy  (TOption TUnit) (TOption _) = True
sameTy  (TOption tyspecl) (TOption tyspecr) = sameTy tyspecl tyspecr
sameTy  (TReference Mutable tyspecl) (TReference Mutable tyspecr) = sameTy tyspecl tyspecr
sameTy  (TReference Immutable tyspecl) (TReference Immutable tyspecr) = sameTy tyspecl tyspecr
sameTy  (TBoxSubtype tyspecl) (TBoxSubtype tyspecr) = sameTy tyspecl tyspecr
sameTy  (TArray typespecl sizel) (TArray typespecr sizer) = sameTy typespecl typespecr && (sizel == sizer)
sameTy  (TDefinedType idl) (TDefinedType idr) = idl == idr
-- TLocation subtypes
sameTy  (TLocation tyspecl) (TLocation tyspecr) = sameTy tyspecl tyspecr
sameTy  (TLocation tyspecl) tyspecr = sameTy tyspecl tyspecr
sameTy  tyspecl (TLocation tyspecr) = sameTy tyspecl tyspecr
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

glbName :: Global' ty expr a -> Identifier
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

globalsName :: AnnASTElement' ty blk expr a -> Identifier
globalsName (Function fId _ _ _ _ _) = fId
globalsName (GlobalDeclaration glb)    = glbName glb
globalsName (TypeDefinition tyDef _)   = tyDefName tyDef
