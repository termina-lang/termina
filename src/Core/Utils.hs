-- | Helper functions for types

module Core.Utils where

import Parser.AST
import qualified Data.List as L

copyTy :: TerminaType' expr a -> Bool
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
copyTy (TStruct _)      = True
copyTy (TEnum _)        = True
copyTy (TResult _ _)    = True
copyTy (TStatus _)      = True
copyTy (TOption (TBoxSubtype {})) = False
copyTy (TOption _)      = True
copyTy (TFixedLocation _)    = True
copyTy (TConstSubtype ty) = copyTy ty
copyTy _               = False

statusTy :: TerminaType' expr a -> Bool
statusTy TUInt8           = True
statusTy TUInt16          = True
statusTy TUInt32          = True
statusTy TUInt64          = True
statusTy TInt8            = True
statusTy TInt16           = True
statusTy TInt32           = True
statusTy TInt64           = True
statusTy TUSize           = True
statusTy TBool            = True
statusTy TChar            = True
statusTy (TStruct _)      = True
statusTy (TEnum _)        = True
statusTy _                = False 

resultTy :: TerminaType' expr a -> Bool
resultTy TUInt8           = True
resultTy TUInt16          = True
resultTy TUInt32          = True
resultTy TUInt64          = True
resultTy TInt8            = True
resultTy TInt16           = True
resultTy TInt32           = True
resultTy TInt64           = True
resultTy TUSize           = True
resultTy TBool            = True
resultTy TChar            = True
resultTy (TStruct _)      = True
resultTy (TEnum _)        = True
resultTy _                = False 

optionTy :: TerminaType' expr a -> Bool
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
optionTy (TStruct _)      = True
optionTy (TEnum _)        = True
optionTy (TBoxSubtype ty) = boxTy ty
optionTy _                = False 

-- | Predicate defining when a |TerminaType| is a declaration type.
-- This is used to determine if a type can be used in a declaration.
declTy :: TerminaType' expr a -> Bool
declTy TUInt8      = True
declTy TUInt16     = True
declTy TUInt32     = True
declTy TUInt64     = True
declTy TInt8       = True
declTy TInt16      = True
declTy TInt32      = True
declTy TInt64      = True
declTy TUSize      = True
declTy TBool       = True
declTy TChar       = True
declTy (TArray {}) = True
declTy (TStruct _) = True
declTy (TEnum _)   = True
declTy (TOption _) = True
declTy (TResult _ _) = True
declTy (TStatus _) = True
declTy _           = False 

arrayTy :: TerminaType' expr a -> Bool
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
arrayTy (TStruct _)      = True
arrayTy (TEnum _)        = True
arrayTy (TResult _ _)    = True
arrayTy (TStatus _)      = True
arrayTy (TOption (TBoxSubtype _)) = False
arrayTy (TOption _)      = True
arrayTy _               = False 

parameterTy :: TerminaType' expr a -> Bool
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
parameterTy (TStruct _)       = True
parameterTy (TEnum _)         = True
parameterTy (TResult _ _)     = True
parameterTy (TStatus _)       = True
parameterTy (TReference _ (TOption (TBoxSubtype _))) = False
parameterTy (TReference {}) = True
parameterTy (TOption (TBoxSubtype _)) = False
parameterTy (TOption ty)      = optionTy ty
parameterTy (TConstSubtype ty) = constTy ty
parameterTy _                = False

viewerParamTy :: TerminaType' expr a -> Bool
viewerParamTy TUInt8            = True
viewerParamTy TUInt16           = True
viewerParamTy TUInt32           = True
viewerParamTy TUInt64           = True
viewerParamTy TInt8             = True
viewerParamTy TInt16            = True
viewerParamTy TInt32            = True
viewerParamTy TInt64            = True
viewerParamTy TUSize            = True
viewerParamTy TBool             = True
viewerParamTy TChar             = True
viewerParamTy (TStruct _)       = True
viewerParamTy (TEnum _)         = True
viewerParamTy (TResult _ _)     = True
viewerParamTy (TStatus _)       = True
viewerParamTy (TReference _ (TOption (TBoxSubtype _))) = False
viewerParamTy (TReference Immutable ty) = refTy ty
viewerParamTy (TReference {}) = False
viewerParamTy (TOption (TBoxSubtype _)) = False
viewerParamTy (TOption ty)      = optionTy ty
viewerParamTy (TConstSubtype ty) = constTy ty
viewerParamTy _                = False

procedureParamTy :: TerminaType' expr a -> Bool
procedureParamTy TUInt8            = True
procedureParamTy TUInt16           = True
procedureParamTy TUInt32           = True
procedureParamTy TUInt64           = True
procedureParamTy TInt8             = True
procedureParamTy TInt16            = True
procedureParamTy TInt32            = True
procedureParamTy TInt64            = True
procedureParamTy TUSize            = True
procedureParamTy TBool             = True
procedureParamTy TChar             = True
procedureParamTy (TStruct _)       = True
procedureParamTy (TEnum _)         = True
procedureParamTy (TResult _ _)     = True
procedureParamTy (TStatus _)       = True
procedureParamTy (TReference _ ty) = refTy ty
procedureParamTy (TOption ty)      = optionTy ty
procedureParamTy (TBoxSubtype ty)  = boxTy ty
procedureParamTy (TConstSubtype ty) = constTy ty
procedureParamTy _                 = False

actionParamTy :: TerminaType' expr a -> Bool
actionParamTy TUInt8           = True
actionParamTy TUInt16          = True
actionParamTy TUInt32          = True
actionParamTy TUInt64          = True
actionParamTy TInt8            = True
actionParamTy TInt16           = True
actionParamTy TInt32           = True
actionParamTy TInt64           = True
actionParamTy TUSize           = True
actionParamTy TBool            = True
actionParamTy TChar            = True
actionParamTy (TStruct _)      = True
actionParamTy (TEnum _)        = True
actionParamTy (TResult _ _)    = True
actionParamTy (TStatus _)      = True
actionParamTy (TBoxSubtype ty) = boxTy ty
actionParamTy (TConstSubtype ty) = constTy ty
actionParamTy _                = False

classFieldTy :: TerminaType' expr a -> Bool
classFieldTy (TSinkPort {})    = True
classFieldTy (TInPort {})      = True
classFieldTy (TOutPort {})     = True
classFieldTy (TAccessPort {})  = True
classFieldTy (TFixedLocation _) = True
classFieldTy ty                = fieldTy ty

fieldTy :: TerminaType' expr a -> Bool
fieldTy = arrayTy

locTy :: TerminaType' expr a -> Bool
locTy = fieldTy

boxTy :: TerminaType' expr a -> Bool
boxTy TUInt8      = True
boxTy TUInt16     = True
boxTy TUInt32     = True
boxTy TUInt64     = True
boxTy TInt8       = True
boxTy TInt16      = True
boxTy TInt32      = True
boxTy TInt64      = True
boxTy TUSize      = True
boxTy TBool       = True
boxTy TChar       = True
boxTy (TStruct _) = True
boxTy (TEnum _)   = True
boxTy _           = False

allocTy :: TerminaType' expr a -> Bool
allocTy = boxTy

accessPortTy :: TerminaType' expr a -> Bool
accessPortTy (TInterface _ _)        = True
accessPortTy (TAllocator _)          = True
accessPortTy (TAtomicAccess _)       = True
accessPortTy (TAtomicArrayAccess {}) = True
accessPortTy _ = False

msgTy :: TerminaType' expr a -> Bool
msgTy TUnit            = True
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
msgTy (TStruct _)      = True
msgTy (TEnum _)        = True
msgTy (TBoxSubtype {}) = True
msgTy _               = False

refTy :: TerminaType' expr a -> Bool
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
refTy (TStruct _)      = True
refTy (TEnum _)        = True
refTy (TResult _ _)    = True
refTy (TStatus _)      = True
refTy (TOption _)      = True
refTy (TArray {})      = True
refTy _               = False

boolTy :: TerminaType' expr a -> Bool
boolTy TBool = True
boolTy _    = False

constTy :: TerminaType' expr a -> Bool
constTy TUInt8           = True
constTy TUInt16          = True
constTy TUInt32          = True
constTy TUInt64          = True
constTy TInt8            = True
constTy TInt16           = True
constTy TInt32           = True
constTy TInt64           = True
constTy TUSize           = True
constTy TBool            = True
constTy TChar            = True
constTy _                = False  

-- | Predicate definining when a |TerminaType| is numeric.
numTy :: TerminaType' expr a -> Bool
numTy TUInt8  = True
numTy TUInt16 = True
numTy TUInt32 = True
numTy TUInt64 = True
numTy TInt8   = True
numTy TInt16  = True
numTy TInt32  = True
numTy TInt64  = True
numTy TUSize  = True
numTy (TConstSubtype ty) = numTy ty
numTy _      = False

posTy :: TerminaType' expr a -> Bool
posTy TUInt8  = True
posTy TUInt16 = True
posTy TUInt32 = True
posTy TUInt64 = True
posTy TUSize  = True
posTy (TConstSubtype ty) = posTy ty
posTy _      = False

-- | Predicate defining when a |TerminaType| can be used in a comparison.
eqTy :: TerminaType' expr a -> Bool
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
eqTy (TConstSubtype ty) = eqTy ty
eqTy _      = False

memberIntCons :: Integer -> TerminaType' expr a -> Bool
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
memberIntCons i (TConstSubtype ty) = memberIntCons i ty
memberIntCons _ _      = False

getTypeIdentifier :: TypeDef' ty blk a -> Identifier
getTypeIdentifier (Struct ident _ _)        = ident
getTypeIdentifier (Enum ident _ _)          = ident
getTypeIdentifier (Class _ ident _ _ _)     = ident
getTypeIdentifier (Interface _ ident _ _ _) = ident

getGlobalIdentifier :: Global' ty expr a -> Identifier
getGlobalIdentifier (Task ident _ _ _ _)     = ident
getGlobalIdentifier (Resource ident _ _ _ _) = ident
getGlobalIdentifier (Channel ident _ _ _ _)  = ident
getGlobalIdentifier (Emitter ident _ _ _ _)  = ident
getGlobalIdentifier (Handler ident _ _ _ _)  = ident
getGlobalIdentifier (Const ident _ _ _ _)    = ident
getGlobalIdentifier (ConstExpr ident _ _ _ _) = ident

getGlobalType :: Global' ty expr a -> ty a
getGlobalType (Task _ ty _ _ _)     = ty
getGlobalType (Resource _ ty _ _ _) = ty
getGlobalType (Channel _ ty _ _ _)  = ty
getGlobalType (Emitter _ ty _ _ _)  = ty
getGlobalType (Handler _ ty _ _ _)  = ty
getGlobalType (Const _ ty _ _ _)    = ty
getGlobalType (ConstExpr _ ty _ _ _) = ty

----------------------------------------
-- Box Helpers
isBox :: TerminaType' expr a -> Maybe (TerminaType' expr a)
isBox (TBoxSubtype t) = Just t
isBox _ = Nothing

isNonBoxOption :: TerminaType' expr a -> Bool
isNonBoxOption (TOption (TBoxSubtype _)) = False
isNonBoxOption (TOption _) = True
isNonBoxOption _ = False

----------------------------------------

rootType :: TerminaType' expr a -> TerminaType' expr a
rootType (TOption ts) = rootType ts
rootType (TArray ts _) = rootType ts
rootType (TMsgQueue ts _) = rootType ts
rootType (TPool ts _) = rootType ts
rootType (TReference _ ts) = rootType ts
rootType (TBoxSubtype ts) = rootType ts
rootType (TFixedLocation ts) = rootType ts
rootType t = t

-- Type equality
sameTy :: 
  TerminaType' expr a -- ^ Expected type
  -> TerminaType' expr a  -- ^ Actual type
  -> Bool
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
sameTy  TChar TChar = True
sameTy  (TConstSubtype tyspecl) (TConstSubtype tyspecr) = sameTy tyspecl tyspecr
sameTy  (TConstSubtype _) _ = False
sameTy  tyspecl (TConstSubtype tyspecr) = sameTy tyspecl tyspecr
sameTy  (TOption _) (TOption TUnit) = True
sameTy  (TOption TUnit) (TOption _) = True
sameTy  (TOption tyspecl) (TOption tyspecr) = sameTy tyspecl tyspecr
sameTy  (TResult tyll tylr) (TResult tyrl tyrr) = sameTy tyll tyrl && sameTy tylr tyrr
sameTy  (TStatus tyspecl) (TStatus tyspecr) = sameTy tyspecl tyspecr
sameTy  (TReference Mutable tyspecl) (TReference Mutable tyspecr) = sameTy tyspecl tyspecr
sameTy  (TReference Immutable tyspecl) (TReference Immutable tyspecr) = sameTy tyspecl tyspecr
sameTy  (TBoxSubtype tyspecl) (TBoxSubtype tyspecr) = sameTy tyspecl tyspecr
sameTy  (TArray typespecl _sizel) (TArray typespecr _sizer) = sameTy typespecl typespecr
sameTy  (TStruct idl) (TStruct idr) = idl == idr
sameTy  (TEnum idl) (TEnum idr) = idl == idr
sameTy  (TGlobal _ idl) (TGlobal _ idr) = idl == idr
sameTy  (TInterface RegularInterface idl) (TInterface RegularInterface idr) = idl == idr
sameTy  (TInterface SystemInterface idl) (TInterface SystemInterface idr) = idl == idr
sameTy  (TAtomic tyl) (TAtomic tyr) = sameTy tyl tyr 
sameTy  (TAtomicArray tyl _sizel) (TAtomicArray tyr _sizer) = sameTy tyl tyr
-- TFixedLocation subtypes
sameTy  (TFixedLocation tyspecl) (TFixedLocation tyspecr) = sameTy tyspecl tyspecr
sameTy  (TFixedLocation tyspecl) tyspecr = sameTy tyspecl tyspecr
sameTy  tyspecl (TFixedLocation tyspecr) = sameTy tyspecl tyspecr
--
sameTy  _ _ = False

emitterInstTy :: Identifier -> Bool
emitterInstTy "PeriodicTimer" = True
emitterInstTy _ = False

findClassField :: Identifier -> [ ClassMember' ty blk a ] -> Maybe (ty a, a)
findClassField i
  =
  fmap
  (\case { ClassField (FieldDefinition _ t a) -> (t, a);
           _ -> error "Impossible after find"})
  .
  L.find (\case {ClassField (FieldDefinition ident _ _) -> ident == i;
                 _ -> False;})

findClassProcedure :: Identifier -> [ ClassMember' ty blk a ] -> Maybe ([Parameter' ty a], a)
findClassProcedure i
  = fmap
  (\case {ClassProcedure _ _ ps _ a -> (ps, a)
         ; _ -> error "Impossible after find"})
  .
  L.find (\case{ ClassProcedure _ ident _ _ _ -> (ident == i)
               ; _ -> False})

findInterfaceProcedure :: Identifier -> [ InterfaceMember' ty a ] -> Maybe (AccessKind, [Parameter' ty a], a)
findInterfaceProcedure i
  = fmap
  (\case {InterfaceProcedure ak _ ps _ a -> (ak, ps, a)})
  .
  L.find (\case{InterfaceProcedure _ ident _ _ _ -> (ident == i)})

findClassViewerOrMethod :: Identifier -> [ ClassMember' ty blk a ] -> Maybe ([Parameter' ty a], Maybe (ty a), a)
findClassViewerOrMethod i
  = fmap
  (\case {
    ClassViewer _ ps mty _ a -> (ps, mty, a);
    ClassMethod _ _ ty _ a -> ([], ty, a);
    _ -> error "Impossible after find"
  })
  .
  L.find (
    \case{
      ClassViewer ident _ _ _ _ -> (ident == i);
      ClassMethod _ ident _ _ _ -> (ident == i);
      _ -> False
    }
  )

findClassAction :: Identifier -> [ ClassMember' ty blk a ] -> Maybe (Maybe (Parameter' ty a), ty a, a)
findClassAction i
  =  fmap
  (\case {
    ClassAction _ _ param rty _ a -> (param, rty, a);
    _ -> error "Impossible after find"
    })
  .
  L.find (\case{
    ClassAction _ ident _ _ _ _ -> (ident == i);
    _ -> False})

className :: ClassMember' ty blk a -> Identifier
className (ClassField e)                = fieldIdentifier e
className (ClassMethod _ mIdent _ _ _)    = mIdent
className (ClassProcedure _ pIdent _ _ _) = pIdent
className (ClassViewer vIdent _ _ _ _)  = vIdent
className (ClassAction _ aIdent _ _ _ _)  = aIdent
