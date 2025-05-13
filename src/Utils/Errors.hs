{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module Utils.Errors where

import qualified Data.Text as T
import Errata
import Errata.Styles

import Semantic.AST
import Numeric
import Utils.Annotations
import Text.Parsec.Pos
import qualified Language.LSP.Protocol.Types as LSP
import qualified Data.Map as M
import qualified Data.Text.Lazy as TL
import qualified Parser.AST as PAST

class ShowText a where
    showText :: a -> T.Text

instance ShowText AccessKind where
    showText Mutable = "mut "
    showText Private = "priv "
    showText Immutable = ""

instance ShowText Op where
    showText Addition = "+"
    showText Subtraction = "-"
    showText Multiplication = "*"
    showText Division = "/"
    showText Modulo = "%"
    showText BitwiseAnd = "&"
    showText BitwiseOr = "|"
    showText BitwiseXor = "^"
    showText BitwiseLeftShift = "<<"
    showText BitwiseRightShift = ">>"
    showText RelationalLT = "<"
    showText RelationalLTE = "<="
    showText RelationalGT = ">"
    showText RelationalGTE = ">="
    showText RelationalEqual = "=="
    showText RelationalNotEqual = "!="
    showText LogicalAnd = "&&"
    showText LogicalOr = "||"

instance ShowText (PAST.TypeParameter a) where
    showText :: PAST.TypeParameter a -> T.Text
    showText (TypeParamIdentifier ident) = T.pack ident
    showText (TypeParamTypeSpec ts) = showText ts
    showText (TypeParamSize size) = showText size

instance ShowText (PAST.TypeSpecifier a) where
    showText TSUInt8 = "u8"
    showText TSUInt16 = "u16"
    showText TSUInt32 = "u32"
    showText TSUInt64 = "u64"
    showText TSInt8 = "i8"
    showText TSInt16 = "i16"
    showText TSInt32 = "i32"
    showText TSInt64 = "i64"
    showText TSUSize = "usize"
    showText TSBool = "bool"
    showText TSChar = "char"
    showText (TSConstSubtype ts) = "const " <> showText ts
    showText (TSArray ts size) = "[" <> showText ts <> "; "  <> showText size <> "]"
    showText (TSBoxSubtype ts) = "box " <> showText ts
    showText (TSLocation ts) = "loc " <> showText ts
    showText (TSAccessPort ts) = "access " <> showText ts
    showText (TSSinkPort ts ident) = "sink " <> showText ts <> " triggers " <> T.pack ident
    showText (TSInPort ts ident) = "in " <> showText ts <> " triggers " <> T.pack ident
    showText (TSOutPort ts) = "out " <> showText ts
    showText (TSReference ak ts) = "&" <> showText ak <> showText ts
    showText (TSDefinedType ident []) = T.pack ident
    showText (TSDefinedType ident tsps) = T.pack ident <> "<" <> T.intercalate "; " (map showText tsps) <> ">"
    showText TSUnit = "()"

instance (ShowText (expr a)) => ShowText (FieldAssignment' expr a) where
    showText (FieldValueAssignment ident expr _) = 
        T.pack ident <> " = " <> showText expr
    showText (FieldAddressAssignment ident addr _) = 
        T.pack ident <> " @ " <> showText addr
    showText (FieldPortConnection InboundPortConnection ident glb _) = 
        T.pack ident <> " <- " <> T.pack glb
    showText (FieldPortConnection OutboundPortConnection ident glb _) =
        T.pack ident <> " -> " <> T.pack glb
    showText (FieldPortConnection AccessPortConnection ident glb _) =
        T.pack ident <> " <-> " <> T.pack glb

instance (ShowText (expr a)) => ShowText (MonadicVariant' expr a) where
    showText (Some e) = "Some(" <> showText e <> ")"
    showText None = "None"
    showText (Ok e) = "Ok(" <> showText e <> ")"
    showText (Error e) = "Error(" <> showText e <> ")"
    showText Success = "Success"
    showText (Failure e) = "Failure(" <> showText e <> ")"

instance ShowText MonadicVariantLabel where
    showText SomeLabel = "Some"
    showText NoneLabel = "None"
    showText OkLabel = "Ok"
    showText ErrorLabel = "Error"
    showText SuccessLabel = "Success"
    showText FailureLabel = "Failure"

instance ShowText (PAST.Object a) where
    showText (PAST.Variable ident _) = T.pack ident
    showText (PAST.ArrayIndexExpression obj expr _) = 
        showText obj <> "[" <> showText expr <> "]"
    showText (PAST.MemberAccess obj ident _) = 
        showText obj <> "." <> T.pack ident
    showText (PAST.Dereference obj _) = "*" <> showText obj
    showText (PAST.DereferenceMemberAccess obj ident _) = 
        "*" <> showText obj <> "." <> T.pack ident
    showText (PAST.ArraySlice obj expr1 expr2 _) = 
        showText obj <> "[" <> showText expr1 <> ".." <> showText expr2 <> "]"

instance ShowText (PAST.Expression a) where
    showText (PAST.AccessObject obj) = showText obj
    showText (PAST.Constant c _) = showText c
    showText (PAST.BinOp op lhe rhe _) = showText lhe <> " " <> showText op <> " " <> showText rhe
    showText (PAST.ReferenceExpression ak obj _) = "&" <> showText ak <> " " <> showText obj
    showText (PAST.Casting e ty _) = "(" <> showText ty <> ") " <> showText e
    showText (PAST.FunctionCall ident args _) = 
        T.pack ident <> "(" <> T.intercalate ", " (map showText args) <> ")"
    showText (PAST.MemberFunctionCall obj ident args _) =
        showText obj <> "." <> T.pack ident <> "(" <> T.intercalate ", " (map showText args) <> ")"
    showText (PAST.DerefMemberFunctionCall obj ident args _) = 
        showText obj <> "->" <> T.pack ident <> "(" <> T.intercalate ", " (map showText args) <> ")"
    showText (PAST.ArrayInitializer value size _) =
        "[" <> showText value <> "; " <> showText size <> "]"
    showText (PAST.ArrayExprListInitializer exprs _) = 
        "{" <> T.intercalate ", " (map showText exprs) <> "}"
    showText (PAST.StructInitializer fs Nothing _) = 
        "{" <> T.intercalate ", " (map showText fs) <> "}"
    showText (PAST.StructInitializer fs (Just ty) _) = 
        "{" <> T.intercalate ", " (map showText fs) <> "} : " <> showText ty
    showText (PAST.EnumVariantInitializer ident variant args _) = 
        T.pack ident <> "::" <> T.pack variant <> "(" <> T.intercalate ", " (map showText args) <> ")"
    showText (PAST.MonadicVariantInitializer ov _) = 
        "Some(" <> showText ov <> ")"
    showText (PAST.StringInitializer str _) = 
        "\"" <> T.pack str <> "\""
    showText (PAST.IsEnumVariantExpression obj ident variant _) = 
        showText obj <> " is " <> T.pack ident <> "::" <> T.pack variant
    showText (PAST.IsMonadicVariantExpression obj variant _) = 
        showText obj <> " is " <> showText variant

instance ShowText (Object a) where
    showText (Variable ident _) = T.pack ident
    showText (ArrayIndexExpression obj expr _) = 
        showText obj <> "[" <> showText expr <> "]"
    showText (MemberAccess obj ident _) = 
        showText obj <> "." <> T.pack ident
    showText (Dereference obj _) = "*" <> showText obj
    showText (DereferenceMemberAccess obj ident _) = 
        "*" <> showText obj <> "." <> T.pack ident
    showText (Unbox obj _) = "unbox " <> showText obj

instance ShowText (Expression a) where
    showText (AccessObject obj) = showText obj
    showText (Constant c _) = showText c
    showText (BinOp op lhe rhe _) = showText lhe <> " " <> showText op <> " " <> showText rhe
    showText (ReferenceExpression ak obj _) = "&" <> showText ak <> " " <> showText obj
    showText (Casting e ty _) = "(" <> showText ty <> ") " <> showText e
    showText (FunctionCall ident args _) = 
        T.pack ident <> "(" <> T.intercalate ", " (map showText args) <> ")"
    showText (MemberFunctionCall obj ident args _) =
        showText obj <> "." <> T.pack ident <> "(" <> T.intercalate ", " (map showText args) <> ")"
    showText (DerefMemberFunctionCall obj ident args _) = 
        showText obj <> "->" <> T.pack ident <> "(" <> T.intercalate ", " (map showText args) <> ")"
    showText (ArrayInitializer value size _) =
        "[" <> showText value <> "; " <> showText size <> "]"
    showText (ArrayExprListInitializer exprs _) = 
        "{" <> T.intercalate ", " (map showText exprs) <> "}"
    showText (StructInitializer fs _) = 
        "{" <> T.intercalate ", " (map showText fs) <> "}"
    showText (EnumVariantInitializer ident variant args _) = 
        T.pack ident <> "::" <> T.pack variant <> "(" <> T.intercalate ", " (map showText args) <> ")"
    showText (MonadicVariantInitializer ov _) = 
        "Some(" <> showText ov <> ")"
    showText (StringInitializer str _) = 
        "\"" <> T.pack str <> "\""
    showText (IsEnumVariantExpression obj ident variant _) = 
        showText obj <> " is " <> T.pack ident <> "::" <> T.pack variant
    showText (IsMonadicVariantExpression obj variant _) = 
        showText obj <> " is " <> showText variant
    showText (ArraySliceExpression ak obj lower upper _) = 
         showText ak <> " " <> showText obj <> "[" <> showText lower <> ".." <> showText upper <> "]"


instance ShowText (TerminaType a) where

    showText TUInt8 = "u8"
    showText TUInt16 = "u16"
    showText TUInt32 = "u32"
    showText TUInt64 = "u64"
    showText TInt8 = "i8"
    showText TInt16 = "i16"
    showText TInt32 = "i32"
    showText TInt64 = "i64"
    showText TUSize = "usize"
    showText TBool = "bool"
    showText TChar = "char"
    showText (TConstSubtype ts) = "const " <> showText ts
    showText (TStruct ident) = T.pack ident
    showText (TEnum ident) = T.pack ident
    showText (TInterface _ ident) = T.pack ident
    showText (TGlobal _ ident) = T.pack ident
    showText (TArray ts size) = "[" <> showText ts <> "; "  <> showText size <> "]"
    showText (TOption ty) = "Option<" <> showText ty <> ">"
    showText (TResult tyOk tyError) = "Result<" <> showText tyOk <> "; " <> showText tyError <> ">"
    showText (TStatus ty) = "Status<" <> showText ty <> ">"
    showText (TMsgQueue ts size) = "MsgQueue<" <> showText ts <> "; " <> showText size <> ">"
    showText (TPool ts size) = "Pool<" <> showText ts <> "; " <> showText size <> ">"
    showText (TAllocator ts) = "Allocator<" <> showText ts <> ">"
    showText (TAtomicAccess ts) = "AtomicAccess<" <> showText ts <> ">"
    showText (TAtomicArrayAccess ts size) = "AtomicArrayAccess<" <> showText ts <> "; " <> showText size <> ">"
    showText (TAtomic ts) = "Atomic<" <> showText ts <> ">"
    showText (TAtomicArray ts size) = "AtomicArray<" <> showText ts <> "; " <> showText size <> ">"
    showText (TReference ak ts) = "&" <> showText ak <> showText ts
    showText (TBoxSubtype ts) = "box " <> showText ts
    showText (TFixedLocation ts) = "loc " <> showText ts
    showText (TAccessPort ts) = "access " <> showText ts
    showText (TSinkPort ts ident) = "sink " <> showText ts <> " triggers " <> T.pack ident
    showText (TInPort ts ident) = "in " <> showText ts <> " triggers " <> T.pack ident
    showText (TOutPort ts) = "out " <> showText ts
    showText TUnit = "()"

instance ShowText TInteger where
    showText (TInteger value DecRepr) = T.pack $ show value
    showText (TInteger value HexRepr) = T.toUpper . T.pack $ "0x" <> showHex value ""
    showText (TInteger value OctalRepr) = T.pack ("0" <> showOct value "")

instance (ShowText (ty a)) => ShowText (Const' ty a) where
    showText (I i Nothing) = showText i
    showText (I i (Just ts)) = showText i <> " : " <> showText ts
    showText (B True) = "true"
    showText (B False) = "false"
    showText (C c) = T.pack [c]

instance ShowText (TypeDef' ty blk a) where
    showText (Struct ident _ _) = T.pack $ "struct " <> ident
    showText (Enum ident _ _) = T.pack $ "enum " <> ident
    showText (Class TaskClass ident _ _ _) = T.pack $ "task class " <> ident
    showText (Class ResourceClass ident _ _ _) = T.pack $ "resource class " <> ident
    showText (Class HandlerClass ident _ _ _) = T.pack $ "handler class " <> ident
    showText (Class EmitterClass ident _ _ _) = T.pack $ "emitter class " <> ident
    showText (Class ChannelClass ident _ _ _) = T.pack $ "channel class " <> ident
    showText (Interface RegularInterface ident _ _ _) = T.pack $ "interface " <> ident
    showText (Interface SystemInterface ident _ _ _) = T.pack $ "system interface " <> ident

class ErrorMessage a where

    -- | Error identifier 
    errorIdent :: a -> T.Text

    -- | Error title
    errorTitle :: a -> T.Text

    -- | Generates a message from a given error.
    toText :: 
        a -- ^ The error
        -> M.Map FilePath T.Text -- ^ Map of the project's source files to their contents
        -> T.Text

    -- | Generates an LSP diagnostic from a given error
    toDiagnostics :: 
        a -- ^ The error
        -> M.Map FilePath T.Text -- ^ Map of the project's source files to their contents
        -> [LSP.Diagnostic]
    
emptyRange :: LSP.Range
emptyRange = LSP.Range (LSP.Position 0 0) (LSP.Position 0 0)

loc2Range :: Location -> LSP.Range
loc2Range (Position _ start end) = 
    LSP.Range 
        (LSP.Position (fromIntegral (sourceLine start) - 1) (fromIntegral (sourceColumn start) - 1))
        (LSP.Position (fromIntegral (sourceLine end) - 1) (fromIntegral (sourceColumn end) - 1))
loc2Range _ = emptyRange

pprintSimpleError :: T.Text -> T.Text -> String -> Location -> Maybe T.Text -> T.Text
pprintSimpleError sourceLines errorMessage fileName (Position _ start end) msg = 
    TL.toStrict $ prettyErrors sourceLines [genSimpleErrata]
    
    where

        startLine = sourceLine start
        endLine = sourceLine end
        startColumn = sourceColumn start
        endColumn = 
            if startLine == endLine then 
                sourceColumn end 
            else 
                T.length (T.lines sourceLines !! (startLine - 1)) + 1

        genSimpleBlock :: Errata.Block
        genSimpleBlock = Errata.Block
            fancyRedStyle
            (fileName, startLine, startColumn)
            Nothing
            [Pointer startLine startColumn endColumn False Nothing fancyRedPointer]
            Nothing

        genSimpleErrata :: Errata
        genSimpleErrata = Errata
            (Just errorMessage)
            [genSimpleBlock]
            msg
pprintSimpleError _ _ _ _ _ = error "Internal error: invalid error position"
