{-# LANGUAGE OverloadedStrings #-}

module Utils.Errors where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import Errata
import Errata.Styles

import Semantic.AST
import Numeric
import Utils.Annotations
import Text.Parsec.Pos

class ShowText a where
    showText :: a -> T.Text

instance ShowText Size where
    showText (K (TInteger value DecRepr)) = T.pack $ show value
    showText (K (TInteger value HexRepr)) = T.toUpper . T.pack $ "0x" <> showHex value ""
    showText (K (TInteger value OctalRepr)) = T.pack ("0" <> showOct value "")
    showText (V ident) = T.pack ident

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

instance ShowText TypeParameter where
    showText (TypeParamTypeSpec ts) = showText ts
    showText (TypeParamSize size) = showText size

instance ShowText TypeSpecifier where
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


instance ShowText TerminaType where
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
    showText (TStruct ident) = T.pack ident
    showText (TEnum ident) = T.pack ident
    showText (TInterface ident) = T.pack ident
    showText (TGlobal _ ident) = T.pack ident
    showText (TArray ts size) = "[" <> showText ts <> "; "  <> showText size <> "]"
    showText (TOption ts) = "Option<" <> showText ts <> ">"
    showText (TMsgQueue ts size) = "MsgQueue<" <> showText ts <> "; " <> showText size <> ">"
    showText (TPool ts size) = "TPool<" <> showText ts <> "; " <> showText size <> ">"
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

instance (ShowText ty) => ShowText (Const' ty) where
    showText (I (TInteger value DecRepr) Nothing) = T.pack $ show value
    showText (I (TInteger value HexRepr) Nothing) = T.toUpper . T.pack $ "0x" <> showHex value ""
    showText (I (TInteger value DecRepr) (Just ts)) = T.pack (show value) <> " : " <> showText ts
    showText (I (TInteger value HexRepr) (Just ts)) = T.toUpper $ T.pack ("0x" <> showHex value "" <> " : ") <> showText ts
    showText (I (TInteger value OctalRepr) Nothing) = T.pack ("0" <> showOct value "")
    showText (I (TInteger value OctalRepr) (Just ts)) = T.pack ("0" <> showOct value "") <> " : " <> showText ts
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
    showText (Interface ident _ _) = T.pack $ "interface " <> ident

printSimpleError :: TL.Text -> T.Text -> String -> Location -> Maybe T.Text -> IO ()
printSimpleError sourceLines errorMessage fileName (Position start end) msg = 
    TLIO.putStrLn $ prettyErrors 
        sourceLines
        [genSimpleErrata]
    
    where

        startLine = sourceLine start
        endLine = sourceLine end
        startColumn = sourceColumn start
        endColumn = 
            if startLine == endLine then 
                sourceColumn end 
            else 
                fromIntegral $ TL.length (TL.lines sourceLines !! (startLine - 1)) + 1

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
printSimpleError _ _ _ _ _ = error "Internal error: invalid error position"
