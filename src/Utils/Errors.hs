{-# LANGUAGE OverloadedStrings #-}

module Utils.Errors where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import Errata
import Errata.Styles

import Core.AST
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

instance ShowText TerminaType where
    showText UInt8 = "u8"
    showText UInt16 = "u16"
    showText UInt32 = "u32"
    showText UInt64 = "u64"
    showText Int8 = "i8"
    showText Int16 = "i16"
    showText Int32 = "i32"
    showText Int64 = "i64"
    showText USize = "usize"
    showText Bool = "bool"
    showText Char = "char"
    showText (DefinedType ident) = T.pack ident
    showText (Array ts size) = "[" <> showText ts <> "; "  <> showText size <> "]"
    showText (Option ts) = "Option<" <> showText ts <> ">"
    showText (MsgQueue ts size) = "MsgQueue<" <> showText ts <> "; " <> showText size <> ">"
    showText (Pool ts size) = "Pool<" <> showText ts <> "; " <> showText size <> ">"
    showText (Allocator ts) = "Allocator<" <> showText ts <> ">"
    showText (AtomicAccess ts) = "AtomicAccess<" <> showText ts <> ">"
    showText (AtomicArrayAccess ts size) = "AtomicArrayAccess<" <> showText ts <> "; " <> showText size <> ">"
    showText (Atomic ts) = "Atomic<" <> showText ts <> ">"
    showText (AtomicArray ts size) = "AtomicArray<" <> showText ts <> "; " <> showText size <> ">"
    showText (Reference ak ts) = "&" <> showText ak <> showText ts
    showText (BoxSubtype ts) = "box " <> showText ts
    showText (Location ts) = "loc " <> showText ts
    showText (AccessPort ts) = "access " <> showText ts
    showText (SinkPort ts ident) = "sink " <> showText ts <> " triggers " <> T.pack ident
    showText (InPort ts ident) = "in " <> showText ts <> " triggers " <> T.pack ident
    showText (OutPort ts) = "out " <> showText ts
    showText Unit = "()"

instance ShowText Const where
    showText (I (TInteger value DecRepr) Nothing) = T.pack $ show value
    showText (I (TInteger value HexRepr) Nothing) = T.toUpper . T.pack $ "0x" <> showHex value ""
    showText (I (TInteger value DecRepr) (Just ts)) = T.pack (show value) <> " : " <> showText ts
    showText (I (TInteger value HexRepr) (Just ts)) = T.toUpper $ T.pack ("0x" <> showHex value "" <> " : ") <> showText ts
    showText (I (TInteger value OctalRepr) Nothing) = T.pack ("0" <> showOct value "")
    showText (I (TInteger value OctalRepr) (Just ts)) = T.pack ("0" <> showOct value "") <> " : " <> showText ts
    showText (B True) = "true"
    showText (B False) = "false"
    showText (C c) = T.pack [c]

instance ShowText (TypeDef' blk a) where
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
