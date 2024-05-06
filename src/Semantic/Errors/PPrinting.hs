-- | Semantic Error Printing
{-# LANGUAGE OverloadedStrings #-}

module Semantic.Errors.PPrinting where

import Semantic.Errors.Errors

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Parser.Parsing
import Text.Parsec.Pos
import Errata
import Errata.Styles
import AST.Seman
import Numeric
import qualified Data.Map as M

class ShowText a where
    showText :: a -> T.Text

instance ShowText Size where
    showText (K (TInteger value DecRepr)) = T.pack $ show value
    showText (K (TInteger value HexRepr)) = T.toUpper . T.pack $ "0x" <> showHex value ""
    showText (K (TInteger value OctalRepr)) = T.pack ("0" <> showOct value "")
    showText (V ident) = T.pack ident

instance ShowText AccessKind where
    showText Mutable = "mut"
    showText Immutable = ""
    showText Private = "priv"

instance ShowText TypeSpecifier where
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
    showText (Slice ts) = "[" <> showText ts <> "]"
    showText (Option ts) = "Option<" <> showText ts <> ">"
    showText (MsgQueue ts size) = "MsgQueue<" <> showText ts <> "; " <> showText size <> ">"
    showText (Pool ts size) = "Pool<" <> showText ts <> "; " <> showText size <> ">"
    showText (Allocator ts) = "Allocator<" <> showText ts <> ">"
    showText (Reference ak ts) = "&" <> showText ak <> " " <> showText ts
    showText (DynamicSubtype ts) = "dyn " <> showText ts
    showText (Location ts) = "loc " <> showText ts
    showText (AccessPort ts) = "access " <> showText ts
    showText (SinkPort ts ident) = "sink " <> showText ts <> " triggers " <> T.pack ident
    showText (InPort ts ident) = "in " <> showText ts <> " triggers " <> T.pack ident
    showText (OutPort ts) = "out " <> showText ts
    showText Unit = "()"

printSimpleError :: TL.Text -> T.Text -> String -> Int -> Int -> Int -> Maybe T.Text -> IO ()
printSimpleError sourceLines errorMessage fileName lineNumber lineColumn len msg = 
    TL.putStrLn $ prettyErrors 
        sourceLines
        [genSimpleErrata]
    
    where

        genSimpleBlock :: Errata.Block
        genSimpleBlock = Block
            fancyRedStyle
            (fileName, lineNumber, lineColumn)
            Nothing
            [Pointer lineNumber lineColumn (lineColumn + len) False Nothing fancyRedPointer]
            Nothing

        genSimpleErrata :: Errata
        genSimpleErrata = Errata
            (Just errorMessage)
            [genSimpleBlock]
            msg


-- useful prettyprinter doc
-- https://hackage.haskell.org/package/prettyprinter-1.7.1/docs/Prettyprinter.html
ppError :: M.Map FilePath TL.Text -> 
    SemanticErrors -> IO ()
ppError toModuleAST (AnnError e (Position pos)) =
  let fileName = sourceName pos
      lineNumber = sourceLine pos
      lineColumn = sourceColumn pos
      sourceLines = toModuleAST M.! fileName
  in
  case e of
    (EArray ts) -> 
        let title = "error[E001]: invalid array indexing."
        in
        case ts of
            (Slice _) -> printSimpleError 
                sourceLines title fileName 
                lineNumber lineColumn 1 
                (Just ("You cannot index a slice. It is not an array. \n" <>
                "A slice can only be used to create references  to a part of an array."))
            _ -> printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 1
                (Just "You are trying to index an object that is not an array.")
    (ENotNamedObject ident) -> 
        let title = "error[E002]: Object not found."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn (length ident)
                (Just ("The variable \x1b[31m" <> T.pack ident <> "\x1b[0m has not been declared"))
    (ENotConstant ident) -> 
        let 
            title = "error[E003]: invalid use of a non-constant object."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn (length ident)
                (Just ("The object \x1b[31m" <> T.pack ident <> "\x1b[0m is not a constant."))
    EAssignmentToImmutable -> 
        let title = "error[E004]: assignment to immutable variable."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 1
                (Just "You are trying to assign a value to an immutable object.")
    EIfElseNoOtherwise ->
        let title = "error[E005]: missing else clause."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 2
                (Just ("You are missing the else clause in an if-else-if statement.\n" <>
                    "You must provide an else clause if you are defining an else-if clause."))
    ECasteable ts1 ts2 -> 
        let title = "error[E006]: invalid cast."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 2
                (Just ("You cannot cast a value of type \x1b[31m" <> showText ts1 <> "\x1b[0m to type \x1b[31m" <> showText ts2 <> "\x1b[0m."))
    EInvalidParameterType (Parameter ident ts) -> 
        let title = "error[E007]: invalid parameter type."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 1
                (Just ("Parameter \x1b[31m" <> T.pack ident <> "\x1b[0m has an invalid type \x1b[31m" <> showText ts <> "\x1b[0m."))
    EInvalidReturnType ts -> 
        let title = "error[E008]: invalid return type."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 1
                (Just ("Function returns an invalid type \x1b[31m" <> showText ts <> "\x1b[0m."))
    EProcedureExtraParams (procId, params, Position procPos) paramNumber ->
        let title = "error[E009]: extra parameters in procedure call."
            procFileName = sourceName procPos
            procLineNumber = sourceLine procPos
            procLineColumn = sourceColumn procPos
            procSourceLines = toModuleAST M.! procFileName
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 1
                (Just ("Procedure \x1b[31m" <> T.pack procId <> 
                    "\x1b[0m has only \x1b[31m" <> T.pack (show (length params)) <> 
                    "\x1b[0m parameters but you are providing \x1b[31m" <> T.pack (show paramNumber) <> "\x1b[0m.")) >>
            printSimpleError 
                procSourceLines "The interface of the procedure is defined as follows:" procFileName
                procLineNumber procLineColumn 1
                Nothing

    _ -> putStrLn $ show pos ++ ": " ++ show e
-- | Print the error as is
ppError _ (AnnError e pos) = putStrLn $ show pos ++ ": " ++ show e
