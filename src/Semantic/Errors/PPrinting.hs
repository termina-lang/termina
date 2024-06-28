-- | Semantic Error Printing
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

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
import Semantic.Types (SemanTypeDef)

class ShowText a where
    showText :: a -> T.Text

instance ShowText Size where
    showText (K (TInteger value DecRepr)) = T.pack $ show value
    showText (K (TInteger value HexRepr)) = T.toUpper . T.pack $ "0x" <> showHex value ""
    showText (K (TInteger value OctalRepr)) = T.pack ("0" <> showOct value "")
    showText (V ident) = T.pack ident

instance ShowText AccessKind where
    showText Mutable = "mut "
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
    showText (AtomicAccess ts) = "AtomicAccess<" <> showText ts <> ">"
    showText (AtomicArrayAccess ts size) = "AtomicArrayAccess<" <> showText ts <> "; " <> showText size <> ">"
    showText (Atomic ts) = "Atomic<" <> showText ts <> ">"
    showText (AtomicArray ts size) = "AtomicArray<" <> showText ts <> "; " <> showText size <> ">"
    showText (Reference ak ts) = "&" <> showText ak <> showText ts
    showText (DynamicSubtype ts) = "dyn " <> showText ts
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

instance ShowText (SemanTypeDef a) where
    showText (Struct ident _ _) = T.pack $ "struct " <> ident
    showText (Enum ident _ _) = T.pack $ "enum " <> ident
    showText (Class TaskClass ident _ _ _) = T.pack $ "task class " <> ident
    showText (Class ResourceClass ident _ _ _) = T.pack $ "resource class " <> ident
    showText (Class HandlerClass ident _ _ _) = T.pack $ "handler class " <> ident
    showText (Class EmitterClass ident _ _ _) = T.pack $ "emitter class " <> ident
    showText (Class ChannelClass ident _ _ _) = T.pack $ "channel class " <> ident
    showText (Interface ident _ _) = T.pack $ "interface " <> ident

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
    ENotCasteable ts1 ts2 -> 
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
                (Just ("Invalid return type \x1b[31m" <> showText ts <> "\x1b[0m."))
    EProcedureCallExtraParams (procId, params, Position procPos) paramNumber ->
        let title = "error[E009]: extra parameters in procedure call."
            procFileName = sourceName procPos
            procLineNumber = sourceLine procPos
            procLineColumn = sourceColumn procPos
            procSourceLines = toModuleAST M.! procFileName
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn (length procId)
                (Just ("Procedure \x1b[31m" <> T.pack procId <> 
                    "\x1b[0m has only \x1b[31m" <> T.pack (show (length params)) <> 
                    "\x1b[0m parameters but you are providing \x1b[31m" <> T.pack (show paramNumber) <> "\x1b[0m.")) >>
            printSimpleError 
                procSourceLines "The interface of the procedure is defined here:" procFileName
                procLineNumber procLineColumn 1
                Nothing
    EProcedureCallMissingParams (procId, params, Position procPos) paramNumber ->
        let title = "error[E010]: missing parameters in procedure call."
            procFileName = sourceName procPos
            procLineNumber = sourceLine procPos
            procLineColumn = sourceColumn procPos
            procSourceLines = toModuleAST M.! procFileName
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn (length procId)
                (Just ("Procedure \x1b[31m" <> T.pack procId <> 
                    "\x1b[0m has \x1b[31m" <> T.pack (show (length params)) <> 
                    "\x1b[0m parameters but you are providing only \x1b[31m" <> T.pack (show paramNumber) <> "\x1b[0m.")) >>
            printSimpleError 
                procSourceLines "The interface of the procedure is defined here:" procFileName
                procLineNumber procLineColumn 1
                Nothing
    EProcedureCallParamTypeMismatch (ident, Parameter param expectedTy, ann) actualTy ->
        let title = "error[E011]: parameter type mismatch in procedure call."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 1
                (Just ("Parameter \x1b[31m" <> T.pack param <> 
                    "\x1b[0m is expected to be of type \x1b[31m" <> showText expectedTy <> 
                    "\x1b[0m but you are providing it of type \x1b[31m" <> showText actualTy <> "\x1b[0m.")) >>
            case ann of
                Position procPos ->
                    let procFileName = sourceName procPos
                        procLineNumber = sourceLine procPos
                        procLineColumn = sourceColumn procPos
                        procSourceLines = toModuleAST M.! procFileName
                    in
                    printSimpleError 
                        procSourceLines 
                            ("The procedure \x1b[31m" <> T.pack ident <> 
                            "\x1b[0m is defined here:") 
                            procFileName
                        procLineNumber procLineColumn 1
                        Nothing
                _ -> return ()
    EUnknownProcedure ident ->
        let title = "error[E012]: unknown procedure."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn (length ident)
                (Just ("Unknown procedure \x1b[31m" <> T.pack ident <> "\x1b[0m."))
    EResourceClassNoProvides ident ->
        let title = "error[E013]: resource class does not provide any interface."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn (length ident)
                (Just ("Resource class \x1b[31m" <> T.pack ident <> "\x1b[0m does not provide any interface.\n" <>
                    "A resource class must provide at least one interface."))
    EResourceClassAction (classId, Position posClass) ident ->
        let title = "error[E014]: resource class defines an action."
        in
            TL.putStrLn $ prettyErrors 
                sourceLines
                [
                    Errata
                        (Just title)
                        [
                            Block
                                fancyRedStyle
                                (sourceName posClass, sourceLine posClass, sourceColumn posClass)
                                Nothing
                                [
                                    Pointer (sourceLine posClass) (sourceColumn posClass) 
                                            (sourceColumn posClass + length classId) 
                                            True Nothing fancyRedPointer,
                                    Pointer lineNumber lineColumn (lineColumn + length ident) 
                                            True (Just " \x1b[31minvalid action definition\x1b[0m") fancyRedPointer
                                ]
                                Nothing
                        ]
                        (Just 
                            ("Resource class \x1b[31m" <> T.pack classId <> "\x1b[0m defines the action \x1b[31m" <> T.pack ident <> "\x1b[0m.\n"
                            <> "Resource classes cannot define actions."))
                ]
    EResourceClassInPort (classId, Position posClass) ident ->
        let title = "error[E015]: resource class defines an in port."
        in
            TL.putStrLn $ prettyErrors 
                sourceLines
                [
                    Errata
                        (Just title)
                        [
                            Block
                                fancyRedStyle
                                (sourceName posClass, sourceLine posClass, sourceColumn posClass)
                                Nothing
                                [
                                    Pointer (sourceLine posClass) (sourceColumn posClass) 
                                            (sourceColumn posClass + length classId) 
                                            True Nothing fancyRedPointer,
                                    Pointer lineNumber lineColumn (lineColumn + length ident) 
                                            True (Just " \x1b[31minvalid port definition\x1b[0m") fancyRedPointer
                                ]
                                Nothing
                        ]
                        (Just 
                            ("Resource class \x1b[31m" <> T.pack classId <> "\x1b[0m defines the in port \x1b[31m" <> T.pack ident <> "\x1b[0m.\n"
                            <> "Resource classes cannot define in ports."))
                ]
    EResourceClassOutPort (classId, Position posClass) ident ->
        let title = "error[E016]: resource class defines an out port."
        in
            TL.putStrLn $ prettyErrors 
                sourceLines
                [
                    Errata
                        (Just title)
                        [
                            Block
                                fancyRedStyle
                                (sourceName posClass, sourceLine posClass, sourceColumn posClass)
                                Nothing
                                [
                                    Pointer (sourceLine posClass) (sourceColumn posClass) 
                                            (sourceColumn posClass + length classId) 
                                            True Nothing fancyRedPointer,
                                    Pointer lineNumber lineColumn (lineColumn + length ident) 
                                            True (Just " \x1b[31minvalid port definition\x1b[0m") fancyRedPointer
                                ]
                                Nothing
                        ]
                        (Just 
                            ("Resource class \x1b[31m" <> T.pack classId <> "\x1b[0m defines the out port \x1b[31m" <> T.pack ident <> "\x1b[0m.\n"
                            <> "Resource classes cannot define out ports."))
                ]
    EInterfaceNotFound ident ->
        let title = "error[E017]: interface not found."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 1
                (Just ("Interface \x1b[31m" <> T.pack ident <> "\x1b[0m not found."))
    EMismatchIdNotInterface ident -> 
        let title = "error[E018]: identifier not an interface."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn (length ident)
                (Just ("The identifier \x1b[31m" <> T.pack ident <> "\x1b[0m is not an interface."))
    EProcedureNotFromProvidedInterfaces (classId, Position posClass) ident ->
        let title = "error[E019]: procedure not from provided interfaces."
        in
            TL.putStrLn $ prettyErrors 
                sourceLines
                [
                    Errata
                        (Just title)
                        [
                            Block
                                fancyRedStyle
                                (sourceName posClass, sourceLine posClass, sourceColumn posClass)
                                Nothing
                                [
                                    Pointer (sourceLine posClass) (sourceColumn posClass) 
                                            (sourceColumn posClass + length classId) 
                                            True Nothing fancyRedPointer,
                                    Pointer lineNumber lineColumn (lineColumn + length ident) 
                                            True (Just " \x1b[31munknown procedure\x1b[0m") fancyRedPointer
                                ]
                                Nothing
                        ]
                        (Just 
                            ("The procedure \x1b[31m" <> T.pack ident 
                                <> "\x1b[0m does not belong to any of the provided interfaces of resource class \x1b[31m" 
                                <> T.pack classId <> "\x1b[0m.\n"))
                ]
    EMissingProcedure ifaceId procId ->
        let title = "error[E020]: missing procedure."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 1
                (Just ("Procedure \x1b[31m" <> T.pack procId <> "\x1b[0m of interface \x1b[31m" <> T.pack ifaceId <> "\x1b[0m is not being provided."))
    EProcedureExtraParams (ifaceId, procId, params, Position procPos) paramNumber ->
        let title = "error[E021]: extra parameters in procedure definition."
            procFileName = sourceName procPos
            procLineNumber = sourceLine procPos
            procLineColumn = sourceColumn procPos
            procSourceLines = toModuleAST M.! procFileName
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 1
                (Just ("Procedure \x1b[31m" <> T.pack procId <> 
                    "\x1b[0m of interface \x1b[31m" <> T.pack ifaceId <> 
                    "\x1b[0m has only \x1b[31m" <> T.pack (show (length params)) <> 
                    "\x1b[0m parameters but you are providing \x1b[31m" <> T.pack (show paramNumber) <> "\x1b[0m.")) >>
            printSimpleError 
                procSourceLines "The interface of the procedure is defined here:" procFileName
                procLineNumber procLineColumn 1
                Nothing
    EProcedureMissingParams (ifaceId, procId, params, Position procPos) paramNumber ->
        let title = "error[E022]: missing parameters in procedure definition."
            procFileName = sourceName procPos
            procLineNumber = sourceLine procPos
            procLineColumn = sourceColumn procPos
            procSourceLines = toModuleAST M.! procFileName
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 1
                (Just ("Procedure \x1b[31m" <> T.pack procId <> 
                    "\x1b[0m of interface \x1b[31m" <> T.pack ifaceId <> 
                    "\x1b[0m has \x1b[31m" <> T.pack (show (length params)) <> 
                    "\x1b[0m parameters but you are providing only \x1b[31m" <> T.pack (show paramNumber) <> "\x1b[0m.")) >>
            printSimpleError 
                procSourceLines "The interface of the procedure is defined here:" procFileName
                procLineNumber procLineColumn 1
                Nothing
    EProcedureParamTypeMismatch (ifaceId, procId, Parameter paramId expectedTy, Position procPos) actualTy ->
        let title = "error[E023]: parameter type mismatch in procedure definition."
            procFileName = sourceName procPos
            procLineNumber = sourceLine procPos
            procLineColumn = sourceColumn procPos
            procSourceLines = toModuleAST M.! procFileName
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 1
                (Just ("Parameter \x1b[31m" <> T.pack paramId <> 
                    "\x1b[0m is expected to be of type \x1b[31m" <> showText expectedTy <> 
                    "\x1b[0m but you are defining it of type \x1b[31m" <> showText actualTy <> "\x1b[0m.")) >>
            printSimpleError 
                procSourceLines 
                    ("The procedure \x1b[31m" <> T.pack procId <> 
                     "\x1b[0m of the interface \x1b[31m" <> T.pack ifaceId <> 
                     "\x1b[0m is defined here:") 
                    procFileName
                procLineNumber procLineColumn 1
                Nothing
    EIfElseIfCondNotBool ts ->
        let title = "error[E024]: if-else-if condition not boolean."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 1
                (Just ("The condition in the statement is expected to be of type \x1b[31mbool\x1b[0m but it is of type \x1b[31m" <> showText ts <> "\x1b[0m."))
    EFunctionCallExtraParams (funcId, params, Position funcPos) paramNumber ->
        let title = "error[E025]: extra parameters in function call."
            funcFileName = sourceName funcPos
            funcLineNumber = sourceLine funcPos
            funcLineColumn = sourceColumn funcPos
            funcSourceLines = toModuleAST M.! funcFileName
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn (length funcId)
                (Just ("Function \x1b[31m" <> T.pack funcId <> 
                    "\x1b[0m has only \x1b[31m" <> T.pack (show (length params)) <> 
                    "\x1b[0m parameters but you are providing \x1b[31m" <> T.pack (show paramNumber) <> "\x1b[0m.")) >>
            printSimpleError 
                funcSourceLines "The interface of the function is defined here:" funcFileName
                funcLineNumber funcLineColumn (length funcId)
                Nothing
    EFunctionCallMissingParams (funcId, params, Position funcPos) paramNumber ->
        let title = "error[E026]: missing parameters in function call."
            funcFileName = sourceName funcPos
            funcLineNumber = sourceLine funcPos
            funcLineColumn = sourceColumn funcPos
            funcSourceLines = toModuleAST M.! funcFileName
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn (length funcId)
                (Just ("Function \x1b[31m" <> T.pack funcId <> 
                    "\x1b[0m has \x1b[31m" <> T.pack (show (length params)) <> 
                    "\x1b[0m parameters but you are providing only \x1b[31m" <> T.pack (show paramNumber) <> "\x1b[0m.")) >>
            printSimpleError 
                funcSourceLines "The interface of the function is defined here:" funcFileName
                funcLineNumber funcLineColumn (length funcId)
                Nothing
    EFunctionCallParamTypeMismatch (funcId, Parameter paramId expectedTy, Position funcPos) actualTy ->
        let title = "error[E027]: parameter type mismatch in function call."
            funcFileName = sourceName funcPos
            funcLineNumber = sourceLine funcPos
            funcLineColumn = sourceColumn funcPos
            funcSourceLines = toModuleAST M.! funcFileName
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn (length funcId)
                (Just ("Parameter \x1b[31m" <> T.pack paramId <> 
                    "\x1b[0m is expected to be of type \x1b[31m" <> showText expectedTy <> 
                    "\x1b[0m but you are providing it of type \x1b[31m" <> showText actualTy <> "\x1b[0m.")) >>
            printSimpleError 
                funcSourceLines 
                    ("The function \x1b[31m" <> T.pack funcId <> 
                     "\x1b[0m is defined here:") 
                    funcFileName
                funcLineNumber funcLineColumn (length funcId)
                Nothing
    EMemberAccessNotFunction ident ->
        let title = "error[E028]: Access to a member that is not a function."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn (length ident)
                (Just ("The identifier \x1b[31m" <> T.pack ident <> "\x1b[0m is not a valid member function."))
    EMutableReferenceToImmutable ->
        let title = "error[E029]: mutable reference to immutable object."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 4 -- ^ Size of "&mut"
                (Just "You are trying to create a mutable reference to an immutable object.")
    EBinOpExpectedTypeLeft op expectedTy actualTy ->
        let title = "error[E030]: Binary operation expected type on the left."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 1
                (Just ("The result of the binary operation \x1b[31m" <> showText op <> 
                    "\x1b[0m is expected to be of type \x1b[31m" <> showText expectedTy <>
                    "\x1b[0m but the left operand you are providing is of type \x1b[31m" <> 
                    showText actualTy <> "\x1b[0m."))
    EBinOpExpectedTypeRight op expectedTy actualTy ->
        let title = "error[E031]: Binary operation expected type on the right."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 1
                (Just ("The result of the binary operation \x1b[31m" <> showText op <> 
                    "\x1b[0m is expected to be of type \x1b[31m" <> showText expectedTy <>
                    "\x1b[0m but the right operand you are providing is of type \x1b[31m" <> 
                    showText actualTy <> "\x1b[0m."))
    EBinOpTypeMismatch op ty_le ty_re ->
        let title = "error[E032]: binary operation type mismatch."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn (T.length (showText op))
                (Just ("Binary operation \x1b[31m" <> showText op <> 
                    "\x1b[0m expects operands of the same type but the left one is of type \x1b[31m" <>
                    showText ty_le <> "\x1b[0m and the right one is of type \x1b[31m" <> showText ty_re <> "\x1b[0m."))
    EBinOpExpectedTypeNotBool op ty ->
        let title = "error[E033]: binary operation expected result type not boolean."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn (T.length (showText op))
                (Just ("The binary operation \x1b[31m" <> showText op <> 
                    "will result in a value of type \x1b[31m" <> showText Bool <> 
                    "\x1b[0m but it is expected to be of type \x1b[31m" <> showText ty <> "\x1b[0m."))
    EBinOpLeftTypeNotBool op ty ->
        let title = "error[E034]: binary operation expected boolean type on the left."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 1
                (Just ("The left operand of the binary operation \x1b[31m" <> showText op <> 
                    "\x1b[0m is of type \x1b[31m" <> showText ty <> 
                    "\x1b[0m but it is expected to be of type \x1b[31m" <> showText Bool <> "\x1b[0m."))
    EBinOpRightTypeNotBool op ty ->
        let title = "error[E035]: binary operation expected boolean type on the right."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 1
                (Just ("The right operand of the binary operation \x1b[31m" <> showText op <> 
                    "\x1b[0m is of type \x1b[31m" <> showText ty <> 
                    "\x1b[0m but it is expected to be of type \x1b[31m" <> showText Bool <> "\x1b[0m."))
    EBinOpExpectedTypeNotNum op ty ->
        let title = "error[E036]: binary operation expected result type not numeric."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn (T.length (showText op))
                (Just ("The binary operation \x1b[31m" <> showText op <> 
                    "\x1b[0m will result in a numeric value but the expected type is \x1b[31m" <> showText ty <> "\x1b[0m."))
    EBinOpLeftTypeNotNum op ty ->
        let title = "error[E037]: binary operation expected numeric type on the left."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 1
                (Just ("The left operand of the binary operation \x1b[31m" <> showText op <> 
                    "\x1b[0m is of type \x1b[31m" <> showText ty <> 
                    "\x1b[0m but it is expected to be of numeric type."))
    EBinOpRightTypeNotNum op ty ->
        let title = "error[E038]: binary operation expected numeric type on the right."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 1
                (Just ("The right operand of the binary operation \x1b[31m" <> showText op <> 
                    "\x1b[0m is of type \x1b[31m" <> showText ty <> 
                    "\x1b[0m but it is expected to be of numeric type."))
    EBinOpRightTypeNotPos op ty ->
        let title = "error[E039]: binary operation expected positive numeric type on the right."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 1
                (Just ("The right operand of the binary operation \x1b[31m" <> showText op <> 
                    "\x1b[0m is of type \x1b[31m" <> showText ty <> 
                    "\x1b[0m but it is expected to be of positive numeric type."))
    EBinOpLeftTypeNotEquatable op ty ->
        let title = "error[E040]: binary operation expected equatable type on the left."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 1
                (Just ("The left operand of the binary operation \x1b[31m" <> showText op <> 
                    "\x1b[0m is of type \x1b[31m" <> showText ty <> 
                    "\x1b[0m but it is expected to be of equatable type."))
    EBinOpRightTypeNotEquatable op ty ->
        let title = "error[E041]: binary operation expected equatable type on the right."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 1
                (Just ("The right operand of the binary operation \x1b[31m" <> showText op <> 
                    "\x1b[0m is of type \x1b[31m" <> showText ty <> 
                    "\x1b[0m but it is expected to be of equatable type."))
    EAtomicAccessInvalidType ty ->
        let title = "error[E042]: invalid type for the atomic access interface."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 1
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not valid for atomic access."))
    EAtomicArrayAccessInvalidType ty ->
        let title = "error[E043]: invalid type for the atomic array access interface."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 1
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not valid for atomic array access."))
    EAtomicInvalidType ty ->
        let title = "error[E044]: invalid atomic type."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 1
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not valid for atomic."))
    EAtomicArrayInvalidType ty ->
        let title = "error[E045]: invalid atomic array type."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 1
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not valid for atomic array."))
    EAtomicConnectionTypeMismatch expectedTy actualTy ->
        let title = "error[E046]: atomic connection type mismatch."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 1
                (Just ("The type of the connected atomic resource is expected to be \x1b[31m" <> showText expectedTy <> 
                    "\x1b[0m but it is of type \x1b[31m" <> showText actualTy <> "\x1b[0m."))
    EAtomicArrayConnectionTypeMismatch expectedTy actualTy ->
        let title = "error[E047]: atomic array connection type mismatch."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 1
                (Just ("The type of the elements of the connected atomic array is expected to be \x1b[31m" <> showText expectedTy <> 
                    "\x1b[0m but the array is of elements of type \x1b[31m" <> showText actualTy <> "\x1b[0m."))
    EAtomicArrayConnectionSizeMismatch expectedSize actualSize ->
        let title = "error[E048]: atomic array connection size mismatch."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 1
                (Just ("The size of the connected atomic array is expected to be \x1b[31m" <> showText expectedSize <> 
                    "\x1b[0m but the array has size \x1b[31m" <> showText actualSize <> "\x1b[0m."))
    EConstantWithoutKnownType c ->
        let title = "error[E049]: constant without known type."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 1
                (Just ("The type of the constant \x1b[31m" <> showText c <> 
                    "\x1b[0m cannot be inferred from the environment and must be explicitly defined."))
    EStructInitializerInvalidUse ->
        let title = "error[E050]: invalid use of struct initializer."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 1
                (Just $ "You are trying to use a struct initializer in an invalid context.\n" <>
                        "Struct initializers can only be used to initialize struct objects.")
    EStructInitializerTypeMismatch expectedTy actualTy ->
        let title = "error[E051]: struct initializer type mismatch."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 1
                (Just ("The struct initializer is expected to be of type \x1b[31m" <> showText expectedTy <> 
                    "\x1b[0m but it is of type \x1b[31m" <> showText actualTy <> "\x1b[0m."))
    EStructInitializerGlobalNotStruct tydef ->
        let title = "error[E052]: struct initializer expected global type not struct."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 1
                (Just ("Invalid use of a struct initializer.\n" <> 
                    "You are using a struct initializer but the expected type is \x1b[31m" <> 
                    showText tydef <> "\x1b[0m."))
    EStructInitializerExpectedTypeNotStruct ty ->
        let title = "error[E053]: struct initializer expected type not struct."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 1
                (Just ("Invalid use of a struct initializer.\n" <> 
                    "You are using a struct initializer but the expected type is \x1b[31m" <> 
                    showText ty <> "\x1b[0m."))
    EStructInitializerUnknownType ident ->
        let title = "error[E054]: struct initializer unknown type."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 1
                (Just ("The type \x1b[31m" <> T.pack ident <> "\x1b[0m of the struct initializer is unknown."))
    ESliceInvalidUse ->
        let title = "error[E055]: invalid use of slice."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 1
                (Just $ "You are trying to use a slice in an invalid context.\n" <>
                        "Slices can only be used to create references to a part of an array.")
    EArrayIntitalizerInvalidUse ->
        let title = "error[E056]: invalid use of an array initializer."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 1
                (Just $ "You are trying to use an array initializer in an invalid context.\n" <>
                        "Array initializers can only be used to initialize array objects.")
    EArrayExprListIntitalizerInvalidUse -> 
        let title = "error[E057]: invalid use of an expression list array initializer."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 1
                (Just $ "You are trying to use an array expression list initializer in an invalid context.\n" <>
                        "Array expression list initializers can only be used to initialize array objects.")
    EOptionVariantInitializerInvalidUse ->
        let title = "error[E058]: invalid use of an option variant initializer."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 1
                (Just $ "You are trying to use an option variant initializer in an invalid context.\n" <>
                        "Option variant initializers can only be used to initialize option objects.")
    EArrayInitializerSizeMismatch expectedSize initializerSize ->
        let title = "error[E059]: array initializer size mismatch."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 1
                (Just ("The size of the array initializer is \x1b[31m" <> showText initializerSize <> 
                    "\x1b[0m but the expected size is \x1b[31m" <> showText expectedSize <> "\x1b[0m."))
    EArrayExprListInitializerSizeMismatch expectedSize initializerSize ->
        let title = "error[E060]: array expression list initializer size mismatch."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 1
                (Just ("The size of the array expression list initializer is \x1b[31m" <> T.pack (show initializerSize) <>
                    "\x1b[0m but the expected size is \x1b[31m" <> T.pack (show expectedSize) <> "\x1b[0m."))
    EArrayExprListInitializerExprTypeMismatch expectedTy actualTy ->
        let title = "error[E061]: array expression list initializer expression type mismatch."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 1
                (Just ("The expression in the array expression list initializer is expected to be of type \x1b[31m" <> showText expectedTy <> 
                    "\x1b[0m but it is of type \x1b[31m" <> showText actualTy <> "\x1b[0m."))
    EReturnValueExpected ty ->
        let title = "error[E062]: expected return value."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 1
                (Just ("The function is expected to return a value of type \x1b[31m" <> showText ty <> "\x1b[0m."))
    EReturnValueNotUnit ->
        let title = "error[E063]: return value not expected."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 1
                (Just "The function is not expected to return a value.")
    _ -> putStrLn $ show pos ++ ": " ++ show e
-- | Print the error as is
ppError _ (AnnError e pos) = putStrLn $ show pos ++ ": " ++ show e
