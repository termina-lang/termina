-- | Semantic Error Printing
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Semantic.Errors.PPrinting where

import Semantic.Errors.Errors

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import Text.Parsec.Pos
import Errata
import Errata.Styles
import Semantic.AST
import qualified Data.Map as M
import Utils.Annotations
import Utils.Errors



-- useful prettyprinter doc
-- https://hackage.haskell.org/package/prettyprinter-1.7.1/docs/Prettyprinter.html
ppError :: M.Map FilePath TL.Text ->
    SemanticErrors -> IO ()
ppError toModuleAST (AnnotatedError e pos@(Position start end)) =
  let fileName = sourceName start
      sourceLines = toModuleAST M.! fileName
  in
  case e of
    (EInvalidArrayIndexing ts) -> 
        let title = "\x1b[31merror [E001]\x1b[0m: invalid array indexing."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Invalid array indexing of type \x1b[31m" <> showText ts <> "\x1b[0m.")) 
    (ENotNamedObject ident) ->
        let title = "\x1b[31merror [E002]\x1b[0m: Object not found."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The variable \x1b[31m" <> T.pack ident <> "\x1b[0m has not been declared"))
    (ENotConstant ident) ->
        let
            title = "\x1b[31merror [E003]\x1b[0m: invalid use of a non-constant object."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The object \x1b[31m" <> T.pack ident <> "\x1b[0m is not a constant."))
    EAssignmentToImmutable ->
        let title = "\x1b[31merror [E004]\x1b[0m: assignment to immutable variable."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just "You are trying to assign a value to an immutable object.")
    EIfElseNoOtherwise ->
        let title = "\x1b[31merror [E005]\x1b[0m: missing else clause."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("You are missing the else clause in an if-else-if statement.\n" <>
                    "You must provide an else clause if you are defining an else-if clause."))
    ENotCasteable ts1 ts2 ->
        let title = "\x1b[31merror [E006]\x1b[0m: invalid cast."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("You cannot cast a value of type \x1b[31m" <> showText ts1 <> "\x1b[0m to type \x1b[31m" <> showText ts2 <> "\x1b[0m."))
    EInvalidParameterType (Parameter ident ts) -> 
        let title = "\x1b[31merror [E007]\x1b[0m: invalid parameter type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Parameter \x1b[31m" <> T.pack ident <> "\x1b[0m has an invalid type \x1b[31m" <> showText ts <> "\x1b[0m."))
    EInvalidReturnType ts ->
        let title = "\x1b[31merror [E008]\x1b[0m: invalid return type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Invalid return type \x1b[31m" <> showText ts <> "\x1b[0m."))
    EProcedureCallExtraParams (procId, params, procPos@(Position procStart _procEnd)) paramNumber ->
        let title = "\x1b[31merror [E009]\x1b[0m: extra parameters in procedure call."
            procFileName = sourceName procStart
            procSourceLines = toModuleAST M.! procFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Procedure \x1b[31m" <> T.pack procId <>
                    "\x1b[0m has only \x1b[31m" <> T.pack (show (length params)) <>
                    "\x1b[0m parameters but you are providing \x1b[31m" <> T.pack (show paramNumber) <> "\x1b[0m.")) >>
            printSimpleError
                procSourceLines "The interface of the procedure is defined here:" procFileName
                procPos Nothing
    EProcedureCallMissingParams (ident, params, procPos@(Position procStart _procEnd)) paramNumber ->
        let title = "\x1b[31merror [E010]\x1b[0m: missing parameters in procedure call."
            procFileName = sourceName procStart
            procSourceLines = toModuleAST M.! procFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Procedure \x1b[31m" <> T.pack ident <>
                    "\x1b[0m has \x1b[31m" <> T.pack (show (length params)) <>
                    "\x1b[0m parameters but you are providing only \x1b[31m" <> T.pack (show paramNumber) <> "\x1b[0m.")) >>
            printSimpleError
                procSourceLines
                ("Procedure \x1b[31m" <> T.pack ident <> "\x1b[0m is defined here:")
                procFileName procPos Nothing
    EProcedureCallParamTypeMismatch (ident, expectedTy, procPos@(Position procStart _procEnd)) actualTy ->
        let title = "\x1b[31merror [E011]\x1b[0m: parameter type mismatch in procedure call."
            procFileName = sourceName procStart
            procSourceLines = toModuleAST M.! procFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Parameter is expected to be of type \x1b[31m" <> showText expectedTy <>
                    "\x1b[0m but you are providing it of type \x1b[31m" <> showText actualTy <> "\x1b[0m.")) >>
            printSimpleError
                procSourceLines
                ("Procedure \x1b[31m" <> T.pack ident <> "\x1b[0m is defined here:")
                procFileName procPos Nothing
    EUnknownProcedure ident ->
        let title = "\x1b[31merror [E012]\x1b[0m: unknown procedure."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Unknown procedure \x1b[31m" <> T.pack ident <> "\x1b[0m."))
    EResourceClassNoProvides ident ->
        let title = "\x1b[31merror [E013]\x1b[0m: resource class does not provide any interface."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Resource class \x1b[31m" <> T.pack ident <> "\x1b[0m does not provide any interface.\n" <>
                       "A resource class must provide at least one interface."))
    EResourceClassAction (classId, Position startPosClass endPosClass) ident ->
        let title = "\x1b[31merror [E014]\x1b[0m: resource class defines an action."
            actionStartLine = sourceLine start
            actionEndLine = sourceLine end
            actionStartColumn = sourceColumn start
            actionEndColumn = 
                if actionStartLine == actionEndLine then 
                    sourceColumn end 
                else 
                    fromIntegral $ TL.length (TL.lines sourceLines !! (actionStartLine - 1)) + 1
            classStartLine = sourceLine startPosClass
            classEndLine = sourceLine endPosClass
            classStartColumn = sourceColumn startPosClass
            classEndColumn = 
                if classStartLine == classEndLine then 
                    sourceColumn endPosClass 
                else 
                    fromIntegral $ TL.length (TL.lines sourceLines !! (classStartLine - 1)) + 1

        in
            TLIO.putStrLn $ prettyErrors
                sourceLines
                [
                    Errata
                        (Just title)
                        [
                            Errata.Block
                                fancyRedStyle
                                (sourceName start, classStartLine, classStartColumn)
                                Nothing
                                [
                                    Pointer classStartLine classStartColumn
                                            classEndColumn
                                            True Nothing fancyRedPointer,
                                    Pointer actionStartLine actionStartColumn actionEndColumn
                                            True (Just " \x1b[31minvalid action definition\x1b[0m") fancyRedPointer
                                ]
                                Nothing
                        ]
                        (Just
                            ("Resource class \x1b[31m" <> T.pack classId <> "\x1b[0m defines the action \x1b[31m" <> T.pack ident <> "\x1b[0m.\n"
                            <> "Resource classes cannot define actions."))
                ]
    EResourceClassInPort (classId, Position startPosClass endPosClass) ident ->
        let title = "\x1b[31merror [E015]\x1b[0m: resource class defines an in port."
            portStartLine = sourceLine start
            portEndLine = sourceLine end
            portStartColumn = sourceColumn start
            portEndColumn = 
                if portStartLine == portEndLine then 
                    sourceColumn end 
                else 
                    fromIntegral $ TL.length (TL.lines sourceLines !! (portStartLine - 1)) + 1
            classStartLine = sourceLine startPosClass
            classEndLine = sourceLine endPosClass
            classStartColumn = sourceColumn startPosClass
            classEndColumn = 
                if classStartLine == classEndLine then 
                    sourceColumn endPosClass 
                else 
                    fromIntegral $ TL.length (TL.lines sourceLines !! (classStartLine - 1)) + 1

        in
            TLIO.putStrLn $ prettyErrors
                sourceLines
                [
                    Errata
                        (Just title)
                        [
                            Errata.Block
                                fancyRedStyle
                                (sourceName start, classStartLine, classStartColumn)
                                Nothing
                                [
                                    Pointer classStartLine classStartColumn
                                            classEndColumn
                                            True Nothing fancyRedPointer,
                                    Pointer portStartLine portStartColumn portEndColumn
                                            True (Just " \x1b[31minvalid port definition\x1b[0m") fancyRedPointer
                                ]
                                Nothing
                        ]
                        (Just
                            ("Resource class \x1b[31m" <> T.pack classId <> "\x1b[0m defines the in port \x1b[31m" <> T.pack ident <> "\x1b[0m.\n"
                            <> "Resource classes cannot define in ports."))
                ]
    EResourceClassOutPort (classId, Position startPosClass endPosClass) ident ->
        let title = "\x1b[31merror [E016]\x1b[0m: resource class defines an out port."
            portStartLine = sourceLine start
            portEndLine = sourceLine end
            portStartColumn = sourceColumn start
            portEndColumn = 
                if portStartLine == portEndLine then 
                    sourceColumn end 
                else 
                    fromIntegral $ TL.length (TL.lines sourceLines !! (portStartLine - 1)) + 1
            classStartLine = sourceLine startPosClass
            classEndLine = sourceLine endPosClass
            classStartColumn = sourceColumn startPosClass
            classEndColumn = 
                if classStartLine == classEndLine then 
                    sourceColumn endPosClass 
                else 
                    fromIntegral $ TL.length (TL.lines sourceLines !! (classStartLine - 1)) + 1

        in
            TLIO.putStrLn $ prettyErrors
                sourceLines
                [
                    Errata
                        (Just title)
                        [
                            Errata.Block
                                fancyRedStyle
                                (sourceName start, classStartLine, classStartColumn)
                                Nothing
                                [
                                    Pointer classStartLine classStartColumn
                                            classEndColumn
                                            True Nothing fancyRedPointer,
                                    Pointer portStartLine portStartColumn portEndColumn
                                            True (Just " \x1b[31minvalid port definition\x1b[0m") fancyRedPointer
                                ]
                                Nothing
                        ]
                        (Just
                            ("Resource class \x1b[31m" <> T.pack classId <> "\x1b[0m defines the out port \x1b[31m" <> T.pack ident <> "\x1b[0m.\n"
                            <> "Resource classes cannot define out ports."))
                ]
    EInterfaceNotFound ident ->
        let title = "\x1b[31merror [E017]\x1b[0m: interface not found."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Interface \x1b[31m" <> T.pack ident <> "\x1b[0m not found."))
    EMismatchIdNotInterface ident ->
        let title = "\x1b[31merror [E018]\x1b[0m: identifier not an interface."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The identifier \x1b[31m" <> T.pack ident <> "\x1b[0m is not an interface."))
    EProcedureNotFromProvidedInterfaces (classId, Position posClass _endPosClass) ident ->
        let title = "\x1b[31merror [E019]\x1b[0m: procedure not from provided interfaces."
        in
            TLIO.putStrLn $ prettyErrors
                sourceLines
                [
                    Errata
                        (Just title)
                        [
                            Errata.Block
                                fancyRedStyle
                                (sourceName posClass, sourceLine posClass, sourceColumn posClass)
                                Nothing
                                [
                                    Pointer (sourceLine posClass) (sourceColumn posClass)
                                            (sourceColumn posClass + length classId)
                                            True Nothing fancyRedPointer,
                                    Pointer (sourceLine start) (sourceColumn start) (sourceColumn start + length ident)
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
        let title = "\x1b[31merror [E020]\x1b[0m: missing procedure."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Procedure \x1b[31m" <> T.pack procId <> "\x1b[0m of interface \x1b[31m" <> T.pack ifaceId <> "\x1b[0m is not being provided."))
    EProcedureExtraParams (ifaceId, procId, params, procPos@(Position procStart _procEnd)) paramNumber ->
        let title = "\x1b[31merror [E021]\x1b[0m: extra parameters in procedure definition."
            procFileName = sourceName procStart
            procSourceLines = toModuleAST M.! procFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Procedure \x1b[31m" <> T.pack procId <>
                    "\x1b[0m of interface \x1b[31m" <> T.pack ifaceId <>
                    "\x1b[0m has only \x1b[31m" <> T.pack (show (length params)) <>
                    "\x1b[0m parameters but you are providing \x1b[31m" <> T.pack (show paramNumber) <> "\x1b[0m.")) >>
            printSimpleError
                procSourceLines "The interface of the procedure is defined here:" procFileName
                procPos Nothing
    EProcedureMissingParams (ifaceId, procId, params, procPos@(Position procStart _procEnd)) paramNumber ->
        let title = "\x1b[31merror [E022]\x1b[0m: missing parameters in procedure definition."
            procFileName = sourceName procStart
            procSourceLines = toModuleAST M.! procFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Procedure \x1b[31m" <> T.pack procId <>
                    "\x1b[0m of interface \x1b[31m" <> T.pack ifaceId <>
                    "\x1b[0m has \x1b[31m" <> T.pack (show (length params)) <>
                    "\x1b[0m parameters but you are providing only \x1b[31m" <> T.pack (show paramNumber) <> "\x1b[0m.")) >>
            printSimpleError
                procSourceLines "The interface of the procedure is defined here:" procFileName
                procPos Nothing
    EProcedureParamTypeMismatch (ifaceId, procId, expectedTy, procPos@(Position procStart _procEnd)) actualTy ->
        let title = "\x1b[31merror [E023]\x1b[0m: parameter type mismatch in procedure definition."
            procFileName = sourceName procStart
            procSourceLines = toModuleAST M.! procFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Parameter is expected to be of type \x1b[31m" <> showText expectedTy <>
                    "\x1b[0m but you are defining it of type \x1b[31m" <> showText actualTy <> "\x1b[0m.")) >>
            printSimpleError
                procSourceLines
                    ("The procedure \x1b[31m" <> T.pack procId <>
                     "\x1b[0m of the interface \x1b[31m" <> T.pack ifaceId <>
                     "\x1b[0m is defined here:")
                    procFileName procPos Nothing
    EIfElseIfCondNotBool ts ->
        let title = "\x1b[31merror [E024]\x1b[0m: if-else-if condition not boolean."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The condition in the statement is expected to be of type \x1b[31mbool\x1b[0m but it is of type \x1b[31m" <> showText ts <> "\x1b[0m."))
    EFunctionCallExtraParams (funcId, params, funcPos@(Position funcStart _procEnd)) paramNumber ->
        let title = "\x1b[31merror [E025]\x1b[0m: extra parameters in function call."
            funcFileName = sourceName funcStart
            funcSourceLines = toModuleAST M.! funcFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Function \x1b[31m" <> T.pack funcId <>
                    "\x1b[0m has only \x1b[31m" <> T.pack (show (length params)) <>
                    "\x1b[0m parameters but you are providing \x1b[31m" <> T.pack (show paramNumber) <> "\x1b[0m.")) >>
            printSimpleError
                funcSourceLines ("Function \x1b[31m" <> T.pack funcId <> "\x1b[0m is defined here:") funcFileName
                funcPos Nothing
    EFunctionCallMissingParams (funcId, params, funcPos@(Position funcStart _procEnd)) paramNumber ->
        let title = "\x1b[31merror [E026]\x1b[0m: missing parameters in function call."
            funcFileName = sourceName funcStart
            funcSourceLines = toModuleAST M.! funcFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Function \x1b[31m" <> T.pack funcId <>
                    "\x1b[0m has \x1b[31m" <> T.pack (show (length params)) <>
                    "\x1b[0m parameters but you are providing only \x1b[31m" <> T.pack (show paramNumber) <> "\x1b[0m.")) >>
            printSimpleError
                funcSourceLines ("Function \x1b[31m" <> T.pack funcId <> "\x1b[0m is defined here:") funcFileName
                funcPos Nothing
    EFunctionCallParamTypeMismatch (funcId, expectedTy, funcPos@(Position funcStart _procEnd)) actualTy ->
        let title = "\x1b[31merror [E027]\x1b[0m: parameter type mismatch in function call."
            funcFileName = sourceName funcStart
            funcSourceLines = toModuleAST M.! funcFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Parameter is expected to be of type \x1b[31m" <> showText expectedTy <>
                    "\x1b[0m but you are providing it of type \x1b[31m" <> showText actualTy <> "\x1b[0m.")) >>
            printSimpleError
                funcSourceLines ("Function \x1b[31m" <> T.pack funcId <> "\x1b[0m is defined here:") funcFileName
                funcPos Nothing
    EMemberAccessNotFunction ident ->
        let title = "\x1b[31merror [E028]\x1b[0m: Access to a member that is not a function."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The identifier \x1b[31m" <> T.pack ident <> "\x1b[0m is not a valid member function."))
    EMutableReferenceToImmutable ->
        let title = "\x1b[31merror [E029]\x1b[0m: mutable reference to immutable object."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just "You are trying to create a mutable reference to an immutable object.")
    EMutableReferenceToPrivate ->
        let title = "\x1b[31merror [E030]\x1b[0m: mutable reference to private object."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just "You are trying to create a mutable reference to a private object.")
    EBinOpExpectedTypeLeft op expectedTy actualTy ->
        let title = "\x1b[31merror [E031]\x1b[0m: Binary operation expected type on the left."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The result of the binary operation \x1b[31m" <> showText op <>
                    "\x1b[0m is expected to be of type \x1b[31m" <> showText expectedTy <>
                    "\x1b[0m but the left operand you are providing is of type \x1b[31m" <>
                    showText actualTy <> "\x1b[0m."))
    EBinOpExpectedTypeRight op expectedTy actualTy ->
        let title = "\x1b[31merror [E032]\x1b[0m: Binary operation expected type on the right."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The result of the binary operation \x1b[31m" <> showText op <>
                    "\x1b[0m is expected to be of type \x1b[31m" <> showText expectedTy <>
                    "\x1b[0m but the right operand you are providing is of type \x1b[31m" <>
                    showText actualTy <> "\x1b[0m."))
    EBinOpTypeMismatch op ty_le ty_re ->
        let title = "\x1b[31merror [E033]\x1b[0m: binary operation type mismatch."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Binary operation \x1b[31m" <> showText op <>
                    "\x1b[0m expects operands of the same type but the left one is of type \x1b[31m" <>
                    showText ty_le <> "\x1b[0m and the right one is of type \x1b[31m" <> showText ty_re <> "\x1b[0m."))
    EBinOpExpectedTypeNotBool op ty ->
        let title = "\x1b[31merror [E034]\x1b[0m: binary operation expected result type not boolean."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The binary operation \x1b[31m" <> showText op <>
                    "will result in a value of type \x1b[31m" <> showText Bool <>
                    "\x1b[0m but it is expected to be of type \x1b[31m" <> showText ty <> "\x1b[0m."))
    EBinOpLeftTypeNotBool op ty ->
        let title = "\x1b[31merror [E035]\x1b[0m: binary operation expected boolean type on the left."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The left operand of the binary operation \x1b[31m" <> showText op <>
                    "\x1b[0m is of type \x1b[31m" <> showText ty <>
                    "\x1b[0m but it is expected to be of type \x1b[31m" <> showText Bool <> "\x1b[0m."))
    EBinOpRightTypeNotBool op ty ->
        let title = "\x1b[31merror [E036]\x1b[0m: binary operation expected boolean type on the right."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The right operand of the binary operation \x1b[31m" <> showText op <>
                    "\x1b[0m is of type \x1b[31m" <> showText ty <>
                    "\x1b[0m but it is expected to be of type \x1b[31m" <> showText Bool <> "\x1b[0m."))
    EBinOpExpectedTypeNotNum op ty ->
        let title = "\x1b[31merror [E037]\x1b[0m: binary operation expected result type not numeric."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The binary operation \x1b[31m" <> showText op <>
                    "\x1b[0m will result in a numeric value but the expected type is \x1b[31m" <> showText ty <> "\x1b[0m."))
    EBinOpLeftTypeNotNum op ty ->
        let title = "\x1b[31merror [E038]\x1b[0m: binary operation expected numeric type on the left."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The left operand of the binary operation \x1b[31m" <> showText op <>
                    "\x1b[0m is of type \x1b[31m" <> showText ty <>
                    "\x1b[0m but it is expected to be of numeric type."))
    EBinOpRightTypeNotNum op ty ->
        let title = "\x1b[31merror [E039]\x1b[0m: binary operation expected numeric type on the right."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The right operand of the binary operation \x1b[31m" <> showText op <>
                    "\x1b[0m is of type \x1b[31m" <> showText ty <>
                    "\x1b[0m but it is expected to be of numeric type."))
    EBinOpRightTypeNotPos op ty ->
        let title = "\x1b[31merror [E040]\x1b[0m: binary operation expected positive numeric type on the right."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The right operand of the binary operation \x1b[31m" <> showText op <>
                    "\x1b[0m is of type \x1b[31m" <> showText ty <>
                    "\x1b[0m but it is expected to be of positive numeric type."))
    EBinOpLeftTypeNotEquatable op ty ->
        let title = "\x1b[31merror [E041]\x1b[0m: binary operation expected equatable type on the left."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The left operand of the binary operation \x1b[31m" <> showText op <>
                    "\x1b[0m is of type \x1b[31m" <> showText ty <>
                    "\x1b[0m but it is expected to be of equatable type."))
    EBinOpRightTypeNotEquatable op ty ->
        let title = "\x1b[31merror [E042]\x1b[0m: binary operation expected equatable type on the right."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The right operand of the binary operation \x1b[31m" <> showText op <>
                    "\x1b[0m is of type \x1b[31m" <> showText ty <>
                    "\x1b[0m but it is expected to be of equatable type."))
    EAtomicAccessInvalidType ty ->
        let title = "\x1b[31merror [E043]\x1b[0m: invalid type for the atomic access interface."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not valid for atomic access."))
    EAtomicArrayAccessInvalidType ty ->
        let title = "\x1b[31merror [E044]\x1b[0m: invalid type for the atomic array access interface."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not valid for atomic array access."))
    EAtomicInvalidType ty ->
        let title = "\x1b[31merror [E045]\x1b[0m: invalid atomic type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not valid for atomic."))
    EAtomicArrayInvalidType ty ->
        let title = "\x1b[31merror [E046]\x1b[0m: invalid atomic array type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not valid for atomic array."))
    EAtomicConnectionTypeMismatch expectedTy actualTy ->
        let title = "\x1b[31merror [E047]\x1b[0m: atomic connection type mismatch."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type of the connected atomic resource is expected to be \x1b[31m" <> showText expectedTy <>
                    "\x1b[0m but it is of type \x1b[31m" <> showText actualTy <> "\x1b[0m."))
    EAtomicArrayConnectionTypeMismatch expectedTy actualTy ->
        let title = "\x1b[31merror [E048]\x1b[0m: atomic array connection type mismatch."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type of the elements of the connected atomic array is expected to be \x1b[31m" <> showText expectedTy <>
                    "\x1b[0m but the array is of elements of type \x1b[31m" <> showText actualTy <> "\x1b[0m."))
    EAtomicArrayConnectionSizeMismatch expectedSize actualSize ->
        let title = "\x1b[31merror [E049]\x1b[0m: atomic array connection size mismatch."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The size of the connected atomic array is expected to be \x1b[31m" <> showText expectedSize <>
                    "\x1b[0m but the array has size \x1b[31m" <> showText actualSize <> "\x1b[0m."))
    EConstantWithoutKnownType c ->
        let title = "\x1b[31merror [E050]\x1b[0m: constant without known type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type of the constant \x1b[31m" <> showText c <>
                    "\x1b[0m cannot be inferred from the environment and must be explicitly defined."))
    EStructInitializerInvalidUse ->
        let title = "\x1b[31merror [E051]\x1b[0m: invalid use of struct initializer."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just $ "You are trying to use a struct initializer in an invalid context.\n" <>
                        "Struct initializers can only be used to initialize struct objects.")
    EStructInitializerTypeMismatch expectedTy actualTy ->
        let title = "\x1b[31merror [E052]\x1b[0m: struct initializer type mismatch."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The struct initializer is expected to be of type \x1b[31m" <> showText expectedTy <>
                    "\x1b[0m but it is of type \x1b[31m" <> showText actualTy <> "\x1b[0m."))
    EStructInitializerGlobalNotStruct tydef ->
        let title = "\x1b[31merror [E053]\x1b[0m: struct initializer expected global type not struct."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Invalid use of a struct initializer.\n" <>
                    "You are using a struct initializer but the expected type is \x1b[31m" <>
                    showText tydef <> "\x1b[0m."))
    EStructInitializerExpectedTypeNotStruct ty ->
        let title = "\x1b[31merror [E054]\x1b[0m: struct initializer expected type not struct."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Invalid use of a struct initializer.\n" <>
                    "You are using a struct initializer but the expected type is \x1b[31m" <>
                    showText ty <> "\x1b[0m."))
    EStructInitializerUnknownType ident ->
        let title = "\x1b[31merror [E055]\x1b[0m: struct initializer unknown type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> T.pack ident <> "\x1b[0m of the struct initializer is unknown."))
    ESliceInvalidUse ->
        let title = "\x1b[31merror [E056]\x1b[0m: invalid use of slice."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just $ "You are trying to use a slice in an invalid context.\n" <>
                        "Slices can only be used to create references to a part of an array.")
    EArrayInitializerInvalidUse ->
        let title = "\x1b[31merror [E057]\x1b[0m: invalid use of an array initializer."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just $ "You are trying to use an array initializer in an invalid context.\n" <>
                        "Array initializers can only be used to initialize array objects.")
    EArrayInitializerNotArray ty ->
        let title = "\x1b[31merror [E058]\x1b[0m: assignment of an array initializer to a non-array type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Invalid use of an array initializer.\n" <>
                    "You are trying to assign an array initializer to a non-array type \x1b[31m" <>
                    showText ty <> "\x1b[0m."))
    EArrayExprListInitializerInvalidUse ->
        let title = "\x1b[31merror [E059]\x1b[0m: invalid use of an expression list array initializer."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just $ "You are trying to use an array expression list initializer in an invalid context.\n" <>
                        "Array expression list initializers can only be used to initialize array objects.")
    EArrayExprListInitializerNotArray ty ->
        let title = "\x1b[31merror [E060]\x1b[0m: assignment of an array expression list initializer to a non-array type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Invalid use of an array expression list initializer.\n" <>
                    "You are trying to assign an array expression list initializer to a non-array type \x1b[31m" <>
                    showText ty <> "\x1b[0m."))
    EOptionVariantInitializerInvalidUse ->
        let title = "\x1b[31merror [E061]\x1b[0m: invalid use of an option variant initializer."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just $ "You are trying to use an option variant initializer in an invalid context.\n" <>
                        "Option variant initializers can only be used to initialize option objects.")
    EArrayInitializerSizeMismatch expectedSize initializerSize ->
        let title = "\x1b[31merror [E062]\x1b[0m: array initializer size mismatch."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The size of the array initializer is \x1b[31m" <> showText initializerSize <>
                    "\x1b[0m but the expected size is \x1b[31m" <> showText expectedSize <> "\x1b[0m."))
    EArrayExprListInitializerSizeMismatch expectedSize initializerSize ->
        let title = "\x1b[31merror [E063]\x1b[0m: array expression list initializer size mismatch."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The size of the array expression list initializer is \x1b[31m" <> T.pack (show initializerSize) <>
                    "\x1b[0m but the expected size is \x1b[31m" <> T.pack (show expectedSize) <> "\x1b[0m."))
    EArrayExprListInitializerExprTypeMismatch expectedTy actualTy ->
        let title = "\x1b[31merror [E064]\x1b[0m: array expression list initializer expression type mismatch."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The expression in the array expression list initializer is expected to be of type \x1b[31m" <> showText expectedTy <>
                    "\x1b[0m but it is of type \x1b[31m" <> showText actualTy <> "\x1b[0m."))
    EReturnValueExpected ty ->
        let title = "\x1b[31merror [E065]\x1b[0m: expected return value."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The function is expected to return a value of type \x1b[31m" <> showText ty <> "\x1b[0m."))
    EReturnValueNotUnit ->
        let title = "\x1b[31merror [E066]\x1b[0m: return value not expected."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just "The function is not expected to return a value.")
    EInvalidArrayType ty ->
        let title = "\x1b[31merror [E067]\x1b[0m: invalid array type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid array type."))
    EInvalidBoxType ty ->
        let title = "\x1b[31merror [E068]\x1b[0m: invalid box type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid box type."))
    ENoTypeFound ident ->
        let title = "\x1b[31merror [E069]\x1b[0m: no type found."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> T.pack ident <> "\x1b[0m is not found."))
    EGlobalNotType ident -> 
        let title = "\x1b[31merror [E070]\x1b[0m: global object but not a type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The global object \x1b[31m" <> T.pack ident <> "\x1b[0m is not a type."))
    EInvalidAccessToGlobal ident ->
        let title = "\x1b[31merror [E071]\x1b[0m: invalid access to global object."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The global object \x1b[31m" <> T.pack ident <> "\x1b[0m cannot be accessed from within this context."))
    EConstantIsReadOnly ident ->
        let title = "\x1b[31merror [E072]\x1b[0m: invalid write to a constant."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The constant \x1b[31m" <> T.pack ident <> "\x1b[0m is read-only and cannot be modified."))
    ESymbolDefined ident symbolPos@(Position symbolStart _symbolEnd) ->
        let title = "\x1b[31merror [E073]\x1b[0m: symbol already defined."
            symbolFileName = sourceName symbolStart
            symbolSourceLines = toModuleAST M.! symbolFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The symbol \x1b[31m" <> T.pack ident <> "\x1b[0m is already defined.")) >>
            printSimpleError
                symbolSourceLines "The symbol was previoulsy defined here:" symbolFileName
                symbolPos Nothing
    EExpressionNotConstant ->
        let title = "\x1b[31merror [E074]\x1b[0m: expression not constant."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just "The expression is not constant and cannot be evaluated at compile time.")
    EContinueInvalidExpression -> 
        let title = "\x1b[31merror [E075]\x1b[0m: invalid expression in continue statement."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just "The expression in a continue statement must be a call to a member action.")
    EContinueInvalidProcedureCall ident -> 
        let title = "\x1b[31merror [E076]\x1b[0m: invalid procedure call in continue statement."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("This statement can only be used to call a continuation action.\n" <>
                       "The procedure call \x1b[31m" <> T.pack ident <> "\x1b[0m in a continue statement is invalid."))
    EContinueInvalidMethodOrViewerCall ident -> 
        let title = "\x1b[31merror [E077]\x1b[0m: invalid method or viewer call in continue statement."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("This statement can only be used to call a continuation action.\n" <>
                       "The member function call \x1b[31m" <> T.pack ident <> "\x1b[0m in a continue statement is invalid."))
    EContinueInvalidMemberCall ts ->
        let title = "\x1b[31merror [E078]\x1b[0m: invalid member call in continue statement."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("This statement can only be used to call a continuation action.\n" <>
                       "Calling a procedure of an object of type \x1b[31m" <> showText ts <> "\x1b[0m in a continue statement is invalid."))
    EContinueActionNotFound ident -> 
        let title = "\x1b[31merror [E079]\x1b[0m: continuation action not found."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Action \x1b[31m" <> T.pack ident <> "\x1b[0m not found."))
    EContinueActionExtraParams (ident, params, actionPos@(Position actStartPos _endPos)) paramNumber ->
        let title = "\x1b[31merror [E080]\x1b[0m: extra parameters in continuation action."
            actFileName = sourceName actStartPos
            actSourceLines = toModuleAST M.! actFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Action \x1b[31m" <> T.pack ident <>
                    "\x1b[0m has only \x1b[31m" <> T.pack (show (length params)) <>
                    "\x1b[0m parameters but you are providing \x1b[31m" <> T.pack (show paramNumber) <> "\x1b[0m.")) >>
            printSimpleError
                actSourceLines "The action is defined here:" actFileName
                actionPos Nothing
    EContinueActionMissingParam (ident, actionPos@(Position actStartPos _endPos)) ->
        let title = "\x1b[31merror [E081]\x1b[0m: missing parameters in continuation action."
            actFileName = sourceName actStartPos
            actSourceLines = toModuleAST M.! actFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Action \x1b[31m" <> T.pack ident <>
                    "\x1b[0m requires \x1b[31mone\x1b[0m parameter but you are providing \x1b[31mnone\x1b[0m.")) >>
            printSimpleError
                actSourceLines "The action is defined here:" actFileName
                actionPos Nothing
    EEnumVariantInitializerInvalidUse ->
        let title = "\x1b[31merror [E082]\x1b[0m: invalid use of an enum variant initializer."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just $ "You are trying to use an enum variant initializer in an invalid context.\n" <>
                        "Enum variant initializers can only be used to initialize enum objects.")
    ENoEnumFound ident ->
        let title = "\x1b[31merror [E083]\x1b[0m: no enum found."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The enum \x1b[31m" <> T.pack ident <> "\x1b[0m is not found."))
    EGlobalNotEnum (ident, globalPos@(Position globalStart _)) ->
        let title = "\x1b[31merror [E084]\x1b[0m: global object but not an enum."
            globalFileName = sourceName globalStart
            globalSourceLines = toModuleAST M.! globalFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The global object \x1b[31m" <> T.pack ident <> "\x1b[0m is not an enum.")) >>
            printSimpleError
                globalSourceLines "The global object is defined here:" globalFileName
                globalPos Nothing
    EEnumVariantNotFound enumId variant ->
        let title = "\x1b[31merror [E085]\x1b[0m: enum variant not found."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Enum \x1b[31m" <> T.pack enumId <> "\x1b[0m does not have a variant named \x1b[31m" <> T.pack variant <> "\x1b[0m."))
    EEnumVariantExtraParams (enumId, enumPos@(Position enumStart _end)) (variant, params) paramNumber ->
        let title = "\x1b[31merror [E086]\x1b[0m: extra parameters in enum variant."
            enumFileName = sourceName enumStart
            enumSourceLines = toModuleAST M.! enumFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Enum variant \x1b[31m" <> T.pack variant <>
                    "\x1b[0m of enum \x1b[31m" <> T.pack enumId <>
                    "\x1b[0m has only \x1b[31m" <> T.pack (show (length params)) <>
                    "\x1b[0m parameters but you are providing \x1b[31m" <> T.pack (show paramNumber) <> "\x1b[0m.")) >>
            printSimpleError
                enumSourceLines "The enum is defined here:" enumFileName
                enumPos Nothing
    EEnumVariantMissingParams (enumId, enumPos@(Position enumStart _end)) (variant, params) paramNumber ->
        let title = "\x1b[31merror [E087]\x1b[0m: missing parameters in enum variant."
            enumFileName = sourceName enumStart
            enumSourceLines = toModuleAST M.! enumFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Enum variant \x1b[31m" <> T.pack variant <>
                    "\x1b[0m of enum \x1b[31m" <> T.pack enumId <>
                    "\x1b[0m has \x1b[31m" <> T.pack (show (length params)) <>
                    "\x1b[0m parameters but you are providing only \x1b[31m" <> T.pack (show paramNumber) <> "\x1b[0m.")) >>
            printSimpleError
                enumSourceLines "The enum is defined here:" enumFileName
                enumPos Nothing
    EEnumVariantParamTypeMismatch (enumId, enumPos@(Position enumStart _end)) (variant, paramNumber, expectedTy) actualTy ->
        let title = "\x1b[31merror [E088]\x1b[0m: enum variant parameter type mismatch."
            enumFileName = sourceName enumStart
            enumSourceLines = toModuleAST M.! enumFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Parameter \x1b[31m" <> T.pack (show paramNumber) <>
                    "\x1b[0m of enum variant \x1b[31m" <> T.pack variant <>
                    "\x1b[0m of enum \x1b[31m" <> T.pack enumId <>
                    "\x1b[0m is expected to be of type \x1b[31m" <> showText expectedTy <>
                    "\x1b[0m but it is of type \x1b[31m" <> showText actualTy <> "\x1b[0m.")) >>
            printSimpleError
                enumSourceLines "The enum is defined here:" enumFileName
                enumPos Nothing
    EFunctionNotFound ident ->
        let title = "\x1b[31merror [E089]\x1b[0m: function not found."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Function \x1b[31m" <> T.pack ident <> "\x1b[0m not found."))
    EGlobalNotFunction (ident, globalPos@(Position globalStart _)) ->
        let title = "\x1b[31merror [E090]\x1b[0m: global object but not a function."
            globalFileName = sourceName globalStart
            globalSourceLines = toModuleAST M.! globalFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The global object \x1b[31m" <> T.pack ident <> "\x1b[0m is not a function.")) >>
            printSimpleError
                globalSourceLines "The global object is defined here:" globalFileName
                globalPos Nothing
    _ -> putStrLn $ show pos ++ ": " ++ show e
-- | Print the error as is
ppError _ (AnnotatedError e pos) = putStrLn $ show pos ++ ": " ++ show e
