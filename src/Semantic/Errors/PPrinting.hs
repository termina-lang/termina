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
        let title = "\x1b[31merror [SE-001]\x1b[0m: invalid array indexing."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("You are trying to index an object of type \x1b[31m" <> showText ts <> "\x1b[0m.")) 
    (ENotNamedObject ident) ->
        let title = "\x1b[31merror [SE-002]\x1b[0m: Object not found."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The variable \x1b[31m" <> T.pack ident <> "\x1b[0m has not been declared"))
    (ENotConstant ident) ->
        let
            title = "\x1b[31merror [SE-003]\x1b[0m: invalid use of a non-constant object."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The object \x1b[31m" <> T.pack ident <> "\x1b[0m is not a constant."))
    EAssignmentToImmutable ->
        let title = "\x1b[31merror [SE-004]\x1b[0m: assignment to immutable variable."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just "You are trying to assign a value to an immutable object.")
    EIfElseNoOtherwise ->
        let title = "\x1b[31merror [SE-005]\x1b[0m: missing else clause."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("You are missing the else clause in an if-else-if statement.\n" <>
                    "You must provide an else clause if you are defining an else-if clause."))
    ENotCasteable ts1 ts2 ->
        let title = "\x1b[31merror [SE-006]\x1b[0m: invalid cast."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("You cannot cast a value of type \x1b[31m" <> showText ts1 <> "\x1b[0m to type \x1b[31m" <> showText ts2 <> "\x1b[0m."))
    EInvalidParameterType (Parameter ident ts) -> 
        let title = "\x1b[31merror [SE-007]\x1b[0m: invalid parameter type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Parameter \x1b[31m" <> T.pack ident <> "\x1b[0m has an invalid type \x1b[31m" <> showText ts <> "\x1b[0m."))
    EInvalidReturnType ts ->
        let title = "\x1b[31merror [SE-008]\x1b[0m: invalid return type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Invalid return type \x1b[31m" <> showText ts <> "\x1b[0m."))
    EProcedureCallExtraArgs (procId, params, procPos) argNumber ->
        let title = "\x1b[31merror [SE-009]\x1b[0m: extra arguments in procedure call."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Procedure \x1b[31m" <> T.pack procId <>
                    "\x1b[0m has only \x1b[31m" <> T.pack (show (length params)) <>
                    "\x1b[0m parameters but you are providing \x1b[31m" <> T.pack (show argNumber) <> "\x1b[0m.")) >>
            case procPos of 
                Position procStart _procEnd -> 
                    let procFileName = sourceName procStart
                        procSourceLines = toModuleAST M.! procFileName in
                    printSimpleError
                        procSourceLines "The interface of the procedure is defined here:" procFileName
                        procPos Nothing
                _ -> return ()
    EProcedureCallMissingArgs (ident, params, procPos) argNumber ->
        let title = "\x1b[31merror [SE-010]\x1b[0m: missing arguments in procedure call."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Procedure \x1b[31m" <> T.pack ident <>
                    "\x1b[0m has \x1b[31m" <> T.pack (show (length params)) <>
                    "\x1b[0m parameters but you are providing only \x1b[31m" <> T.pack (show argNumber) <> "\x1b[0m.")) >>
            case procPos of 
                Position procStart _procEnd -> 
                    let procFileName = sourceName procStart
                        procSourceLines = toModuleAST M.! procFileName in
                    printSimpleError
                        procSourceLines
                        ("Procedure \x1b[31m" <> T.pack ident <> "\x1b[0m is defined here:")
                        procFileName procPos Nothing
                _ -> return ()
    EProcedureCallArgTypeMismatch (ident, expectedTy, procPos) paramCount actualTy ->
        let title = "\x1b[31merror [SE-011]\x1b[0m: parameter type mismatch in procedure call."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Argument \x1b[31m#" <> T.pack (show paramCount) <> "\x1b[0m of procedure \x1b[31m" <> T.pack ident <>
                    "\x1b[0m is expected to be of type \x1b[31m" <> showText expectedTy <>
                    "\x1b[0m but you are providing it of type \x1b[31m" <> showText actualTy <> "\x1b[0m.")) >>
            case procPos of 
                Position procStart _procEnd -> 
                    let procFileName = sourceName procStart
                        procSourceLines = toModuleAST M.! procFileName in
                    printSimpleError
                        procSourceLines
                        ("Procedure \x1b[31m" <> T.pack ident <> "\x1b[0m is defined here:")
                        procFileName procPos Nothing
                _ -> return ()
    EUnknownProcedure ident ->
        let title = "\x1b[31merror [SE-012]\x1b[0m: unknown procedure."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Unknown procedure \x1b[31m" <> T.pack ident <> "\x1b[0m."))
    EResourceClassNoProvides ident ->
        let title = "\x1b[31merror [SE-013]\x1b[0m: resource class does not provide any interface."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Resource class \x1b[31m" <> T.pack ident <> "\x1b[0m does not provide any interface.\n" <>
                       "A resource class must provide at least one interface."))
    EResourceClassAction (classId, Position startPosClass endPosClass) ident ->
        let title = "\x1b[31merror [SE-014]\x1b[0m: resource class defines an action."
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
        let title = "\x1b[31merror [SE-015]\x1b[0m: resource class defines an in port."
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
        let title = "\x1b[31merror [SE-016]\x1b[0m: resource class defines an out port."
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
        let title = "\x1b[31merror [SE-017]\x1b[0m: interface not found."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Interface \x1b[31m" <> T.pack ident <> "\x1b[0m not found."))
    EGlobalNotInterface ident ->
        let title = "\x1b[31merror [SE-018]\x1b[0m: identifier not an interface."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The identifier \x1b[31m" <> T.pack ident <> "\x1b[0m is not an interface."))
    EProcedureNotFromProvidedInterfaces (classId, Position startPosClass endPosClass) ident ->
        let title = "\x1b[31merror [SE-019]\x1b[0m: procedure not from provided interfaces."
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
                                            True (Just " \x1b[31munknown procedure\x1b[0m") fancyRedPointer
                                ]
                                Nothing
                        ]
                        (Just
                            ("The procedure \x1b[31m" <> T.pack ident
                                <> "\x1b[0m does not belong to any of the provided interfaces of resource class \x1b[31m"
                                <> T.pack classId <> "\x1b[0m."))
                ]
    EMissingProcedure ifaceId procId ->
        let title = "\x1b[31merror [SE-020]\x1b[0m: missing procedure."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Procedure \x1b[31m" <> T.pack procId <> "\x1b[0m of interface \x1b[31m" <> T.pack ifaceId <> "\x1b[0m is not being provided."))
    EProcedureExtraParams (ifaceId, procId, params, procPos@(Position procStart _procEnd)) paramNumber ->
        let title = "\x1b[31merror [SE-021]\x1b[0m: extra parameters in procedure definition."
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
        let title = "\x1b[31merror [SE-022]\x1b[0m: missing parameters in procedure definition."
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
        let title = "\x1b[31merror [SE-023]\x1b[0m: parameter type mismatch in procedure definition."
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
    ETaskClassProvides ident ->
        let title = "\x1b[31merror [SE-024]\x1b[0m: task class provides an interface."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Task class \x1b[31m" <> T.pack ident <> "\x1b[0m provides an interface.\n" <>
                       "Task classes must not provide any interface."))
    ETaskClassProcedure (classId, Position startPosClass endPosClass) ident ->
        let title = "\x1b[31merror [SE-025]\x1b[0m: task class defines a procedure."
            procStartLine = sourceLine start
            procEndLine = sourceLine end
            procStartColumn = sourceColumn start
            procEndColumn = 
                if procStartLine == procEndLine then 
                    sourceColumn end 
                else 
                    fromIntegral $ TL.length (TL.lines sourceLines !! (procStartLine - 1)) + 1
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
                                    Pointer procStartLine procStartColumn procEndColumn
                                            True (Just " \x1b[31minvalid procedure definition\x1b[0m") fancyRedPointer
                                ]
                                Nothing
                        ]
                        (Just
                            ("Task class \x1b[31m" <> T.pack classId <> "\x1b[0m defines the procedure \x1b[31m" <> T.pack ident <> "\x1b[0m.\n"
                            <> "Task classes cannot define procedures."))
                ]
    ETaskClassNoActions ident ->
        let title = "\x1b[31merror [SE-026]\x1b[0m: task class does not define any actions."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Task class \x1b[31m" <> T.pack ident <> "\x1b[0m does not define any actions.\n" <>
                       "Task classes must define at least one action."))
    EHandlerClassProvides ident ->
        let title = "\x1b[31merror [SE-027]\x1b[0m: handler class provides an interface."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Handler class \x1b[31m" <> T.pack ident <> "\x1b[0m provides an interface.\n" <>
                       "Handler classes must not provide any interface."))
    EHandlerClassProcedure (classId, Position startPosClass endPosClass) ident ->
        let title = "\x1b[31merror [SE-028]\x1b[0m: handler class defines a procedure."
            procStartLine = sourceLine start
            procEndLine = sourceLine end
            procStartColumn = sourceColumn start
            procEndColumn = 
                if procStartLine == procEndLine then 
                    sourceColumn end 
                else 
                    fromIntegral $ TL.length (TL.lines sourceLines !! (procStartLine - 1)) + 1
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
                                    Pointer procStartLine procStartColumn procEndColumn
                                            True (Just " \x1b[31minvalid procedure definition\x1b[0m") fancyRedPointer
                                ]
                                Nothing
                        ]
                        (Just
                            ("Handler class \x1b[31m" <> T.pack classId <> "\x1b[0m defines the procedure \x1b[31m" <> T.pack ident <> "\x1b[0m.\n"
                            <> "Handler classes cannot define procedures."))
                ]
    EHandlerClassNoAction ident ->
        let title = "\x1b[31merror [SE-029]\x1b[0m: handler class does not define any actions."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Handler class \x1b[31m" <> T.pack ident <> "\x1b[0m does not define any actions.\n" <>
                       "Handler classes must define exactly one action."))
    EHandlerClassMultipleActions classId prevActPos@(Position actStartPos _actEndPos) ->
        let title = "\x1b[31merror [SE-030]\x1b[0m: handler class defines multiple actions."
            actFileName = sourceName actStartPos
            actSourceLines = toModuleAST M.! actFileName 
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Handler class \x1b[31m" <> T.pack classId <> "\x1b[0m defines multiple actions.")) >>
            printSimpleError
                actSourceLines "Another action is defined here:" actFileName
                prevActPos Nothing
    EHandlerClassNoSinkPort classId ->
        let title = "\x1b[31merror [SE-031]\x1b[0m: handler class does not define any sink port."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Handler class \x1b[31m" <> T.pack classId <> "\x1b[0m does not define any sink port.\n" <>
                       "Handler classes must define exactly one sink port."))
    EHandlerClassMultipleSinkPorts classId prevPortPos@(Position portStartPos _portEndPos) ->
        let title = "\x1b[31merror [SE-032]\x1b[0m: handler class defines multiple sink ports."
            portFileName = sourceName portStartPos
            portSourceLines = toModuleAST M.! portFileName 
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Handler class \x1b[31m" <> T.pack classId <> "\x1b[0m defines multiple sink ports.")) >>
            printSimpleError
                portSourceLines "Another sink port is defined here:" portFileName
                prevPortPos Nothing
    EHandlerClassInPort (classId, Position startPosClass endPosClass) ident ->
        let title = "\x1b[31merror [SE-033]\x1b[0m: handler class defines an in port."
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
                            ("Handler class \x1b[31m" <> T.pack classId <> "\x1b[0m defines the in port \x1b[31m" <> T.pack ident <> "\x1b[0m.\n"
                            <> "Handler classes cannot define in ports."))
                ]
    EIfElseIfCondNotBool ts ->
        let title = "\x1b[31merror [SE-034]\x1b[0m: if-else-if condition not boolean."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The condition in the statement is expected to be of type \x1b[31mbool\x1b[0m but it is of type \x1b[31m" <> showText ts <> "\x1b[0m."))
    EFunctionCallExtraArgs (funcId, params, funcPos@(Position funcStart _procEnd)) argNumber ->
        let title = "\x1b[31merror [SE-035]\x1b[0m: extra arguments in function call."
            funcFileName = sourceName funcStart
            funcSourceLines = toModuleAST M.! funcFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Function \x1b[31m" <> T.pack funcId <>
                    "\x1b[0m has only \x1b[31m" <> T.pack (show (length params)) <>
                    "\x1b[0m parameters but you are providing \x1b[31m" <> T.pack (show argNumber) <> "\x1b[0m.")) >>
            printSimpleError
                funcSourceLines ("Function \x1b[31m" <> T.pack funcId <> "\x1b[0m is defined here:") funcFileName
                funcPos Nothing
    EFunctionCallMissingArgs (funcId, params, funcPos@(Position funcStart _procEnd)) argNumber ->
        let title = "\x1b[31merror [SE-036]\x1b[0m: missing arguments in function call."
            funcFileName = sourceName funcStart
            funcSourceLines = toModuleAST M.! funcFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Function \x1b[31m" <> T.pack funcId <>
                    "\x1b[0m has \x1b[31m" <> T.pack (show (length params)) <>
                    "\x1b[0m parameters but you are providing only \x1b[31m" <> T.pack (show argNumber) <> "\x1b[0m.")) >>
            printSimpleError
                funcSourceLines ("Function \x1b[31m" <> T.pack funcId <> "\x1b[0m is defined here:") funcFileName
                funcPos Nothing
    EFunctionCallArgTypeMismatch (funcId, expectedTy, funcPos@(Position funcStart _procEnd)) argNumber actualTy ->
        let title = "\x1b[31merror [SE-037]\x1b[0m: argument type mismatch in function call."
            funcFileName = sourceName funcStart
            funcSourceLines = toModuleAST M.! funcFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Argument \x1b[31m#" <> T.pack (show argNumber) <> "\x1b[0m of function \x1b[31m" <> T.pack funcId <>
                    "\x1b[0m is expected to be of type \x1b[31m" <> showText expectedTy <>
                    "\x1b[0m but you are providing it of type \x1b[31m" <> showText actualTy <> "\x1b[0m.")) >>
            printSimpleError
                funcSourceLines ("Function \x1b[31m" <> T.pack funcId <> "\x1b[0m is defined here:") funcFileName
                funcPos Nothing
    EMemberAccessNotFunction ident ->
        let title = "\x1b[31merror [SE-038]\x1b[0m: Access to a member that is not a function."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The identifier \x1b[31m" <> T.pack ident <> "\x1b[0m is not a valid member function."))
    EMutableReferenceToImmutable ->
        let title = "\x1b[31merror [SE-039]\x1b[0m: mutable reference to immutable object."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just "You are trying to create a mutable reference to an immutable object.")
    EMutableReferenceToPrivate ->
        let title = "\x1b[31merror [SE-040]\x1b[0m: mutable reference to private object."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just "You are trying to create a mutable reference to a private object.")
    EBinOpExpectedTypeLeft op expectedTy actualTy ->
        let title = "\x1b[31merror [SE-041]\x1b[0m: Binary operation expected type on the left."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The result of the binary operation \x1b[31m" <> showText op <>
                    "\x1b[0m is expected to be of type \x1b[31m" <> showText expectedTy <>
                    "\x1b[0m but the left operand you are providing is of type \x1b[31m" <>
                    showText actualTy <> "\x1b[0m."))
    EBinOpExpectedTypeRight op expectedTy actualTy ->
        let title = "\x1b[31merror [SE-042]\x1b[0m: Binary operation expected type on the right."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The result of the binary operation \x1b[31m" <> showText op <>
                    "\x1b[0m is expected to be of type \x1b[31m" <> showText expectedTy <>
                    "\x1b[0m but the right operand you are providing is of type \x1b[31m" <>
                    showText actualTy <> "\x1b[0m."))
    EBinOpTypeMismatch op ty_le ty_re ->
        let title = "\x1b[31merror [SE-043]\x1b[0m: binary operation type mismatch."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Binary operation \x1b[31m" <> showText op <>
                    "\x1b[0m expects operands of the same type but the left one is of type \x1b[31m" <>
                    showText ty_le <> "\x1b[0m and the right one is of type \x1b[31m" <> showText ty_re <> "\x1b[0m."))
    EBinOpExpectedTypeNotBool op ty ->
        let title = "\x1b[31merror [SE-044]\x1b[0m: binary operation expected result type not boolean."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The binary operation \x1b[31m" <> showText op <>
                    "\x1b[0m will result in a value of type \x1b[31m" <> showText TBool <>
                    "\x1b[0m but it is expected to be of type \x1b[31m" <> showText ty <> "\x1b[0m."))
    EBinOpLeftTypeNotBool op ty ->
        let title = "\x1b[31merror [SE-045]\x1b[0m: binary operation expected boolean type on the left."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The left operand of the binary operation \x1b[31m" <> showText op <>
                    "\x1b[0m is of type \x1b[31m" <> showText ty <>
                    "\x1b[0m but it is expected to be of type \x1b[31m" <> showText TBool <> "\x1b[0m."))
    EBinOpRightTypeNotBool op ty ->
        let title = "\x1b[31merror [SE-046]\x1b[0m: binary operation expected boolean type on the right."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The right operand of the binary operation \x1b[31m" <> showText op <>
                    "\x1b[0m is of type \x1b[31m" <> showText ty <>
                    "\x1b[0m but it is expected to be of type \x1b[31m" <> showText TBool <> "\x1b[0m."))
    EBinOpExpectedTypeNotNum op ty ->
        let title = "\x1b[31merror [SE-047]\x1b[0m: binary operation expected result type not numeric."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The binary operation \x1b[31m" <> showText op <>
                    "\x1b[0m will result in a numeric value but the expected type is \x1b[31m" <> showText ty <> "\x1b[0m."))
    EBinOpLeftTypeNotNum op ty ->
        let title = "\x1b[31merror [SE-048]\x1b[0m: binary operation expected numeric type on the left."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The left operand of the binary operation \x1b[31m" <> showText op <>
                    "\x1b[0m is of type \x1b[31m" <> showText ty <>
                    "\x1b[0m but it is expected to be of numeric type."))
    EBinOpRightTypeNotNum op ty ->
        let title = "\x1b[31merror [SE-049]\x1b[0m: binary operation expected numeric type on the right."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The right operand of the binary operation \x1b[31m" <> showText op <>
                    "\x1b[0m is of type \x1b[31m" <> showText ty <>
                    "\x1b[0m but it is expected to be of numeric type."))
    EBinOpRightTypeNotPos op ty ->
        let title = "\x1b[31merror [SE-050]\x1b[0m: binary operation expected positive numeric type on the right."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The right operand of the binary operation \x1b[31m" <> showText op <>
                    "\x1b[0m is of type \x1b[31m" <> showText ty <>
                    "\x1b[0m but it is expected to be of positive numeric type."))
    EBinOpLeftTypeNotEq op ty ->
        let title = "\x1b[31merror [SE-051]\x1b[0m: binary operation expected equatable type on the left."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The left operand of the binary operation \x1b[31m" <> showText op <>
                    "\x1b[0m is of type \x1b[31m" <> showText ty <>
                    "\x1b[0m but it is expected to be of equatable type."))
    EBinOpRightTypeNotEq op ty ->
        let title = "\x1b[31merror [SE-052]\x1b[0m: binary operation expected equatable type on the right."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The right operand of the binary operation \x1b[31m" <> showText op <>
                    "\x1b[0m is of type \x1b[31m" <> showText ty <>
                    "\x1b[0m but it is expected to be of equatable type."))
    EAtomicAccessInvalidType ty ->
        let title = "\x1b[31merror [SE-053]\x1b[0m: invalid type for the atomic access interface."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not valid for atomic access, only numeric types are allowed."))
    EAtomicArrayAccessInvalidType ty ->
        let title = "\x1b[31merror [SE-054]\x1b[0m: invalid type for the atomic array access interface."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not valid for atomic array access, only numeric types are allowed."))
    EAtomicInvalidType ty ->
        let title = "\x1b[31merror [SE-055]\x1b[0m: invalid atomic type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not valid for atomic."))
    EAtomicArrayInvalidType ty ->
        let title = "\x1b[31merror [SE-056]\x1b[0m: invalid atomic array type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not valid for atomic array."))
    EAtomicConnectionTypeMismatch expectedTy actualTy ->
        let title = "\x1b[31merror [SE-057]\x1b[0m: atomic connection type mismatch."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type of the connected atomic resource is expected to be \x1b[31m" <> showText expectedTy <>
                    "\x1b[0m but it is of type \x1b[31m" <> showText actualTy <> "\x1b[0m."))
    EAtomicArrayConnectionTypeMismatch expectedTy actualTy ->
        let title = "\x1b[31merror [SE-058]\x1b[0m: atomic array connection type mismatch."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type of the elements of the connected atomic array is expected to be \x1b[31m" <> showText expectedTy <>
                    "\x1b[0m but the array is of elements of type \x1b[31m" <> showText actualTy <> "\x1b[0m."))
    EAtomicArrayConnectionSizeMismatch expectedSize actualSize ->
        let title = "\x1b[31merror [SE-059]\x1b[0m: atomic array connection size mismatch."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The size of the connected atomic array is expected to be \x1b[31m" <> showText expectedSize <>
                    "\x1b[0m but the array has size \x1b[31m" <> showText actualSize <> "\x1b[0m."))
    EConstantWithoutKnownType c ->
        let title = "\x1b[31merror [SE-060]\x1b[0m: constant without known type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type of the constant \x1b[31m" <> showText c <>
                    "\x1b[0m cannot be inferred from the environment and must be explicitly defined."))
    EStructInitializerInvalidUse ->
        let title = "\x1b[31merror [SE-061]\x1b[0m: invalid use of struct initializer."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just $ "You are trying to use a struct initializer in an invalid context.\n" <>
                        "Struct initializers can only be used to initialize struct objects.")
    EStructInitializerTypeMismatch expectedTy actualTy ->
        let title = "\x1b[31merror [SE-062]\x1b[0m: struct initializer type mismatch."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The struct initializer is expected to be of type \x1b[31m" <> showText expectedTy <>
                    "\x1b[0m but it is of type \x1b[31m" <> showText actualTy <> "\x1b[0m."))
    EEnumInitializerExpectedTypeMismatch expectedTy actualTy ->
        let title = "\x1b[31merror [SE-063]\x1b[0m: enum initializer expected type mismatch."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The enum initializer is expected to be of type \x1b[31m" <> showText expectedTy <>
                    "\x1b[0m but it is of type \x1b[31m" <> showText actualTy <> "\x1b[0m."))
    ESliceInvalidUse ->
        let title = "\x1b[31merror [SE-064]\x1b[0m: invalid use of slice."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just $ "You are trying to use a slice in an invalid context.\n" <>
                        "Slices can only be used to create references to a part of an array.")
    EArrayInitializerInvalidUse ->
        let title = "\x1b[31merror [SE-065]\x1b[0m: invalid use of an array initializer."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just $ "You are trying to use an array initializer in an invalid context.\n" <>
                        "Array initializers can only be used to initialize array objects.")
    EArrayInitializerNotArray ty ->
        let title = "\x1b[31merror [SE-066]\x1b[0m: assignment of an array initializer to a non-array type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Invalid use of an array initializer.\n" <>
                    "You are trying to assign an array initializer to an object of type \x1b[31m" <>
                    showText ty <> "\x1b[0m."))
    EArrayExprListInitializerInvalidUse ->
        let title = "\x1b[31merror [SE-067]\x1b[0m: invalid use of an expression list array initializer."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just $ "You are trying to use an array expression list initializer in an invalid context.\n" <>
                        "TArray expression list initializers can only be used to initialize array objects.")
    EArrayExprListInitializerNotArray ty ->
        let title = "\x1b[31merror [SE-068]\x1b[0m: assignment of an array expression list initializer to a non-array type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Invalid use of an array expression list initializer.\n" <>
                    "You are trying to assign an array expression list initializer to an object of type \x1b[31m" <>
                    showText ty <> "\x1b[0m."))
    EOptionVariantInitializerInvalidUse ->
        let title = "\x1b[31merror [SE-069]\x1b[0m: invalid use of an option variant initializer."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just $ "You are trying to use an option variant initializer in an invalid context.\n" <>
                        "Option variant initializers can only be used to initialize option objects.")
    EArrayInitializerSizeMismatch expectedSize initializerSize ->
        let title = "\x1b[31merror [SE-070]\x1b[0m: array initializer size mismatch."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The size of the array initializer is \x1b[31m" <> showText initializerSize <>
                    "\x1b[0m but the expected size is \x1b[31m" <> showText expectedSize <> "\x1b[0m."))
    EArrayExprListInitializerSizeMismatch expectedSize initializerSize ->
        let title = "\x1b[31merror [SE-071]\x1b[0m: array expression list initializer size mismatch."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The size of the array expression list initializer is \x1b[31m" <> T.pack (show initializerSize) <>
                    "\x1b[0m but the expected size is \x1b[31m" <> T.pack (show expectedSize) <> "\x1b[0m."))
    EArrayExprListInitializerExprTypeMismatch expectedTy actualTy ->
        let title = "\x1b[31merror [SE-072]\x1b[0m: list of initializing expressions type mismatch."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The expression in the array expression list initializer is expected to be of type \x1b[31m" <> showText expectedTy <>
                    "\x1b[0m but it is of type \x1b[31m" <> showText actualTy <> "\x1b[0m."))
    EReturnValueExpected ty ->
        let title = "\x1b[31merror [SE-073]\x1b[0m: expected return value."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The function is expected to return a value of type \x1b[31m" <> showText ty <> "\x1b[0m."))
    EReturnValueNotUnit ->
        let title = "\x1b[31merror [SE-074]\x1b[0m: return value not expected."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just "The function is not expected to return a value.")
    EInvalidArrayType ty ->
        let title = "\x1b[31merror [SE-075]\x1b[0m: invalid array type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid array type."))
    EInvalidBoxType ty ->
        let title = "\x1b[31merror [SE-076]\x1b[0m: invalid box type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid box type."))
    ENoTypeFound ident ->
        let title = "\x1b[31merror [SE-077]\x1b[0m: no type found."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> T.pack ident <> "\x1b[0m is not found."))
    EGlobalNotType (ident, globalPos@(Position globalStart _)) ->
        let title = "\x1b[31merror [SE-078]\x1b[0m: global object but not a type."
            globalFileName = sourceName globalStart
            globalSourceLines = toModuleAST M.! globalFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The global object \x1b[31m" <> T.pack ident <> "\x1b[0m is not a type.")) >>
            printSimpleError
                globalSourceLines "The global object is defined here:" globalFileName
                globalPos Nothing
    EInvalidAccessToGlobal ident ->
        let title = "\x1b[31merror [SE-079]\x1b[0m: invalid access to global object."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The global object \x1b[31m" <> T.pack ident <> "\x1b[0m cannot be accessed from within this context."))
    EConstantIsReadOnly ident ->
        let title = "\x1b[31merror [SE-080]\x1b[0m: invalid write to a constant."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The constant \x1b[31m" <> T.pack ident <> "\x1b[0m is read-only and cannot be modified."))
    ESymbolAlreadyDefined (ident, symbolPos@(Position symbolStart _symbolEnd)) ->
        let title = "\x1b[31merror [SE-081]\x1b[0m: symbol already defined."
            symbolFileName = sourceName symbolStart
            symbolSourceLines = toModuleAST M.! symbolFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The symbol \x1b[31m" <> T.pack ident <> "\x1b[0m is already defined.")) >>
            printSimpleError
                symbolSourceLines "The symbol was previoulsy defined here:" symbolFileName
                symbolPos Nothing
    EContinueInvalidExpression -> 
        let title = "\x1b[31merror [SE-082]\x1b[0m: invalid expression in continue statement."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just "The expression in a continue statement must be a call to a member action.")
    EContinueInvalidMethodOrViewerCall ident -> 
        let title = "\x1b[31merror [SE-083]\x1b[0m: invalid method or viewer call in continue statement."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("This statement can only be used to call a continuation action.\n" <>
                       "The member function call \x1b[31m" <> T.pack ident <> "\x1b[0m in a continue statement is invalid."))
    EContinueInvalidMemberCall ts ->
        let title = "\x1b[31merror [SE-084]\x1b[0m: invalid member call in continue statement."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("This statement can only be used to call a continuation action.\n" <>
                       "Calling a procedure of an object of type \x1b[31m" <> showText ts <> "\x1b[0m in a continue statement is invalid."))
    EContinueActionExtraArgs (ident, params, actionPos@(Position actStartPos _endPos)) argNumber ->
        let title = "\x1b[31merror [SE-085]\x1b[0m: extra arguments in continuation action."
            actFileName = sourceName actStartPos
            actSourceLines = toModuleAST M.! actFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Action \x1b[31m" <> T.pack ident <>
                    "\x1b[0m has only \x1b[31m" <> T.pack (show (length params)) <>
                    "\x1b[0m parameters but you are providing \x1b[31m" <> T.pack (show argNumber) <> "\x1b[0m.")) >>
            printSimpleError
                actSourceLines "The action is defined here:" actFileName
                actionPos Nothing
    EContinueActionMissingArgs (ident, actionPos@(Position actStartPos _endPos)) ->
        let title = "\x1b[31merror [SE-086]\x1b[0m: missing arguments in continuation action."
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
        let title = "\x1b[31merror [SE-087]\x1b[0m: invalid use of an enum variant initializer."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just $ "You are trying to use an enum variant initializer in an invalid context.\n" <>
                        "Enum variant initializers can only be used to initialize enum objects.")
    EEnumVariantNotFound enumId variant ->
        let title = "\x1b[31merror [SE-088]\x1b[0m: enum variant not found."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Enum \x1b[31m" <> T.pack enumId <> "\x1b[0m does not have a variant named \x1b[31m" <> T.pack variant <> "\x1b[0m."))
    EEnumVariantExtraParams (enumId, enumPos@(Position enumStart _end)) (variant, params) paramNumber ->
        let title = "\x1b[31merror [SE-089]\x1b[0m: extra parameters in enum variant."
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
        let title = "\x1b[31merror [SE-090]\x1b[0m: missing parameters in enum variant."
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
        let title = "\x1b[31merror [SE-091]\x1b[0m: enum variant parameter type mismatch."
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
        let title = "\x1b[31merror [SE-092]\x1b[0m: function not found."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Function \x1b[31m" <> T.pack ident <> "\x1b[0m not found."))
    EGlobalNotFunction (ident, globalPos@(Position globalStart _)) ->
        let title = "\x1b[31merror [SE-093]\x1b[0m: global object but not a function."
            globalFileName = sourceName globalStart
            globalSourceLines = toModuleAST M.! globalFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The global object \x1b[31m" <> T.pack ident <> "\x1b[0m is not a function.")) >>
            printSimpleError
                globalSourceLines "The global object is defined here:" globalFileName
                globalPos Nothing
    EUnexpectedNumericConstant ty ->
        let title = "\x1b[31merror [SE-094]\x1b[0m: unexpected numeric constant."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Expected a value of type \x1b[31m" <> showText ty <> "\x1b[0m but found a numeric constant."))
    EInvalidAssignmentExprType ty ->
        let title = "\x1b[31merror [SE-095]\x1b[0m: invalid assignment expression type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Objects of type \x1b[31m" <> showText ty <> "\x1b[0m cannot be copied."))
    EInvalidMessageType ty ->
        let title = "\x1b[31merror [SE-096]\x1b[0m: invalid message type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid message type."))
    EInvalidOptionType ty ->
        let title = "\x1b[31merror [SE-097]\x1b[0m: invalid option type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid option type."))
    EInvalidReferenceType ty ->
        let title = "\x1b[31merror [SE-098]\x1b[0m: invalid reference type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("References to objects of type \x1b[31m" <> showText ty <> "\x1b[0m cannot be created."))
    EInvalidFixedLocationType ty ->
        let title = "\x1b[31merror [SE-099]\x1b[0m: invalid fixed-location type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Fixed-location fields of type \x1b[31m" <> showText ty <> "\x1b[0m cannot be defined."))
    EInvalidAllocatorType ty ->
        let title = "\x1b[31merror [SE-100]\x1b[0m: invalid allocator type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid allocator type."))
    EInvalidClassFieldType ty ->
        let title = "\x1b[31merror [SE-101]\x1b[0m: invalid class field type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid class field type."))
    EInvalidStructFieldType ty ->
        let title = "\x1b[31merror [SE-102]\x1b[0m: invalid struct field type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid struct field type."))
    EInvalidEnumParameterType ty ->
        let title = "\x1b[31merror [SE-103]\x1b[0m: invalid enum parameter type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid parameter type for an enum variant."))
    EInvalidAccessPortType ty ->
        let title = "\x1b[31merror [SE-104]\x1b[0m: invalid access port type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid access port type."))
    EInvalidDeclarationType ty ->
        let title = "\x1b[31merror [SE-105]\x1b[0m: invalid declaration type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid object declaration type."))
    EInvalidTypeSpecifier ts ->
        let title = "\x1b[31merror [SE-106]\x1b[0m: invalid type specifier."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type specifier \x1b[31m" <> showText ts <> "\x1b[0m is not valid."))
    EInvalidNumericConstantType ty ->
        let title = "\x1b[31merror [SE-107]\x1b[0m: invalid numeric constant type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The expected type of this expression is \x1b[31m" <> showText ty <> "\x1b[0m but it is a numeric constant."))
    EInvalidActionParameterType ty ->
        let title = "\x1b[31merror [SE-108]\x1b[0m: invalid action parameter type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid parameter type for an action."))
    EInvalidProcedureParameterType ty ->
        let title = "\x1b[31merror [SE-109]\x1b[0m: invalid procedure parameter type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid parameter type for a procedure."))
    EMemberFunctionCallExtraArgs (funcId, params, funcPos@(Position funcStart _procEnd)) argNumber ->
        let title = "\x1b[31merror [SE-110]\x1b[0m: extra arguments in member function call."
            funcFileName = sourceName funcStart
            funcSourceLines = toModuleAST M.! funcFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Member function \x1b[31m" <> T.pack funcId <>
                    "\x1b[0m has only \x1b[31m" <> T.pack (show (length params)) <>
                    "\x1b[0m parameters but you are providing \x1b[31m" <> T.pack (show argNumber) <> "\x1b[0m.")) >>
            printSimpleError
                funcSourceLines ("Member function \x1b[31m" <> T.pack funcId <> "\x1b[0m is defined here:") funcFileName
                funcPos Nothing
    EMemberFunctionCallMissingArgs (funcId, params, funcPos@(Position funcStart _procEnd)) argNumber ->
        let title = "\x1b[31merror [SE-111]\x1b[0m: missing arguments in member function call."
            funcFileName = sourceName funcStart
            funcSourceLines = toModuleAST M.! funcFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Member function \x1b[31m" <> T.pack funcId <>
                    "\x1b[0m has \x1b[31m" <> T.pack (show (length params)) <>
                    "\x1b[0m parameters but you are providing only \x1b[31m" <> T.pack (show argNumber) <> "\x1b[0m.")) >>
            printSimpleError
                funcSourceLines ("Member function \x1b[31m" <> T.pack funcId <> "\x1b[0m is defined here:") funcFileName
                funcPos Nothing
    EMemberFunctionCallArgTypeMismatch (funcId, expectedTy, funcPos@(Position funcStart _procEnd)) argNumber actualTy ->
        let title = "\x1b[31merror [SE-112]\x1b[0m: member function call argument type mismatch."
            funcFileName = sourceName funcStart
            funcSourceLines = toModuleAST M.! funcFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Argument \x1b[31m#" <> T.pack (show argNumber) <>
                    "\x1b[0m of member function \x1b[31m" <> T.pack funcId <>
                    "\x1b[0m is expected to be of type \x1b[31m" <> showText expectedTy <>
                    "\x1b[0m but it is of type \x1b[31m" <> showText actualTy <> "\x1b[0m.")) >>
            printSimpleError
                funcSourceLines ("Member function \x1b[31m" <> T.pack funcId <> "\x1b[0m is defined here:") funcFileName
                funcPos Nothing
    EArrayIndexNotUSize ty ->
        let title = "\x1b[31merror [SE-113]\x1b[0m: invalid array index type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type of the array index is \x1b[31m" <> showText ty <>
                 "\x1b[0m but it is expected to be of type \x1b[31m" <> showText TUSize <> "\x1b[0m."))
    EArraySliceLowerBoundNotUSize ty ->
        let title = "\x1b[31merror [SE-114]\x1b[0m: invalid array slice lower bound type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type of the lower bound of the array slice is \x1b[31m" <> showText ty <>
                 "\x1b[0m but it is expected to be of type \x1b[31m" <> showText TUSize <> "\x1b[0m."))
    EArraySliceUpperBoundNotUSize ty ->
        let title = "\x1b[31merror [SE-115]\x1b[0m: invalid array slice upper bound type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type of the upper bound of the array slice is \x1b[31m" <> showText ty <>
                 "\x1b[0m but it is expected to be of type \x1b[31m" <> showText TUSize <> "\x1b[0m."))
    EOutboundPortSendInvalidNumArgs argNumber ->
        let title = "\x1b[31merror [SE-116]\x1b[0m: invalid number of arguments in outbound port send."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The send procedure of an outbound port expects \x1b[31mone\x1b[0m argument but you are providing \x1b[31m" <>
                    T.pack (show argNumber) <> "\x1b[0m."))
    EOutboundPortArgTypeMismatch expectedTy actualTy ->
        let title = "\x1b[31merror [SE-117]\x1b[0m: output port argument type mismatch."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The output data is expected to be of type \x1b[31m" <> showText expectedTy <>
                    "\x1b[0m but you are sending data of type \x1b[31m" <> showText actualTy <> "\x1b[0m."))
    EAssignmentExprMismatch expectedTy actualTy ->
        let title = "\x1b[31merror [SE-118]\x1b[0m: assignment expression type mismatch."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The expected type of the assignment is \x1b[31m" <> showText expectedTy <>
                    "\x1b[0m but it is of type \x1b[31m" <> showText actualTy <> "\x1b[0m."))
    EFieldValueAssignmentMissingFields (recordId, recordPos@(Position recordStart _end)) [field] ->
        let title = "\x1b[31merror [SE-119]\x1b[0m: missing field/s in field assignment expression."
            recordFileName = sourceName recordStart
            recordSourceLines = toModuleAST M.! recordFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Field \x1b[31m" <> T.pack field <>
                    "\x1b[0m is not being assigned a value in the field assignment expression.")) >>
            printSimpleError
                recordSourceLines ("The type \x1b[31m" <> T.pack recordId <> "\x1b[0m is defined here:") recordFileName
                recordPos Nothing
    EFieldValueAssignmentMissingFields (recordId, recordPos@(Position recordStart _end)) fields ->
        let title = "\x1b[31merror [SE-119]\x1b[0m: missing field/s in field assignment expression."
            recordFileName = sourceName recordStart
            recordSourceLines = toModuleAST M.! recordFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Fields \x1b[31m" <> T.intercalate ", " (map T.pack fields) <>
                    "\x1b[0m are not being assigned a value in the field assignment expression.")) >>
            printSimpleError
                recordSourceLines ("The type \x1b[31m" <> T.pack recordId <> "\x1b[0m is defined here:") recordFileName
                recordPos Nothing
    EFieldValueAssignmentUnknownFields (recordId, recordPos@(Position recordStart _end)) [field] ->
        let title = "\x1b[31merror [SE-120]\x1b[0m: unknown field/s in field assignment expression."
            recordFileName = sourceName recordStart
            recordSourceLines = toModuleAST M.! recordFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Field \x1b[31m" <> T.pack field <>
                    "\x1b[0m is not a field of the type \x1b[31m" <> T.pack recordId <> "\x1b[0m.")) >>
            printSimpleError
                recordSourceLines ("The type \x1b[31m" <> T.pack recordId <> "\x1b[0m is defined here:") recordFileName
                recordPos Nothing
    EFieldValueAssignmentUnknownFields (recordId, recordPos@(Position recordStart _end)) fields ->
        let title = "\x1b[31merror [SE-120]\x1b[0m: unknown field/s in field assignment expression."
            recordFileName = sourceName recordStart
            recordSourceLines = toModuleAST M.! recordFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Fields \x1b[31m" <> T.intercalate ", " (map T.pack fields) <>
                    "\x1b[0m are not fields of the type \x1b[31m" <> T.pack recordId <> "\x1b[0m.")) >>
            printSimpleError
                recordSourceLines ("The type \x1b[31m" <> T.pack recordId <> "\x1b[0m is defined here:") recordFileName
                recordPos Nothing
    EFieldNotFixedLocation fieldName ty ->
        let title = "\x1b[31merror [SE-121]\x1b[0m: field is not a fixed-location field."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Field \x1b[31m" <> T.pack fieldName <>
                    "\x1b[0m of type \x1b[31m" <> showText ty <>
                    "\x1b[0m is not a fixed-location field."))
    EFieldNotAccessPort fieldName ty ->
        let title = "\x1b[31merror [SE-122]\x1b[0m: field is not an access port field."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Field \x1b[31m" <> T.pack fieldName <>
                    "\x1b[0m of type \x1b[31m" <> showText ty <>
                    "\x1b[0m is not an access port field."))
    EFieldNotSinkOrInboundPort fieldName ty ->
        let title = "\x1b[31merror [SE-123]\x1b[0m: field is not a sink or inbound port field."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Field \x1b[31m" <> T.pack fieldName <>
                    "\x1b[0m of type \x1b[31m" <> showText ty <>
                    "\x1b[0m is not a sink or inbound port field."))
    EFieldNotOutboundPort fieldName ty ->
        let title = "\x1b[31merror [SE-124]\x1b[0m: field is not an outbound port field."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Field \x1b[31m" <> T.pack fieldName <>
                    "\x1b[0m of type \x1b[31m" <> showText ty <>
                    "\x1b[0m is not an outbound port field."))
    EMemberAccessInvalidType ty ->
        let title = "\x1b[31merror [SE-125]\x1b[0m: invalid member access type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid type for member access."))
    EMemberFunctionCallInvalidType ty -> 
        let title = "\x1b[31merror [SE-126]\x1b[0m: invalid member function call type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid type for member function call."))
    EMemberAccessUnknownField (recordId, recordPos@(Position recordStart _end)) field ->
        let title = "\x1b[31merror [SE-127]\x1b[0m: unknown field in member access."
            recordFileName = sourceName recordStart
            recordSourceLines = toModuleAST M.! recordFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Field \x1b[31m" <> T.pack field <>
                    "\x1b[0m is not a field of the type \x1b[31m" <> T.pack recordId <> "\x1b[0m.")) >>
            printSimpleError
                recordSourceLines ("The type \x1b[31m" <> T.pack recordId <> "\x1b[0m is defined here:") recordFileName
                recordPos Nothing
    EInvalidProcedureCallInsideMemberFunction -> 
        let title = "\x1b[31merror [SE-128]\x1b[0m: invalid procedure call inside member function."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just "Procedure calls are not allowed inside member functions.")
    EConstantOutRange ty ->
        let title = "\x1b[31merror [SE-129]\x1b[0m: constant out of range."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The constant value \x1b[31m" <> showText ty <> "\x1b[0m is out of range for its type."))
    EForIteratorInvalidType ty -> 
        let title = "\x1b[31merror [SE-130]\x1b[0m: invalid type for for-loop iterator."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid type for a for-loop iterator."))
    EUsedTypeName ident prevPos@(Position prevStart _) ->
        let title = "\x1b[31merror [SE-131]\x1b[0m: type name already used."
            prevFileName = sourceName prevStart
            prevSourceLines = toModuleAST M.! prevFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type cannot be defined because the symbol \x1b[31m" <> T.pack ident <> "\x1b[0m is already in use.")) >>
            printSimpleError
                prevSourceLines "The symbol is previously used here:" prevFileName
                prevPos Nothing
    EUsedGlobalName ident prevPos@(Position prevStart _) ->
        let title = "\x1b[31merror [SE-132]\x1b[0m: global name already used."
            prevFileName = sourceName prevStart
            prevSourceLines = toModuleAST M.! prevFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The global object cannot be declared because the symbol \x1b[31m" <> T.pack ident <> "\x1b[0m is already in use.")) >>
            printSimpleError
                prevSourceLines "The symbol is previously used here:" prevFileName
                prevPos Nothing
    EUsedFunName ident prevPos@(Position prevStart _) ->
        let title = "\x1b[31merror [SE-133]\x1b[0m: function name already used."
            prevFileName = sourceName prevStart
            prevSourceLines = toModuleAST M.! prevFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The function cannot be declared because the symbol \x1b[31m" <> T.pack ident <> "\x1b[0m is already in use.")) >>
            printSimpleError
                prevSourceLines "The symbol is previously used here:" prevFileName
                prevPos Nothing
    EAccessPortConnectionInvalidGlobal ident ->
        let title = "\x1b[31merror [SE-134]\x1b[0m: invalid global object in access port connection."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The global object \x1b[31m" <> T.pack ident <> "\x1b[0m cannot be used in an access port connection."))
    EAccessPortConnectionInterfaceNotProvided ident iface ->
        let title = "\x1b[31merror [SE-135]\x1b[0m: resource does not provide the interface"
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Resource \x1b[31m" <> T.pack ident <>
                    "\x1b[0m does not provide the interface \x1b[31m" <> T.pack iface <> "\x1b[0m."))
    ESinkPortConnectionInvalidGlobal ident ->
        let title = "\x1b[31merror [SE-136]\x1b[0m: invalid sink port connection."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The global object \x1b[31m" <> T.pack ident <> "\x1b[0m cannot be connected to a sink port."))
    EInboundPortConnectionInvalidObject ident ->
        let title = "\x1b[31merror [SE-137]\x1b[0m: invalid inbound port connection."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The object \x1b[31m" <> T.pack ident <> "\x1b[0m cannot be connected to an inbound port."))
    EOutboundPortConnectionInvalidGlobal ident ->
        let title = "\x1b[31merror [SE-138]\x1b[0m: invalid outbound port connection."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The global object \x1b[31m" <> T.pack ident <> "\x1b[0m cannot be connected to an outbound port."))
    EAllocatorPortConnectionInvalidGlobal ident ->
        let title = "\x1b[31merror [SE-139]\x1b[0m: invalid allocator port connection."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The global object \x1b[31m" <> T.pack ident <> "\x1b[0m cannot be connected to an allocator port."))
    EAtomicAccessPortConnectionInvalidGlobal ident ->
        let title = "\x1b[31merror [SE-140]\x1b[0m: invalid atomic access port connection."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The global object \x1b[31m" <> T.pack ident <> "\x1b[0m cannot be connected to an atomic access port."))
    EAtomicArrayAccessPortConnectionInvalidGlobal ident ->
        let title = "\x1b[31merror [SE-141]\x1b[0m: invalid atomic array access port connection."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The global object \x1b[31m" <> T.pack ident <> "\x1b[0m cannot be connected to an atomic array access port."))
    EStructDefNotUniqueField [fieldName] ->
        let title = "\x1b[31merror [SE-142]\x1b[0m: duplicate field in struct definition."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Field \x1b[31m" <> T.pack fieldName <> "\x1b[0m is duplicated in the struct definition."))
    EStructDefNotUniqueField fieldNames ->
        let title = "\x1b[31merror [SE-142]\x1b[0m: duplicate field in struct definition."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Fields \x1b[31m" <> T.intercalate ", " (map T.pack fieldNames) <>
                    "\x1b[0m are duplicated in the struct definition."))
    EEnumDefNotUniqueVariant [variantName] ->
        let title = "\x1b[31merror [SE-143]\x1b[0m: duplicate variant in enum definition."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Variant \x1b[31m" <> T.pack variantName <> "\x1b[0m is duplicated in the enum definition."))
    EEnumDefNotUniqueVariant variantNames ->
        let title = "\x1b[31merror [SE-143]\x1b[0m: duplicate variant in enum definition."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Variants \x1b[31m" <> T.intercalate ", " (map T.pack variantNames) <>
                    "\x1b[0m are duplicated in the enum definition."))
    EInterfaceNotUniqueProcedure [procName] ->
        let title = "\x1b[31merror [SE-144]\x1b[0m: duplicate procedure in interface definition."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Procedure \x1b[31m" <> T.pack procName <> "\x1b[0m is duplicated in the interface definition."))
    EInterfaceNotUniqueProcedure procNames ->
        let title = "\x1b[31merror [SE-144]\x1b[0m: duplicate procedure in interface definition."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Procedures \x1b[31m" <> T.intercalate ", " (map T.pack procNames) <>
                    "\x1b[0m are duplicated in the interface definition."))
    EClassLoop funcNames ->
        let title = "\x1b[31merror [SE-145]\x1b[0m: Loop between member function calls in class definition."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The member functions \x1b[31m" <> T.intercalate " -> " (map T.pack funcNames) <>
                    "\x1b[0m form a recursive calling loop in the class definition."))
    EDereferenceInvalidType ty ->
        let title = "\x1b[31merror [SE-146]\x1b[0m: invalid type for dereference."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m cannot be dereferenced."))
    EMatchInvalidType ty ->
        let title = "\x1b[31merror [SE-147]\x1b[0m: invalid type for match statement."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid type for match statement."))
    EMatchCaseDuplicate variantName prevCase@(Position prevStart _) ->
        let title = "\x1b[31merror [SE-148]\x1b[0m: duplicate case in match statement."
            prevFileName = sourceName prevStart
            prevSourceLines = toModuleAST M.! prevFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Variant \x1b[31m" <> T.pack variantName <> "\x1b[0m is duplicated in the match statement.")) >>
            printSimpleError
                prevSourceLines "The variant is previously used here:" prevFileName
                prevCase Nothing
    EMatchCaseUnknownVariants [variantName] ->
        let title = "\x1b[31merror [SE-149]\x1b[0m: unknown variant/s in match case."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Variant \x1b[31m" <> T.pack variantName <> "\x1b[0m is not a valid variant of the enum or option."))
    EMatchCaseUnknownVariants variantNames ->
        let title = "\x1b[31merror [SE-149]\x1b[0m: unknown variant/s in match case."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Variants \x1b[31m" <> T.intercalate ", " (map T.pack variantNames) <>
                    "\x1b[0m are not valid variants of the enum or option."))
    EMatchMissingCases [caseIdent] -> 
        let title = "\x1b[31merror [SE-150]\x1b[0m: missing case/s in match statement."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Case \x1b[31m" <> T.pack caseIdent <> "\x1b[0m is missing in the match statement."))
    EMatchMissingCases caseIdents ->
        let title = "\x1b[31merror [SE-150]\x1b[0m: missing case/s in match statement."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Cases \x1b[31m" <> T.intercalate ", " (map T.pack caseIdents) <>
                    "\x1b[0m are missing in the match statement."))
    EIsVariantInvalidType ty ->
        let title = "\x1b[31merror [SE-151]\x1b[0m: invalid type for is-variant expression."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid type for is-variant expression."))
    EIsOptionVariantInvalidType ty ->
        let title = "\x1b[31merror [SE-152]\x1b[0m: invalid type for is-option-variant expression."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not an option type."))
    EIsVariantEnumTypeMismatch expectedEnum actualEnum ->
        let title = "\x1b[31merror [SE-153]\x1b[0m: type mismatch in is-variant expression."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The expected enum type is \x1b[31m" <> T.pack expectedEnum <>
                    "\x1b[0m but the actual type is \x1b[31m" <> T.pack actualEnum <> "\x1b[0m."))
    EOutboundPortInvalidProcedure ident ->
        let title = "\x1b[31merror [SE-154]\x1b[0m: invalid procedure in outbound port."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The procedure \x1b[31m" <> T.pack ident <> "\x1b[0m is not a valid procedure for an outbound port."))
    EInvalidPoolInitialization -> 
        let title = "\x1b[31merror [SE-155]\x1b[0m: invalid pool initialization."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just "A pool object cannot be initialized with a value.")
    EAtomicUninitialized -> 
        let title = "\x1b[31merror [SE-156]\x1b[0m: atomic object is uninitialized."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just "Atomic objects must be initialized with a value.")
    EAtomicArrayUninitialized ->
        let title = "\x1b[31merror [SE-157]\x1b[0m: atomic array object is uninitialized."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just "Atomic array objects must be initialized with a value.")
    _ -> putStrLn $ show pos ++ ": " ++ show e
-- | Print the error as is
ppError _ (AnnotatedError e pos) = putStrLn $ show pos ++ ": " ++ show e
