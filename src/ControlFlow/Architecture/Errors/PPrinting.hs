{-# LANGUAGE FlexibleInstances #-}

module ControlFlow.Architecture.Errors.PPrinting where

{--
ppError :: M.Map FilePath TL.Text ->
    ArchitectureError -> IO ()
ppError toModuleAST (AnnotatedError e pos@(Position startPos _endPos)) =
  let fileName = sourceName startPos
      sourceLines = toModuleAST M.! fileName
  in
  case e of
    EDuplicatedEmitterConnection emitter procPos@(Position procStart _procEnd) ->
        let title = "\x1b[31merror [AE-001]\x1b[0m: Duplicated emitter connection"
            procFileName = sourceName procStart
            procSourceLines = toModuleAST M.! procFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Emitter \x1b[31m" <> T.pack emitter <>
                    "\x1b[0m is already connected to a sink port. " <>
                    "Only one target is allowed per event source.")) >>
            printSimpleError
                procSourceLines "The previous connection was done here:" procFileName
                procPos Nothing
    EDuplicatedChannelConnection channel procPos@(Position procStart _procEnd) ->
        let title = "\x1b[31merror [AE-002]\x1b[0m: Duplicated channel connection"
            procFileName = sourceName procStart
            procSourceLines = toModuleAST M.! procFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Channel \x1b[31m" <> T.pack channel <>
                    "\x1b[0m is already connected to an input port. " <>
                    "Only one target is allowed per channel.")) >>
            printSimpleError
                procSourceLines "The previous connection was done here:" procFileName
                procPos Nothing
    EMismatchedBoxSource expectedSource actualSource boxTrace ->
        let title = "\x1b[31merror [AE-003]\x1b[0m: Mismatched box source" in
        printSimpleError
            sourceLines title fileName pos
            (Just ("Expected allocation from \x1b[31m" <> T.pack expectedSource <>
                "\x1b[0m but the box is being allocated from \x1b[31m" <> T.pack actualSource <>
                "\x1b[0m.")) >>
        printBoxTrace expectedSource (reverse boxTrace)
    EDisconnectedEmitter emitter ->
        let title = "\x1b[31merror [AE-004]\x1b[0m: Disconnected emitter" in
        printSimpleError
            sourceLines title fileName pos
            (Just ("Emitter \x1b[31m" <> T.pack emitter <>
                "\x1b[0m is not connected to any sink port. " <>
                "All event sources must be connected to a target.")) 
    EChannelWithoutSources channel ->
        let title = "\x1b[31merror [AE-005]\x1b[0m: Channel without sources" in
        printSimpleError
            sourceLines title fileName pos
            (Just ("Channel \x1b[31m" <> T.pack channel <>
                "\x1b[0m is not connected to any outbound port. " <>
                "All channels must have at least one source.")) 
    EChannelWithoutTarget channel -> 
        let title = "\x1b[31merror [AE-006]\x1b[0m: Channel without target" in
        printSimpleError
            sourceLines title fileName pos
            (Just ("Channel \x1b[31m" <> T.pack channel <>
                "\x1b[0m is not connected to any inbound port. " <>
                "All channels must be connected to a target.")) 
    EUnusedResource resId ->
        let title = "\x1b[31merror [AE-007]\x1b[0m: Unused resource" in
        printSimpleError
            sourceLines title fileName pos
            (Just ("Resource \x1b[31m" <> T.pack resId <>
                "\x1b[0m is not being used by any element. " <>
                "All resources must be connected to at least one access port."))
    EUnusedPool poolId ->
        let title = "\x1b[31merror [AE-008]\x1b[0m: Unused pool" in
        printSimpleError
            sourceLines title fileName pos
            (Just ("Pool \x1b[31m" <> T.pack poolId <>
                "\x1b[0m is not being used by any element. " <>
                "All pools must be connected to at least one access port."))
    _ -> putStrLn $ show pos ++ ": " ++ show e


    where

        -- | Prints a trace of box allocations 
        printBoxTrace :: Identifier -> [Location] -> IO ()
        printBoxTrace _ [] = return ()
        printBoxTrace expectedSource [tracePos@(Position traceStartPos _)] =
            let title = "The box is being freed here to allocator \x1b[31m" <> T.pack expectedSource <> "\x1b[0m:"
                traceFileName = sourceName traceStartPos
                traceSourceLines = toModuleAST M.! traceFileName
            in
                printSimpleError 
                    traceSourceLines title traceFileName tracePos Nothing
        printBoxTrace expectedSource (tracePos@(Position traceStartPos _) : xr) =
            let title = "The box is first moved here:"
                traceFileName = sourceName traceStartPos
                traceSourceLines = toModuleAST M.! traceFileName
            in
                printSimpleError
                    traceSourceLines title traceFileName tracePos Nothing >> printBoxTrace' expectedSource xr
        printBoxTrace _ _ = error "Internal error: invalid error position"

        printBoxTrace' :: Identifier -> [Location] -> IO ()
        printBoxTrace' _ [] = return ()
        printBoxTrace' expectedSource [tracePos@(Position traceStartPos _)] =
            let title = "Finally, box is being freed here to allocator \x1b[31m" <> T.pack expectedSource <> "\x1b[0m:"
                traceFileName = sourceName traceStartPos
                traceSourceLines = toModuleAST M.! traceFileName
            in
                printSimpleError 
                    traceSourceLines title traceFileName tracePos Nothing
        printBoxTrace' expectedSource (tracePos@(Position traceStartPos _) : xr) =
            let title = "The box is moved again here:"
                traceFileName = sourceName traceStartPos
                traceSourceLines = toModuleAST M.! traceFileName
            in
                printSimpleError
                    traceSourceLines title traceFileName tracePos Nothing >> printBoxTrace' expectedSource xr
        printBoxTrace' _ _ = error "Internal error: invalid error position"
-- | Print the error as is
ppError _ (AnnotatedError (EDisconnectedEmitter emitterId) Internal) =
    TIO.putStrLn "\x1b[31merror [AE-004]\x1b[0m: Disconnected emitter" >>
    TIO.putStrLn ("Emitter \x1b[31m" <> T.pack emitterId <>
        "\x1b[0m is not connected to any sink port. " <>
        "All event sources must be connected to a target.")
ppError _ (AnnotatedError e pos) = putStrLn $ show pos ++ ": " ++ show e
--}