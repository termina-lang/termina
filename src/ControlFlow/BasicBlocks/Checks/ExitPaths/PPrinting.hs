{-# LANGUAGE FlexibleInstances #-}

module ControlFlow.BasicBlocks.Checks.ExitPaths.PPrinting where

{--
ppError :: M.Map FilePath TL.Text ->
    PathsCheckError -> IO ()
ppError toModuleAST (AnnotatedError e pos@(Position startPos _endPos)) =
  let fileName = sourceName startPos
      sourceLines = toModuleAST M.! fileName
  in
  case e of
    EEInvalidReturn ->
        let title = "\x1b[31merror [EE-001]\x1b[0m: invalid return statement."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Invalid return statement.\n" <>
                       "Return statements are only allowed as the last statement of a function."))
    EEInvalidContinue ->
        let title = "\x1b[31merror [EE-002]\x1b[0m: invalid continue statement."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Invalid continue statement.\n" <>
                       "Continue statements are only allowed inside actions."))
    EEInvalidSend ->
        let title = "\x1b[31merror [EE-003]\x1b[0m: invalid send statement."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Invalid send statement.\n" <>
                       "Send statements are only allowed inside actions."))
    EEBlockShallExit ->
        let title = "\x1b[31merror [EE-004]\x1b[0m: missing return statement."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Missing return statement.\n" <>
                       "All functions must have a return statement, even if they return nothing."))
    EEActionShallExit ->
        let title = "\x1b[31merror [EE-005]\x1b[0m: missing exit point on an action."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Missing exit point on an action.\n" <>
                       "All the possible execution paths of an action must have an exit point (return or continue)"))
    EEActionInvalidContinue ->
        let title = "\x1b[31merror [EE-006]\x1b[0m: invalid continue statement on an action."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Invalid continue statement.\n" <>
                       "Continue statements are only allowed as the last statement of an execution path of an action."))
    EEActionInvalidSend ->
        let title = "\x1b[31merror [EE-007]\x1b[0m: invalid send statement on an action."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Invalid send statement.\n" <>
                       "Send statements are only allowed at the end of an execution path of an action."))
    EEActionIfBlockShallExit ->
        let title = "\x1b[31merror [EE-008]\x1b[0m: missing continue statement on an action if block."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Missing continue statement.\n" <>
                       "This if block is the last statement of an action and all its branches must have an exit point in the form of a continue statement."))
    EEActionMatchBlockShallExit ->
        let title = "\x1b[31merror [EE-009]\x1b[0m: missing continue statement on an action match block."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Missing continue statement.\n" <>
                       "This match block is the last statement of an action and all its cases must have an exit point in the form of a continue statement."))
    EEActionIfBlockShallNotExit ->
        let title = "\x1b[31merror [EE-010]\x1b[0m: if block shall not exit."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Invalid continue statement.\n" <>
                       "This if block is not the last statement of an action and thus at least one of its branches must not have an exit point."))
    EEActionMatchBlockShallNotExit ->
        let title = "\x1b[31merror [EE-011]\x1b[0m: match block shall not exit."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Invalid continue statement.\n" <>
                       "This match block is not the last statement of an action and thus at least one of its cases must not have an exit point."))
    EEActionIfBlockMissingElseExit ->
        let title = "\x1b[31merror [EE-012]\x1b[0m: missing else exit on an action if block."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Missing continue statement.\n" <>
                       "This if block is the last statement of an action and all its branches must have an exit point in the form of a continue statement. Thus, it must have an else branch."))
    _ -> putStrLn $ show pos ++ ": " ++ show e
-- | Print the error as is
ppError _ (AnnotatedError e pos) = putStrLn $ show pos ++ ": " ++ show e
--}