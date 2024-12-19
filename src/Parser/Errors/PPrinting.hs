-- | Semantic Error Printing
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Parser.Errors.PPrinting where

{--
ppError :: M.Map FilePath T.Text ->
    ParsingErrors -> IO ()
ppError toModuleAST (AnnotatedError e pos@(Position start _end)) =
  let fileName = sourceName start
      sourceLines = toModuleAST M.! fileName
  in
  case e of
    (EImportedFileNotFound qname) -> 
        let title = "\x1b[31merror [CE-001]\x1b[0m: imported file not found"
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Imported file invalid or not found: " <> T.pack (show qname)))
-- | Print the error as is
ppError _ (AnnotatedError e pos) = putStrLn $ show pos ++ ": " ++ show e
--}