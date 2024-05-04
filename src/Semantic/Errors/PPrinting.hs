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



printSimpleError :: TL.Text -> T.Text -> String -> Int -> Int -> Int -> T.Text -> IO ()
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
            (Just msg) 


-- useful prettyprinter doc
-- https://hackage.haskell.org/package/prettyprinter-1.7.1/docs/Prettyprinter.html
ppError :: TL.Text -> SemanticErrors -> IO ()
ppError sourceLines (AnnError e (Position pos)) =
  let fileName = sourceName pos
      lineNumber = sourceLine pos
      lineColumn = sourceColumn pos
  in
  case e of
    (EArray ts) -> 
        let title = "error[E001]: invalid array indexing."
        in
        case ts of
            (Slice _) -> printSimpleError 
                sourceLines title fileName 
                lineNumber lineColumn 1 
                ("You cannot index a slice. It is not an array. \n" <>
                "A slice can only be used to create references  to a part of an array.")
            _ -> printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 1
                "You are trying to index an object that is not an array."
    (ENotNamedObject ident) -> 
        let title = "error[E002]: undeclared variable."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn (length ident)
                ("The variable \x1b[31m" <> T.pack ident <> "\x1b[0m has not been declared")
    (ENotConstant ident) -> 
        let 
            title = "error[E003]: invalid use of a non-constant object."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn (length ident)
                ("The object \x1b[31m" <> T.pack ident <> "\x1b[0m is not a constant.")
    EAssignmentToImmutable -> 
        let title = "error[E004]: assignment to immutable variable."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 1
                "You are trying to assign a value to an immutable object."
    EIfElseNoOtherwise ->
        let title = "error[E005]: missing else clause."
        in
            printSimpleError
                sourceLines title fileName
                lineNumber lineColumn 2
                ("You are missing the else clause in an if-else-if statement.\n" <>
                "You must provide an else clause if you are defining an else-if clause.")
    _ -> putStrLn $ show pos ++ ": " ++ show e
-- | Print the error as is
ppError _ (AnnError e pos) = putStrLn $ show pos ++ ": " ++ show e
