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
        let block = [ 
                Block
                    fancyRedStyle
                    (fileName, lineNumber, lineColumn)
                    Nothing
                    [Pointer lineNumber lineColumn (lineColumn + 1) False Nothing fancyRedPointer]
                    Nothing
                ]
            title = Just "error[E001]: invalid array indexing."
        in
        case ts of
            (Slice _) -> 
                TL.putStrLn $ prettyErrors 
                    sourceLines 
                    [ 
                        Errata
                            title
                            block
                            (Just $ "You cannot index a slice. It is not an array. \n" <>
                                    "A slice can only be used to create references  to a part of an array.")
                    ]
            _ -> 
                TL.putStrLn $ prettyErrors 
                    sourceLines 
                    [ 
                        Errata
                            title
                            block
                            (Just "You are trying to index an object that is not an array")
                    ]
    (ENotNamedObject ident) -> 
        let block = [ 
                Block
                    fancyRedStyle
                    (fileName, lineNumber, lineColumn)
                    Nothing
                    [Pointer lineNumber lineColumn (lineColumn + length ident) False Nothing fancyRedPointer]
                    Nothing
                ]
            title = Just "error[E002]: undeclared variable."
        in
        TL.putStrLn $ prettyErrors 
            sourceLines 
            [ 
                Errata
                    title
                    block
                    (Just $ "The variable \x1b[31m" <> T.pack ident <> "\x1b[0m has not been declared")
            ]
    (ENotConstant ident) -> 
        let block = [ 
                Block
                    fancyRedStyle
                    (fileName, lineNumber, lineColumn)
                    Nothing
                    [Pointer lineNumber lineColumn (lineColumn + length ident) False Nothing fancyRedPointer]
                    Nothing
                ]
            title = Just "error[E003]: invalid use of a non-constant object."
        in
        TL.putStrLn $ prettyErrors 
            sourceLines 
            [ 
                Errata
                    title
                    block
                    (Just $ "The object \x1b[31m" <> T.pack ident <> "\x1b[0m is not a constant.")
            ]
    EAssignmentToImmutable -> 
        let block = [ 
                Block
                    fancyRedStyle
                    (fileName, lineNumber, lineColumn)
                    Nothing
                    [Pointer lineNumber lineColumn (lineColumn + 1) False Nothing fancyRedPointer]
                    Nothing
                ]
            title = Just "error[E004]: assignment to immutable variable."
        in
        TL.putStrLn $ prettyErrors 
            sourceLines 
            [ 
                Errata
                    title
                    block
                    (Just "You are trying to assign a value to an immutable object.")
            ]
    EIfElseNoOtherwise ->
        let block = [ 
                Block
                    fancyRedStyle
                    (fileName, lineNumber, lineColumn)
                    Nothing
                    [Pointer lineNumber lineColumn (lineColumn + 2) False Nothing fancyRedPointer]
                    Nothing
                ]
            title = Just "error[E005]: missing else clause."
        in
        TL.putStrLn $ prettyErrors 
            sourceLines 
            [ 
                Errata
                    title
                    block
                    (Just $ "You are missing the else clause in an if-else-if statement.\n" <>
                            "You must provide an else clause if you are not providing an else-if clause.")
            ]
    _ -> putStrLn $ show pos ++ ": " ++ show e
-- | Print the error as is
ppError _ (AnnError e pos) = putStrLn $ show pos ++ ": " ++ show e
