{-# LANGUAGE OverloadedStrings #-}

module EFP.Schedulability.Core.AST
    (module EFP.Schedulability.Core.AST
    , module Core.AST) where

import Core.AST
import Utils.Annotations
import Utils.Errors
import Data.Text (pack)

data ConstExprType =
    TConstInt
    | TConstDouble
    deriving (Show, Eq)

instance ShowText ConstExprType where
    showText TConstInt = "integer"
    showText TConstDouble = "floating-point"

data ConstExpression a
    = ConstInt TInteger a
    | ConstDouble Double a
    | ConstObject Identifier a
    | ConstBinOp Op (ConstExpression a) (ConstExpression a) a
    deriving Show

instance ShowText (ConstExpression a) where
    showText (ConstObject ident _) = pack ident
    showText (ConstInt c _) = showText c
    showText (ConstDouble d _) = pack (show d)
    showText (ConstBinOp op lhe rhe _) = showText lhe <> " " <> showText op <> " " <> showText rhe

instance Annotated ConstExpression where
  getAnnotation (ConstInt _ a)       = a
  getAnnotation (ConstDouble _ a)     = a
  getAnnotation (ConstObject _ a)    = a
  getAnnotation (ConstBinOp _ _ _ a) = a

  updateAnnotation (ConstInt v _)           = ConstInt v
  updateAnnotation (ConstDouble v _)         = ConstDouble v
  updateAnnotation (ConstObject ident _)    = ConstObject ident
  updateAnnotation (ConstBinOp op e1 e2 _)  = ConstBinOp op e1 e2
