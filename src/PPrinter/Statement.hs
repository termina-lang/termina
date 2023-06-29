module PPrinter.Statement where

import Prettyprinter

import AST
import PPrinter.Common
import Semantic.Monad
import PPrinter.Expression

ppStatement :: Statement SemanticAnns -> DocStyle
ppStatement (SingleExpStmt expr _) = ppExpression expr <> semi

