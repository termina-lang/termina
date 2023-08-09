module PPrinter.Function where

import Prettyprinter

import SemanAST
import PPrinter.Common
import Semantic.Monad (SemanticAnns)
import PPrinter.Expression
import PPrinter.Statement
import Data.Map

ppBlockRet :: BlockRet SemanticAnns -> DocStyle
ppBlockRet (BlockRet body (ReturnStmt (Just expr) _)) =
  braces' ( 
    indentTab . align $ 
      line <> vsep [ppStatement empty s <> line | s <- body] 
      <> line <> returnC <+> ppExpression empty expr <> semi <> line
  )
ppBlockRet (BlockRet body (ReturnStmt Nothing _)) =
  braces' (
    indentTab . align $
      line <> vsep [ppStatement empty s <> line | s <- body] 
      <> line <> returnC <> semi <> line
  )

ppFunction :: AnnASTElement SemanticAnns -> DocStyle
ppFunction (Function identifier parameters rTS blk _ _) =
  ppCFunctionDeclaration (pretty identifier) (ppParameterDeclaration <$> parameters) (ppReturnType <$> rTS)
    <+> ppBlockRet blk
ppFunction _ = error "AST element is not a function"

ppFunctionDeclaration :: AnnASTElement SemanticAnns -> DocStyle
ppFunctionDeclaration (Function identifier parameters rTS _ _ _) =
  ppCFunctionDeclaration (pretty identifier) (ppParameterDeclaration <$> parameters) (ppReturnType <$> rTS) <> semi
ppFunctionDeclaration _ = error "AST element is not a function"