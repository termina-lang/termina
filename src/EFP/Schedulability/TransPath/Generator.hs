module EFP.Schedulability.TransPath.Generator where
import ControlFlow.BasicBlocks.AST
import Semantic.Types
import EFP.Schedulability.TransPath.AST

genWCEPathBlock :: [WCEPathBlock] -> [BasicBlock SemanticAnn] -> [WCEPathBlock]
genWCEPathBlock _ _ = undefined



genClassMemberWCEPs :: Identifier -> ClassMember SemanticAnn -> [TransactionalWCEPath]
genClassMemberWCEPs className (ClassMethod _ak ident _params _mrty blk _) = []

genTransactionalWCEPS :: AnnotatedProgram SemanticAnn -> [TransactionalWCEPath]
genTransactionalWCEPS (TypeDefinition (Class _kind ident members _ _) _ : xs) = 
    let classWCEPs = concatMap (genClassMemberWCEPs ident) members
    in classWCEPs ++ genTransactionalWCEPS xs
genTransactionalWCEPS (_ : xs) = genTransactionalWCEPS xs
genTransactionalWCEPS [] = []