{-# LANGUAGE FlexibleContexts #-}

module ControlFlow.Common where

import qualified Semantic.AST as SAST
import Core.AST
import ControlFlow.AST
import Semantic.Types
import Control.Monad.Except
import Utils.Annotations

newtype BBGeneratorError = InternalError String
    deriving (Show)

type BBGenerator = Except BBGeneratorError

-- | This function returns the type of an object. The type is extracted from the
-- object's semantic annotation. The function assumes that the object is well-typed
-- and that the semantic annotation is correct. If the object is not well-typed, the
-- function will throw an error.
getObjType :: (MonadError BBGeneratorError m) => SAST.Object SemanticAnn -> m TypeSpecifier
getObjType (SAST.Variable _ (Located (ETy (ObjectType _ ts)) _))                  = return ts
getObjType (SAST.ArrayIndexExpression _ _ (Located (ETy (ObjectType _ ts)) _))    = return ts
getObjType (SAST.MemberAccess _ _ (Located (ETy (ObjectType _ ts)) _))            = return ts
getObjType (SAST.Dereference _ (Located (ETy (ObjectType _ ts)) _))               = return ts
getObjType (SAST.Unbox _ (Located (ETy (ObjectType _ ts)) _))                     = return ts
getObjType (SAST.DereferenceMemberAccess _ _ (Located (ETy (ObjectType _ ts)) _)) = return ts
getObjType ann = throwError $ InternalError $ "invalid object annotation: " ++ show ann

getExprType :: (MonadError BBGeneratorError m) => SAST.Expression SemanticAnn -> m TypeSpecifier
getExprType (SAST.AccessObject obj) = getObjType obj
getExprType (SAST.Constant _ (Located (ETy (SimpleType ts)) _)) = return ts
getExprType (SAST.OptionVariantInitializer _ (Located (ETy (SimpleType ts)) _)) = return ts
getExprType (SAST.BinOp _ _ _ (Located (ETy (SimpleType ts)) _)) = return ts
getExprType (SAST.ReferenceExpression _ _ (Located (ETy (SimpleType ts)) _)) = return ts
getExprType (SAST.Casting _ _ (Located (ETy (SimpleType ts)) _)) = return ts
getExprType (SAST.FunctionCall _ _ (Located (ETy (AppType _ ts)) _)) = return ts
getExprType (SAST.MemberFunctionCall _ _ _ (Located (ETy (AppType _ ts)) _)) = return ts
getExprType (SAST.DerefMemberFunctionCall _ _ _ (Located (ETy (AppType _ ts)) _)) = return ts
getExprType (SAST.StructInitializer _ _ (Located (ETy (SimpleType ts)) _)) = return ts
getExprType (SAST.EnumVariantInitializer _ _ _ (Located (ETy (SimpleType ts)) _)) = return ts
getExprType (SAST.ArrayInitializer _ _ (Located (ETy (SimpleType ts)) _)) = return ts
getExprType (SAST.ArrayExprListInitializer _ (Located (ETy (SimpleType ts)) _)) = return ts
getExprType ann = throwError $ InternalError $ "invalid expression annotation: " ++ show ann

getPortName :: (MonadError BBGeneratorError m) => SAST.Object SemanticAnn -> m Identifier
getPortName obj = do
    obj_type <- getObjType obj
    case obj_type of 
        AccessPort _ -> 
            case obj of
                (SAST.MemberAccess _ portName _) -> return portName
                (SAST.DereferenceMemberAccess _ portName _) -> return portName
                _ -> throwError $ InternalError ("unexpected object type" ++ show obj_type)
        OutPort _ -> 
            case obj of
                (SAST.MemberAccess _ portName _) -> return portName
                (SAST.DereferenceMemberAccess _ portName _) -> return portName
                _ -> throwError $ InternalError ("unexpected object type" ++ show obj_type)
        _ -> throwError $ InternalError "object is not an accessible port"

appendRegularBlock :: [BasicBlock SemanticAnn] -> BasicBlock SemanticAnn -> [SAST.Statement SemanticAnn] -> BBGenerator [BasicBlock SemanticAnn]
appendRegularBlock acc currBlock [] = return $ currBlock : acc
appendRegularBlock acc currBlock@(RegularBlock currStmts) (stmt : xs) = 
    case stmt of
        SAST.SingleExpStmt expr ann -> 
            case expr of
                SAST.MemberFunctionCall obj _ _ _ -> do
                    obj_ty <- getObjType obj
                    case obj_ty of
                        -- | If the object is of a defined type, it means that we are calling an inner function of the
                        -- object, so we shall create a new regular block
                        DefinedType _ -> 
                            appendRegularBlock acc (RegularBlock (SingleExpStmt expr ann : currStmts)) xs
                        Reference {} -> 
                            appendRegularBlock acc (RegularBlock (SingleExpStmt expr ann : currStmts)) xs
                        _ -> genBBlocks (currBlock : acc) (stmt : xs)
                SAST.DerefMemberFunctionCall obj _ _ _ -> do
                    obj_ty <- getObjType obj
                    case obj_ty of
                        DefinedType _ -> 
                            appendRegularBlock acc (RegularBlock (SingleExpStmt expr ann : currStmts)) xs
                        Reference {} -> 
                            appendRegularBlock acc (RegularBlock (SingleExpStmt expr ann : currStmts)) xs
                        _ -> genBBlocks (currBlock : acc) (stmt : xs)
                _ -> appendRegularBlock acc (RegularBlock (SingleExpStmt expr ann : currStmts)) xs 
        SAST.Declaration name accessKind typeSpecifier expr ann -> 
            appendRegularBlock acc (RegularBlock (Declaration name accessKind typeSpecifier expr ann : currStmts)) xs
        SAST.AssignmentStmt obj expr ann -> appendRegularBlock acc (RegularBlock (AssignmentStmt obj expr ann : currStmts)) xs
        _ -> genBBlocks (currBlock : acc) (stmt : xs)
appendRegularBlock _ _ _ = error "appendRegularBlock: unexpected block type"

genElseIfBBlocks :: SAST.ElseIf SemanticAnn -> BBGenerator (ElseIf SemanticAnn)
genElseIfBBlocks (SAST.ElseIf condition elseIfStmts ann) = do
    blocks <- genBBlocks [] (reverse elseIfStmts)
    return $ ElseIf condition blocks ann

genMatchCaseBBlocks :: SAST.MatchCase SemanticAnn -> BBGenerator (MatchCase SemanticAnn)
genMatchCaseBBlocks (SAST.MatchCase identifier args caseStmts ann) = do
    blocks <- genBBlocks [] (reverse caseStmts)
    return $ MatchCase identifier args blocks ann

genBBlocks :: [BasicBlock SemanticAnn] -> [SAST.Statement SemanticAnn] -> BBGenerator [BasicBlock SemanticAnn]
genBBlocks acc [] = return acc
genBBlocks acc (stmt : xs) = 
    case stmt of
        SAST.SingleExpStmt expr ann -> 
            case expr of
                SAST.MemberFunctionCall obj funcName args ann' -> do
                    obj_ty <- getObjType obj
                    case obj_ty of
                        -- | If the object is of a defined type, it means that we are calling an inner function of the
                        -- object, so we shall create a new regular block
                        DefinedType _ -> 
                            appendRegularBlock acc (RegularBlock [SingleExpStmt expr ann]) xs
                        Reference {} -> 
                            appendRegularBlock acc (RegularBlock [SingleExpStmt expr ann]) xs
                        AccessPort (DefinedType {}) -> 
                            genBBlocks (ProcedureCall obj funcName args ann' : acc) xs
                        AccessPort (Allocator _) -> do
                            -- | We need to check the operation (alloc or free)
                            case funcName of 
                                "alloc" -> case args of
                                    [opt] -> genBBlocks (AllocBox obj opt ann' : acc) xs
                                    _ -> throwError $ InternalError "unexpected number of arguments"
                                "free" -> case args of
                                    [elemnt] -> genBBlocks (FreeBox obj elemnt ann' : acc) xs
                                    _ -> throwError $ InternalError "unexpected number of arguments"
                                _ -> throwError $ InternalError "unexpected function name"
                        AccessPort (AtomicAccess _) -> do
                            -- | We need to check the operation (load or store)
                            case funcName of
                                "load" -> case args of
                                    [retval] -> genBBlocks (AtomicLoad obj retval ann' : acc) xs
                                    _ -> throwError $ InternalError "unexpected number of arguments"
                                "store" -> case args of
                                    [value] -> genBBlocks (AtomicStore obj value ann' : acc) xs
                                    _ -> throwError $ InternalError "unexpected number of arguments"
                                _ -> throwError $ InternalError "unexpected function name"
                        AccessPort (AtomicArrayAccess {}) -> do
                            -- | We need to check the operation (load or store)
                            case funcName of
                                "load_index" -> case args of
                                    [index, retval] -> genBBlocks (AtomicArrayLoad obj index retval ann' : acc) xs
                                    _ -> throwError $ InternalError "unexpected number of arguments"
                                "store_index" -> case args of
                                    [index, value] -> genBBlocks (AtomicArrayStore obj index value ann' : acc) xs
                                    _ -> throwError $ InternalError "unexpected number of arguments"
                                _ -> throwError $ InternalError "unexpected function name"
                        OutPort _ -> do
                            case funcName of
                                "send" -> case args of
                                    [msg] -> genBBlocks (SendMessage obj msg ann' : acc) xs
                                    _ -> throwError $ InternalError "unexpected number of arguments"
                                _ -> throwError $ InternalError "unexpected function name"
                        _ -> throwError $ InternalError ("unexpected object type" ++ show obj_ty)
                SAST.DerefMemberFunctionCall obj funcName args ann' -> do
                    obj_ty <- getObjType obj
                    case obj_ty of
                        DefinedType _ -> 
                            appendRegularBlock acc (RegularBlock [SingleExpStmt expr ann]) xs
                        Reference {} -> 
                            appendRegularBlock acc (RegularBlock [SingleExpStmt expr ann]) xs
                        AccessPort (DefinedType {}) -> 
                            genBBlocks (ProcedureCall obj funcName args ann' : acc) xs
                        AccessPort (Allocator _) -> do
                            -- | We need to check the operation (alloc or free)
                            case funcName of 
                                "alloc" -> case args of
                                    [opt] -> genBBlocks (AllocBox obj opt ann' : acc) xs
                                    _ -> throwError $ InternalError "unexpected number of arguments"
                                "free" -> case args of
                                    [elemnt] -> genBBlocks (FreeBox obj elemnt ann' : acc) xs
                                    _ -> throwError $ InternalError "unexpected number of arguments"
                                _ -> throwError $ InternalError "unexpected function name"
                        AccessPort (AtomicAccess _) -> do
                            -- | We need to check the operation (load or store)
                            case funcName of
                                "load" -> case args of
                                    [retval] -> genBBlocks (AtomicLoad obj retval ann' : acc) xs
                                    _ -> throwError $ InternalError "unexpected number of arguments"
                                "store" -> case args of
                                    [value] -> genBBlocks (AtomicStore obj value ann' : acc) xs
                                    _ -> throwError $ InternalError "unexpected number of arguments"
                                _ -> throwError $ InternalError "unexpected function name"
                        AccessPort (AtomicArrayAccess {}) -> do
                            -- | We need to check the operation (load or store)
                            case funcName of
                                "load_index" -> case args of
                                    [index, retval] -> genBBlocks (AtomicArrayLoad obj index retval ann' : acc) xs
                                    _ -> throwError $ InternalError "unexpected number of arguments"
                                "store_index" -> case args of
                                    [index, value] -> genBBlocks (AtomicArrayStore obj index value ann' : acc) xs
                                    _ -> throwError $ InternalError "unexpected number of arguments"
                                _ -> throwError $ InternalError "unexpected function name"
                        OutPort _ -> do
                            case funcName of
                                "send" -> case args of
                                    [msg] -> genBBlocks (SendMessage obj msg ann' : acc) xs
                                    _ -> throwError $ InternalError "unexpected number of arguments"
                                _ -> throwError $ InternalError "unexpected function name"
                        _ -> throwError $ InternalError ("unexpected object type" ++ show obj_ty)
                _ -> appendRegularBlock acc (RegularBlock [SingleExpStmt expr ann]) xs
        SAST.Declaration name accessKind typeSpecifier expr ann -> appendRegularBlock acc (RegularBlock [Declaration name accessKind typeSpecifier expr ann]) xs
        SAST.AssignmentStmt obj expr ann -> appendRegularBlock acc (RegularBlock [AssignmentStmt obj expr ann]) xs
        SAST.ForLoopStmt iterator typeSpecifier initial final breakCondition loopStmts ann -> do
            loopBlocks <- genBBlocks [] (reverse loopStmts) 
            genBBlocks (ForLoopBlock iterator typeSpecifier initial final breakCondition loopBlocks ann : acc) xs
        SAST.IfElseStmt condition ifStmts elseIfs elseStmts ann -> do
            ifBlocks <- genBBlocks [] (reverse ifStmts)
            elseIfBlocks <- mapM genElseIfBBlocks elseIfs
            elseBlocks <- case elseStmts of
                Just elseSts -> do
                    blocks <- genBBlocks [] (reverse elseSts)
                    return $ Just blocks
                Nothing -> return Nothing
            genBBlocks (IfElseBlock condition ifBlocks elseIfBlocks elseBlocks ann : acc) xs
        SAST.MatchStmt expr matchCases ann -> do
            matchCasesBlocks <- mapM genMatchCaseBBlocks matchCases
            genBBlocks (MatchBlock expr matchCasesBlocks ann : acc) xs

genBBBlockRet :: SAST.BlockRet SemanticAnn -> BBGenerator (BlockRet SemanticAnn)
genBBBlockRet (SAST.BlockRet stmts retExpr) = do
    blocks <- genBBlocks [] (reverse stmts)
    return $ BlockRet blocks retExpr
    

genBBClassMember :: SAST.ClassMember SemanticAnn -> BBGenerator (ClassMember SemanticAnn)
genBBClassMember (ClassField field ann) = return $ ClassField field ann
genBBClassMember (ClassMethod name retType body ann) = do
    bRet <- genBBBlockRet body
    return $ ClassMethod name retType bRet ann
genBBClassMember (ClassProcedure name args body ann) = do
    bRet <- genBBBlockRet body
    return $ ClassProcedure name args bRet ann
genBBClassMember (ClassViewer name args retType body ann) = do
    bRet <- genBBBlockRet body
    return $ ClassViewer name args retType bRet ann
genBBClassMember (ClassAction name param retType body ann) = do
    bRet <- genBBBlockRet body
    return $ ClassAction name param retType bRet ann

genBBTypeDef :: SAST.TypeDef SemanticAnn -> BBGenerator (TypeDef SemanticAnn)
genBBTypeDef (SAST.Struct name fields ann) = return $ Struct name fields ann
genBBTypeDef (SAST.Enum name variants ann) = return $ Enum name variants ann
genBBTypeDef (SAST.Class kind name members parents ann) = do
    bbMembers <- mapM genBBClassMember members
    return $ Class kind name bbMembers parents ann
genBBTypeDef (SAST.Interface name members ann) = return $ Interface name members ann


genBBAnnASTElement :: SAST.AnnASTElement SemanticAnn -> BBGenerator (AnnASTElement SemanticAnn)
genBBAnnASTElement (SAST.Function name args retType body modifiers ann) = do
    bRet <- genBBBlockRet body
    return $ Function name args retType bRet modifiers ann
genBBAnnASTElement (SAST.GlobalDeclaration global) = 
    return $ GlobalDeclaration global
genBBAnnASTElement (SAST.TypeDefinition typeDef ann) = do
    bbTypeDef <- genBBTypeDef typeDef
    return $ TypeDefinition bbTypeDef ann

genBBModule :: SAST.AnnotatedProgram SemanticAnn -> BBGenerator (AnnotatedProgram SemanticAnn)
genBBModule = mapM genBBAnnASTElement

runGenBBModule :: SAST.AnnotatedProgram SemanticAnn -> Either BBGeneratorError (AnnotatedProgram SemanticAnn)
runGenBBModule = runExcept . genBBModule