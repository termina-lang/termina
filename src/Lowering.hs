module Lowering (
    lowerBlock,
    lowerStatements,
    lowerElement,
    lowerTypeDef,
    lowerModule,
    runLowerModule
) where

import qualified Semantic.AST as SAST
import Core.AST
import Lowering.AST
import Lowering.Types
import Semantic.Types
import Control.Monad.Except
import Lowering.Utils
import Lowering.Errors

-- | This function appends a statement to a regular block.  If the statement is
-- a declaration or an assignment, or a regular single expression statement, it
-- will be appended to the current block. If the statement is a procedure call,
-- a message send, an atomic load or store, or an atomic array load or store, or
-- a control flow statement, the current block will be closed and a new one will
-- be created starting with the current statement.
-- 
-- The statement will be appended at the beginning of the block, since the order
-- of the statements is reversed upon calling this function.
appendRegularBlock ::
    [BasicBlock SemanticAnn] -- ^ Accumulator of blocks
    -> BasicBlock SemanticAnn -- ^ Current (regular) block
    -> [SAST.Statement SemanticAnn] -- ^ Remaining statements
    -> LoweringMonad [BasicBlock SemanticAnn]
appendRegularBlock acc currBlock [] =
    -- | If there are no more statements, we shall return the current block
    -- appended to the accumulator
    return $ currBlock : acc
appendRegularBlock acc currBlock@(RegularBlock currStmts) (stmt : xs) =
    case stmt of
        SAST.SingleExpStmt expr ann ->
            case expr of
                SAST.MemberFunctionCall obj _ _ _ -> do
                    obj_ty <- getObjType obj
                    case obj_ty of
                        -- | If we are calling a function from a reference, it
                        -- means that we are calling an inner function of the
                        -- self object, since the language does not allow us to
                        -- create references from ports. Thus, we shall create a
                        -- new regular block
                        TGlobal _ _ ->
                            appendRegularBlock acc (RegularBlock (SingleExpStmt expr ann : currStmts)) xs
                        -- | If the object is of a defined type, it means that
                        -- we are calling an inner function of the self object,
                        -- so we shall create a new regular block
                        TReference {} ->
                            appendRegularBlock acc (RegularBlock (SingleExpStmt expr ann : currStmts)) xs
                        -- | In any other case, we shall end the current regular
                        -- block and create a new one 
                        _ -> lowerStatements (currBlock : acc) (stmt : xs)
                SAST.DerefMemberFunctionCall obj _ _ _ -> do
                    obj_ty <- getObjType obj
                    case obj_ty of
                        TReference {} ->
                            appendRegularBlock acc (RegularBlock (SingleExpStmt expr ann : currStmts)) xs
                        _ -> throwError $ InternalError ("appendRegularBlock: unexpected object type " ++ show obj_ty)
                _ -> appendRegularBlock acc (RegularBlock (SingleExpStmt expr ann : currStmts)) xs
        SAST.Declaration name accessKind typeSpecifier expr ann ->
            appendRegularBlock acc (RegularBlock (Declaration name accessKind typeSpecifier expr ann : currStmts)) xs
        SAST.AssignmentStmt obj expr ann -> appendRegularBlock acc (RegularBlock (AssignmentStmt obj expr ann : currStmts)) xs
        _ -> lowerStatements (currBlock : acc) (stmt : xs)
appendRegularBlock _ currBlock _ = throwError $ InternalError ("appendRegularBlock: unexpected block type " ++ show currBlock)

lowerStatements ::
    [BasicBlock SemanticAnn] -- ^ Accumulator of blocks
    -> [SAST.Statement SemanticAnn] -- ^ Remaining statements
    -> LoweringMonad [BasicBlock SemanticAnn]
lowerStatements acc [] =
    -- | If there are no more statements, we shall return the accumulator
    return acc
lowerStatements acc (stmt : xs) =
    case stmt of
        -- | Procedure calls are always a single statement, since they do not
        -- return any value.  For those cases, we shall create a new single
        -- block that will depend on the type of the object and the procedure or
        -- operation that is being called
        SAST.SingleExpStmt expr ann ->
            case expr of
                SAST.MemberFunctionCall obj funcName args ann' -> do
                    obj_ty <- getObjType obj
                    case obj_ty of
                        -- | If the object is of a defined type, it means that
                        -- we are calling an inner function of the self object,
                        -- so we shall create a new regular block
                        TGlobal _ _ ->
                            appendRegularBlock acc (RegularBlock [SingleExpStmt expr ann]) xs
                        -- | If we are calling a function from a reference, it
                        -- means that we are calling an inner function of the
                        -- self object, since the language does not allow us to
                        -- create references from ports. Thus, we shall create a
                        -- new regular block
                        TReference {} ->
                            appendRegularBlock acc (RegularBlock [SingleExpStmt expr ann]) xs
                        -- | If the object is an access port of a user-defined interface type, we shall create
                        -- a new procedure call block
                        TAccessPort (TInterface RegularInterface _) ->
                            lowerStatements (ProcedureInvoke obj funcName args ann' : acc) xs
                        TAccessPort (TInterface SystemInterface _) ->
                            lowerStatements (SystemCall obj funcName args ann' : acc) xs
                        -- | If the object is an access port to an allocator, we shall create a new block
                        -- of the corresponding type (AllocBox or FreeBox)
                        TAccessPort (TAllocator _) -> do
                            -- | We need to check the operation (alloc or free)
                            case funcName of
                                "alloc" -> case args of
                                    [opt] -> lowerStatements (AllocBox obj opt ann' : acc) xs
                                    _ -> throwError $ InternalError ("lowerStatements: unexpected number of arguments of procedure " ++ funcName)
                                "free" -> case args of
                                    [elemnt] -> lowerStatements (FreeBox obj elemnt ann' : acc) xs
                                    _ -> throwError $ InternalError ("lowerStatements: unexpected number of arguments of procedure " ++ funcName)
                                _ -> throwError $ InternalError ("lowerStatements: unexpected function name " ++ funcName)
                        -- | If the object is an access port to an atomic
                        -- object, we shall create a new block of the
                        -- corresponding type (AtomicLoad, AtomicStore,
                        -- AtomicArrayLoad or AtomicArrayStore)
                        TAccessPort (TAtomicAccess _) -> do
                            -- | We need to check the operation (load or store)
                            case funcName of
                                "load" -> case args of
                                    [retval] -> lowerStatements (AtomicLoad obj retval ann' : acc) xs
                                    _ -> throwError $ InternalError ("lowerStatements: unexpected number of arguments of procedure " ++ funcName)
                                "store" -> case args of
                                    [value] -> lowerStatements (AtomicStore obj value ann' : acc) xs
                                    _ -> throwError $ InternalError ("lowerStatements: unexpected number of arguments of procedure " ++ funcName)
                                _ -> throwError $ InternalError ("lowerStatements: unexpected function name " ++ funcName)
                        TAccessPort (TAtomicArrayAccess {}) -> do
                            -- | We need to check the operation (load_index or store_index)
                            case funcName of
                                "load_index" -> case args of
                                    [index, retval] -> lowerStatements (AtomicArrayLoad obj index retval ann' : acc) xs
                                    _ -> throwError $ InternalError ("lowerStatements: unexpected number of arguments of procedure " ++ funcName)
                                "store_index" -> case args of
                                    [index, value] -> lowerStatements (AtomicArrayStore obj index value ann' : acc) xs
                                    _ -> throwError $ InternalError ("lowerStatements: unexpected number of arguments of procedure " ++ funcName)
                                _ -> throwError $ InternalError ("lowerStatements: unexpected function name " ++ funcName)
                        -- | If the object is an output port, we shall create a
                        -- new block of the corresponding type (SendMessage)
                        TOutPort _ -> do
                            case funcName of
                                "send" -> case args of
                                    [msg] -> lowerStatements (SendMessage obj msg ann' : acc) xs
                                    _ -> throwError $ InternalError ("lowerStatements: unexpected number of arguments of procedure " ++ funcName)
                                _ -> throwError $ InternalError ("lowerStatements: unexpected function name " ++ funcName)
                        _ -> throwError $ InternalError ("lowerStatements: unexpected object type " ++ show obj_ty)
                -- | We must repeat the same process for dereference member
                -- function calls.  In this case, the object must be of a
                -- reference type and, since the language does not allow us to
                -- create references from ports, it can only be a reference to
                -- the self object
                SAST.DerefMemberFunctionCall obj _ _ _ -> do
                    obj_ty <- getObjType obj
                    case obj_ty of
                        TReference {} ->
                            appendRegularBlock acc (RegularBlock [SingleExpStmt expr ann]) xs
                        _ -> throwError $ InternalError ("lowerStatements: unexpected object type " ++ show obj_ty)
                _ -> appendRegularBlock acc (RegularBlock [SingleExpStmt expr ann]) xs
        SAST.Declaration name accessKind typeSpecifier expr ann -> appendRegularBlock acc (RegularBlock [Declaration name accessKind typeSpecifier expr ann]) xs
        SAST.AssignmentStmt obj expr ann -> appendRegularBlock acc (RegularBlock [AssignmentStmt obj expr ann]) xs
        SAST.ForLoopStmt iterator typeSpecifier initial final breakCondition (SAST.Block loopStmts blkann) ann -> do
            loopBlocks <- lowerStatements [] (reverse loopStmts)
            lowerStatements (ForLoopBlock iterator typeSpecifier initial final breakCondition (Block loopBlocks blkann) ann : acc) xs
        SAST.IfElseStmt ifCond elseIfs mElse ann -> do
            ifBlocks <- genIfCondBlocks ifCond
            elseIfsBlocks <- mapM genElseIfBBlocks elseIfs
            elseBlocks <- case mElse of
                Just elseBlk -> do
                    blocks <- genElseBlocks elseBlk
                    return $ Just blocks
                Nothing -> return Nothing
            lowerStatements (IfElseBlock ifBlocks elseIfsBlocks elseBlocks ann : acc) xs
        SAST.MatchStmt expr matchCases mDefaultCase ann -> do
            matchCasesBlocks <- mapM genMatchCaseBBlocks matchCases
            defaultCase <- mapM genDefaultBBlock mDefaultCase
            lowerStatements (MatchBlock expr matchCasesBlocks defaultCase ann : acc) xs
        SAST.ReturnStmt expr ann -> lowerStatements (ReturnBlock expr ann : acc) xs
        SAST.ContinueStmt expr ann -> lowerStatements (ContinueBlock expr ann : acc) xs
        SAST.RebootStmt ann -> lowerStatements (RebootBlock ann : acc) xs

    where

        genIfCondBlocks :: SAST.CondIf SemanticAnn -> LoweringMonad (CondIf SemanticAnn)
        genIfCondBlocks (SAST.CondIf condition ifBlk ann) = do
            blocks <- lowerBlock ifBlk
            return $ CondIf condition blocks ann

        -- | This function generates the basic blocks for an else-if block
        genElseIfBBlocks :: SAST.CondElseIf SemanticAnn -> LoweringMonad (CondElseIf SemanticAnn)
        genElseIfBBlocks (SAST.CondElseIf condition elifBlk ann) = do
            blocks <- lowerBlock elifBlk
            return $ CondElseIf condition blocks ann
        
        genElseBlocks :: SAST.CondElse SemanticAnn -> LoweringMonad (CondElse SemanticAnn)
        genElseBlocks (SAST.CondElse elseBlk ann) = do
            blocks <- lowerBlock elseBlk
            return $ CondElse blocks ann

        -- | This function generates the basic blocks for a match case block
        genMatchCaseBBlocks :: SAST.MatchCase SemanticAnn -> LoweringMonad (MatchCase SemanticAnn)
        genMatchCaseBBlocks (SAST.MatchCase identifier args caseBlk ann) = do
            blocks <- lowerBlock caseBlk
            return $ MatchCase identifier args blocks ann

        genDefaultBBlock :: SAST.DefaultCase SemanticAnn -> LoweringMonad (DefaultCase SemanticAnn)
        genDefaultBBlock (SAST.DefaultCase caseBlk ann) = do
            blocks <- lowerBlock caseBlk
            return $ DefaultCase blocks ann

-- | This function generates the basic blocks for a return block. This is the
-- type of block that is the basis of a function and method body. It is
-- composed of a list of basic blocks and an expression that represents the
-- return value of the function or method.
lowerBlock :: SAST.Block SemanticAnn -> LoweringMonad (Block SemanticAnn)
lowerBlock (SAST.Block stmts blkann) = do
    blocks <- lowerStatements [] (reverse stmts)
    return $ Block blocks blkann

-- | This function translates the class members from the semantic AST to the
-- basic block AST. If the member is a field, it will be translated as a field.
-- If the member is a method, procedure, viewer or action, it will be translated
-- as a method, procedure, viewer or action, respectively. In these cases, the
-- statements are grouped into basic blocks and a new return block is created.
lowerClassMember :: SAST.ClassMember SemanticAnn -> LoweringMonad (ClassMember SemanticAnn)
lowerClassMember (ClassField field) = return $ ClassField field
lowerClassMember (ClassMethod ak name args retType body ann) = do
    bRet <- lowerBlock body
    return $ ClassMethod ak name args retType bRet ann
lowerClassMember (ClassProcedure ak name args body ann) = do
    bRet <- lowerBlock body
    return $ ClassProcedure ak name args bRet ann
lowerClassMember (ClassViewer name args retType body ann) = do
    bRet <- lowerBlock body
    return $ ClassViewer name args retType bRet ann
lowerClassMember (ClassAction ak name param retType body ann) = do
    bRet <- lowerBlock body
    return $ ClassAction ak name param retType bRet ann

-- | This function translates the type definitions from the semantic AST to the
-- basic block AST.
lowerTypeDef :: SAST.TypeDef SemanticAnn -> LoweringMonad (TypeDef SemanticAnn)
lowerTypeDef (SAST.Struct name fields ann) = return $ Struct name fields ann
lowerTypeDef (SAST.Enum name variants ann) = return $ Enum name variants ann
lowerTypeDef (SAST.Class kind name members parents ann) = do
    bbMembers <- mapM lowerClassMember members
    return $ Class kind name bbMembers parents ann
lowerTypeDef (SAST.Interface kind name extends members ann) = return $ Interface kind name extends members ann

-- | This function translates the annotated AST elements from the semantic AST
-- to the basic block AST.
lowerElement :: SAST.AnnASTElement SemanticAnn -> LoweringMonad (AnnASTElement SemanticAnn)
lowerElement (SAST.Function name args retType body modifiers ann) = do
    bRet <- lowerBlock body
    return $ Function name args retType bRet modifiers ann
lowerElement (SAST.GlobalDeclaration global) =
    return $ GlobalDeclaration global
lowerElement (SAST.TypeDefinition typeDef ann) = do
    bbTypeDef <- lowerTypeDef typeDef
    return $ TypeDefinition bbTypeDef ann

-- | This function translates the annotated module from the semantic AST to the
-- basic block AST. 
lowerModule :: SAST.AnnotatedProgram SemanticAnn -> LoweringMonad (AnnotatedProgram SemanticAnn)
lowerModule = mapM lowerElement

-- | This function runs the basic block generator on an annotated program
runLowerModule :: SAST.AnnotatedProgram SemanticAnn -> Either LoweringError (AnnotatedProgram SemanticAnn)
runLowerModule = runExcept . lowerModule