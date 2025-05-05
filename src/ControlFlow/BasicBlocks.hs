module ControlFlow.BasicBlocks (
    genBBBlock,
    genBBlocks,
    genBBAnnASTElement,
    genBBTypeDef,
    genBBModule,
    runGenBBModule
) where

import qualified Semantic.AST as SAST
import Core.AST
import ControlFlow.BasicBlocks.AST
import ControlFlow.BasicBlocks.Types
import Semantic.Types
import Control.Monad.Except
import ControlFlow.BasicBlocks.Utils
import ControlFlow.BasicBlocks.Errors
import Control.Monad

-- | This function appends a statement to a regular block.  If the statement is
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
    -> BBGenerator [BasicBlock SemanticAnn]
appendRegularBlock acc currBlock [] =
    -- | If there are no more statements, we shall return the current block
    -- appended to the accumulator
    return $ currBlock : acc
appendRegularBlock acc currBlock@(RegularBlock currStmts) (stmt : xs) =
    case stmt of
        SAST.SingleExpStmt expr ann ->
            case expr of
                SAST.MemberFunctionCall obj _ _ _ -> do
                    obj_ty <- getObjType obj
                    case obj_ty of
                        -- | If we are calling a function from a reference, it
                        -- means that we are calling an inner function of the
                        -- self object, since the language does not allow us to
                        -- create references from ports. Thus, we shall create a
                        -- new regular block
                        TGlobal _ _ ->
                            appendRegularBlock acc (RegularBlock (SingleExpStmt expr ann : currStmts)) xs
                        -- | If the object is of a defined type, it means that
                        -- we are calling an inner function of the self object,
                        -- so we shall create a new regular block
                        TReference {} ->
                            appendRegularBlock acc (RegularBlock (SingleExpStmt expr ann : currStmts)) xs
                        -- | In any other case, we shall end the current regular
                        -- block and create a new one 
                        _ -> genBBlocks (currBlock : acc) (stmt : xs)
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
        _ -> genBBlocks (currBlock : acc) (stmt : xs)
appendRegularBlock _ currBlock _ = throwError $ InternalError ("appendRegularBlock: unexpected block type " ++ show currBlock)

genBBlocks ::
    [BasicBlock SemanticAnn] -- ^ Accumulator of blocks
    -> [SAST.Statement SemanticAnn] -- ^ Remaining statements
    -> BBGenerator [BasicBlock SemanticAnn]
genBBlocks acc [] =
    -- | If there are no more statements, we shall return the accumulator
    return acc
genBBlocks acc (stmt : xs) =
    case stmt of
        -- | Procedure calls are always a single statement, since they do not
        -- return any value.  For those cases, we shall create a new single
        -- block that will depend on the type of the object and the procedure or
        -- operation that is being called
        SAST.SingleExpStmt expr ann ->
            case expr of
                SAST.MemberFunctionCall obj funcName args ann' -> do
                    obj_ty <- getObjType obj
                    case obj_ty of
                        -- | If the object is of a defined type, it means that
                        -- we are calling an inner function of the self object,
                        -- so we shall create a new regular block
                        TGlobal _ _ ->
                            appendRegularBlock acc (RegularBlock [SingleExpStmt expr ann]) xs
                        -- | If we are calling a function from a reference, it
                        -- means that we are calling an inner function of the
                        -- self object, since the language does not allow us to
                        -- create references from ports. Thus, we shall create a
                        -- new regular block
                        TReference {} ->
                            appendRegularBlock acc (RegularBlock [SingleExpStmt expr ann]) xs
                        -- | If the object is an access port of a user-defined interface type, we shall create
                        -- a new procedure call block
                        TAccessPort (TInterface RegularInterface _) ->
                            genBBlocks (ProcedureCall obj funcName args ann' : acc) xs
                        TAccessPort (TInterface SystemInterface _) ->
                            genBBlocks (SystemCall obj funcName args ann' : acc) xs
                        -- | If the object is an access port to an allocator, we shall create a new block
                        -- of the corresponding type (AllocBox or FreeBox)
                        TAccessPort (TAllocator _) -> do
                            -- | We need to check the operation (alloc or free)
                            case funcName of
                                "alloc" -> case args of
                                    [opt] -> genBBlocks (AllocBox obj opt ann' : acc) xs
                                    _ -> throwError $ InternalError ("genBBlocks: unexpected number of arguments of procedure " ++ funcName)
                                "free" -> case args of
                                    [elemnt] -> genBBlocks (FreeBox obj elemnt ann' : acc) xs
                                    _ -> throwError $ InternalError ("genBBlocks: unexpected number of arguments of procedure " ++ funcName)
                                _ -> throwError $ InternalError ("genBBlocks: unexpected function name " ++ funcName)
                        -- | If the object is an access port to an atomic
                        -- object, we shall create a new block of the
                        -- corresponding type (AtomicLoad, AtomicStore,
                        -- AtomicArrayLoad or AtomicArrayStore)
                        TAccessPort (TAtomicAccess _) -> do
                            -- | We need to check the operation (load or store)
                            case funcName of
                                "load" -> case args of
                                    [retval] -> genBBlocks (AtomicLoad obj retval ann' : acc) xs
                                    _ -> throwError $ InternalError ("genBBlocks: unexpected number of arguments of procedure " ++ funcName)
                                "store" -> case args of
                                    [value] -> genBBlocks (AtomicStore obj value ann' : acc) xs
                                    _ -> throwError $ InternalError ("genBBlocks: unexpected number of arguments of procedure " ++ funcName)
                                _ -> throwError $ InternalError ("genBBlocks: unexpected function name " ++ funcName)
                        TAccessPort (TAtomicArrayAccess {}) -> do
                            -- | We need to check the operation (load_index or store_index)
                            case funcName of
                                "load_index" -> case args of
                                    [index, retval] -> genBBlocks (AtomicArrayLoad obj index retval ann' : acc) xs
                                    _ -> throwError $ InternalError ("genBBlocks: unexpected number of arguments of procedure " ++ funcName)
                                "store_index" -> case args of
                                    [index, value] -> genBBlocks (AtomicArrayStore obj index value ann' : acc) xs
                                    _ -> throwError $ InternalError ("genBBlocks: unexpected number of arguments of procedure " ++ funcName)
                                _ -> throwError $ InternalError ("genBBlocks: unexpected function name " ++ funcName)
                        -- | If the object is an output port, we shall create a
                        -- new block of the corresponding type (SendMessage)
                        TOutPort _ -> do
                            case funcName of
                                "send" -> case args of
                                    [msg] -> genBBlocks (SendMessage obj msg ann' : acc) xs
                                    _ -> throwError $ InternalError ("genBBlocks: unexpected number of arguments of procedure " ++ funcName)
                                _ -> throwError $ InternalError ("genBBlocks: unexpected function name " ++ funcName)
                        _ -> throwError $ InternalError ("genBBlocks: unexpected object type " ++ show obj_ty)
                -- | We must repeat the same process for dereference member
                -- function calls.  In this case, the object must be of a
                -- reference type and, since the language does not allow us to
                -- create references from ports, it can only be a reference to
                -- the self object
                SAST.DerefMemberFunctionCall obj _ _ _ -> do
                    obj_ty <- getObjType obj
                    case obj_ty of
                        TReference {} ->
                            appendRegularBlock acc (RegularBlock [SingleExpStmt expr ann]) xs
                        _ -> throwError $ InternalError ("genBBlocks: unexpected object type " ++ show obj_ty)
                _ -> appendRegularBlock acc (RegularBlock [SingleExpStmt expr ann]) xs
        SAST.Declaration name accessKind typeSpecifier expr ann -> appendRegularBlock acc (RegularBlock [Declaration name accessKind typeSpecifier expr ann]) xs
        SAST.AssignmentStmt obj expr ann -> appendRegularBlock acc (RegularBlock [AssignmentStmt obj expr ann]) xs
        SAST.ForLoopStmt iterator typeSpecifier initial final breakCondition (SAST.Block loopStmts blkann) ann -> do
            loopBlocks <- genBBlocks [] (reverse loopStmts)
            genBBlocks (ForLoopBlock iterator typeSpecifier initial final breakCondition (Block loopBlocks blkann) ann : acc) xs
        SAST.IfElseStmt condition ifBlk elseIfs elseStmts ann -> do
            ifBlocks <- genBBBlock ifBlk
            elseIfBlocks <- mapM genElseIfBBlocks elseIfs
            elseBlocks <- case elseStmts of
                Just elseBlk -> do
                    blocks <- genBBBlock elseBlk
                    return $ Just blocks
                Nothing -> return Nothing
            genBBlocks (IfElseBlock condition ifBlocks elseIfBlocks elseBlocks ann : acc) xs
        SAST.MatchStmt expr matchCases mDefaultCase ann -> do
            matchCasesBlocks <- mapM genMatchCaseBBlocks matchCases
            defaultCase <- maybe (return Nothing) (genDefaultBBlock >=> (return . Just)) mDefaultCase
            genBBlocks (MatchBlock expr matchCasesBlocks defaultCase ann : acc) xs
        SAST.ReturnStmt expr ann -> genBBlocks (ReturnBlock expr ann : acc) xs
        SAST.ContinueStmt expr ann -> genBBlocks (ContinueBlock expr ann : acc) xs
        SAST.RebootStmt ann -> genBBlocks (RebootBlock ann : acc) xs

    where

        -- | This function generates the basic blocks for an else-if block
        genElseIfBBlocks :: SAST.ElseIf SemanticAnn -> BBGenerator (ElseIf SemanticAnn)
        genElseIfBBlocks (SAST.ElseIf condition elifBlk ann) = do
            blocks <- genBBBlock elifBlk
            return $ ElseIf condition blocks ann

        -- | This function generates the basic blocks for a match case block
        genMatchCaseBBlocks :: SAST.MatchCase SemanticAnn -> BBGenerator (MatchCase SemanticAnn)
        genMatchCaseBBlocks (SAST.MatchCase identifier args caseBlk ann) = do
            blocks <- genBBBlock caseBlk
            return $ MatchCase identifier args blocks ann

        genDefaultBBlock :: SAST.DefaultCase SemanticAnn -> BBGenerator (DefaultCase SemanticAnn)
        genDefaultBBlock (SAST.DefaultCase caseBlk ann) = do
            blocks <- genBBBlock caseBlk
            return $ DefaultCase blocks ann

-- | This function generates the basic blocks for a return block. This is the
-- type of block that is the basis of a function and method body. It is
-- composed of a list of basic blocks and an expression that represents the
-- return value of the function or method.
genBBBlock :: SAST.Block SemanticAnn -> BBGenerator (Block SemanticAnn)
genBBBlock (SAST.Block stmts blkann) = do
    blocks <- genBBlocks [] (reverse stmts)
    return $ Block blocks blkann

-- | This function translates the class members from the semantic AST to the
-- basic block AST. If the member is a field, it will be translated as a field.
-- If the member is a method, procedure, viewer or action, it will be translated
-- as a method, procedure, viewer or action, respectively. In these cases, the
-- statements are grouped into basic blocks and a new return block is created.
genBBClassMember :: SAST.ClassMember SemanticAnn -> BBGenerator (ClassMember SemanticAnn)
genBBClassMember (ClassField field) = return $ ClassField field
genBBClassMember (ClassMethod name retType body ann) = do
    bRet <- genBBBlock body
    return $ ClassMethod name retType bRet ann
genBBClassMember (ClassProcedure name args body ann) = do
    bRet <- genBBBlock body
    return $ ClassProcedure name args bRet ann
genBBClassMember (ClassViewer name args retType body ann) = do
    bRet <- genBBBlock body
    return $ ClassViewer name args retType bRet ann
genBBClassMember (ClassAction name param retType body ann) = do
    bRet <- genBBBlock body
    return $ ClassAction name param retType bRet ann

-- | This function translates the type definitions from the semantic AST to the
-- basic block AST.
genBBTypeDef :: SAST.TypeDef SemanticAnn -> BBGenerator (TypeDef SemanticAnn)
genBBTypeDef (SAST.Struct name fields ann) = return $ Struct name fields ann
genBBTypeDef (SAST.Enum name variants ann) = return $ Enum name variants ann
genBBTypeDef (SAST.Class kind name members parents ann) = do
    bbMembers <- mapM genBBClassMember members
    return $ Class kind name bbMembers parents ann
genBBTypeDef (SAST.Interface kind name extends members ann) = return $ Interface kind name extends members ann

-- | This function translates the annotated AST elements from the semantic AST
-- to the basic block AST.
genBBAnnASTElement :: SAST.AnnASTElement SemanticAnn -> BBGenerator (AnnASTElement SemanticAnn)
genBBAnnASTElement (SAST.Function name args retType body modifiers ann) = do
    bRet <- genBBBlock body
    return $ Function name args retType bRet modifiers ann
genBBAnnASTElement (SAST.GlobalDeclaration global) =
    return $ GlobalDeclaration global
genBBAnnASTElement (SAST.TypeDefinition typeDef ann) = do
    bbTypeDef <- genBBTypeDef typeDef
    return $ TypeDefinition bbTypeDef ann

-- | This function translates the annotated module from the semantic AST to the
-- basic block AST. 
genBBModule :: SAST.AnnotatedProgram SemanticAnn -> BBGenerator (AnnotatedProgram SemanticAnn)
genBBModule = mapM genBBAnnASTElement

-- | This function runs the basic block generator on an annotated program
runGenBBModule :: SAST.AnnotatedProgram SemanticAnn -> Either BBGeneratorError (AnnotatedProgram SemanticAnn)
runGenBBModule = runExcept . genBBModule