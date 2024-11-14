{-# LANGUAGE FlexibleInstances #-}

module Generator.LanguageC.Printer where

import Generator.LanguageC.AST
import Prettyprinter
import Data.Text (Text)
import Control.Monad.Reader
import Utils.Annotations
import Text.Parsec.Pos

import Generator.Utils

data CPrinterConfig = CPrinterConfig
  { printDebugLines :: Bool,
    printAnnotations :: Bool
  }

newtype CPrinterError = CPrinterError String

type CPrinter = Reader CPrinterConfig DocStyle

-- precedence of C operators
binPrec :: CBinaryOp -> Integer
binPrec COpMul = 20
binPrec COpDiv = 20
binPrec COpMod = 20
binPrec COpAdd = 19
binPrec COpSub = 19
binPrec COpShl = 18
binPrec COpShr = 18
binPrec COpLt  = 17
binPrec COpGt  = 17
binPrec COpLe = 17
binPrec COpGe = 17
binPrec COpEq  = 16
binPrec COpNe = 16
binPrec COpAnd = 15
binPrec COpXor = 14
binPrec COpOr  = 13

class CPrint a where
    pprint :: a -> CPrinter
    pprintPrec :: Integer -> a -> CPrinter

    pprint = pprintPrec 0
    pprintPrec _ = pprint

instance CPrint CQualifier where
    pprint (CQualifier True False False) = 
        return $ pretty "volatile"
    pprint (CQualifier False True False) =
        return $ pretty "const"
    pprint (CQualifier False False True) =
        return $ pretty "_Atomic"
    pprint (CQualifier True True False) =
        return $ pretty "const" <+> pretty "volatile"
    pprint qual =
        error $ "Invalid qualifier: " ++ show qual

parenPrec :: Integer -> Integer -> DocStyle -> DocStyle
parenPrec prec prec2 t = if prec <= prec2 then t else parens t

pprintCTInt :: CIntSize -> CSignedness -> CPrinter
pprintCTInt size signedness = do
    let ssize = case size of
            IntSize8 -> pretty "8"
            IntSize16 -> pretty "16"
            IntSize32 -> pretty "32"
            IntSize64 -> pretty "64"
            IntSize128 -> pretty "128"
        ssignedness = case signedness of
            Signed -> emptyDoc
            Unsigned -> pretty "u"
    return $ ssignedness <> pretty "int" <> ssize <> pretty "_t"

instance CPrint CConstant where
    pprint (CIntConst i) = return $ pretty i
    pprint (CCharConst c) = return $ pretty c
    pprint (CStrConst s) = return $ pretty s

rootCType :: CType -> CType
rootCType ty =
    case ty of
        CTArray ty' _ -> rootCType ty'
        _ -> ty

pprintCTArray :: CType -> CPrinter
pprintCTArray arrayTy  =
    case arrayTy of
        CTArray ty size -> do
            pty <- pprintCTArray ty
            psize <- pprint size
            return $ brackets psize <> pty
        _ -> return emptyDoc

instance CPrint CType where
    pprint (CTVoid (CQualifier False False False)) = return $ pretty "void"
    pprint (CTVoid qual) = do
        pqual <- pprint qual
        return $ pqual <+> pretty "void"
    pprint (CTChar (CQualifier False False False)) = return $ pretty "char"
    pprint (CTChar qual) = do
        pqual <- pprint qual
        return $ pqual <+> pretty "char"
    pprint (CTInt size signedness (CQualifier False False False)) = pprintCTInt size signedness
    pprint (CTInt size signedness qual) = do
        pqual <- pprint qual
        ptype <- pprintCTInt size signedness
        return $ pqual <+> ptype
    pprint (CTPointer ty (CQualifier False False False)) = do
        ptype <- pprint ty
        return $ ptype <+> pretty "*"
    pprint (CTPointer ty qual) = do
        ptype <- pprint ty
        pqual <- pprint qual
        return $ ptype <+> pretty "*" <+> pqual
    pprint (CTArray ty _size) = 
        case ty of
            CTArray {} -> do
                pRootTy <- pprint (rootCType ty)
                pArraySizes <- pprintCTArray ty
                return $ pRootTy <+> parens (pretty "*") <> pArraySizes
            _ -> do
                pty <- pprint ty
                return $ pty <+> pretty "*"
    pprint (CTStruct tag ident (CQualifier False False False)) = do
        return $ pretty tag <+> pretty ident
    pprint (CTStruct tag ident qual) = do
        pqual <- pprint qual
        return $ pqual <+> pretty tag <+> pretty ident
    pprint (CTEnum ident (CQualifier False False False)) = return $ pretty "enum" <+> pretty ident
    pprint (CTEnum ident qual) = do
        pqual <- pprint qual
        return $ pqual <+> pretty "enum" <+> pretty ident
    pprint (CTSizeT (CQualifier False False False)) = return $ pretty "size_t"
    pprint (CTSizeT qual) = do
        pqual <- pprint qual
        return $ pqual <+> pretty "size_t"
    pprint (CTBool (CQualifier False False False)) = return $ pretty "_Bool"
    pprint (CTBool qual) = do
        pqual <- pprint qual
        return $ pqual <+> pretty "_Bool"
    pprint (CTTypeDef ident (CQualifier False False False)) = return $ pretty ident
    pprint (CTTypeDef ident qual) = do
        pqual <- pprint qual
        return $ pqual <+> pretty ident
    pprint ty@(CTFunction {}) = error $ "Printing function types is not supported: " ++ show ty

instance CPrint CObject where
    pprintPrec _ (CVar ident _) = return $ pretty ident
    pprintPrec p (CField expr ident _) = do
        pexpr <- pprintPrec 26 expr
        case getCObjType expr of
            CTPointer _ _ -> return $ parenPrec p 26 $ pexpr <> pretty "->" <> pretty ident
            _ -> return $ parenPrec p 26 $ pexpr <> pretty "." <> pretty ident
    pprintPrec p (CDeref expr _) = do
        pexpr <- pprintPrec 25 expr
        return $ parenPrec p 25 $ pretty "*" <> pexpr
    pprintPrec p (CIndexOf obj index _) = do
        pobj <- pprintPrec 26 obj
        pindex <- pprint index
        return $ parenPrec p 26 $ pobj <> brackets pindex
    pprintPrec p (CObjCast obj ty) = do
        pexpr <- pprintPrec 25 obj
        ptype <- pprint ty
        return $ parenPrec p 25 $ parens ptype <> pexpr

instance CPrint CExpression where
    pprintPrec _ (CExprConstant c _ _) = pprint c
    pprintPrec p (CExprValOf obj _ _) = pprintPrec p obj
    pprintPrec p (CExprAddrOf obj _ _) = do
        pobj <- pprintPrec 25 obj
        return $ parenPrec p 25 $ pretty "&" <> pobj
    pprintPrec p (CExprUnaryOp op expr _ _) = do
        pexpr <- pprintPrec 25 expr
        return $ parenPrec p 25 $ pretty op <> pexpr
    pprintPrec p (CExprBinaryOp op expr1 expr2 _ _) = do
        let prec = binPrec op
        pexpr1 <- pprintPrec prec expr1
        pexpr2 <- pprintPrec (prec + 1) expr2
        return $ parenPrec p prec $ pexpr1 <+> pretty op <+> pexpr2
    pprintPrec p (CExprCast expr ty _) = do
        pexpr <- pprintPrec 25 expr
        ptype <- pprint ty
        return $ parenPrec p 25 $ parens ptype <> pexpr
    pprintPrec p (CExprSeqAnd expr1 expr2 _ _) = do
        pexpr1 <- pprintPrec 12 expr1
        pexpr2 <- pprintPrec 13 expr2
        return $ parenPrec p 12 $ pexpr1 <+> pretty "&&" <+> pexpr2
    pprintPrec p (CExprSeqOr expr1 expr2 _ _) = do
        pexpr1 <- pprintPrec 11 expr1
        pexpr2 <- pprintPrec 12 expr2
        return $ parenPrec p 11 $ pexpr1 <+> pretty "||" <+> pexpr2
    pprintPrec p (CExprSizeOfType decl _ _) = do
        pdecl <- pprint decl
        return $ parenPrec p 25 $ pretty "sizeof" <> parens pdecl
    pprintPrec p (CExprSizeOfExpr expr _ _) = do
        pexpr <- pprintPrec 25 expr
        return $ parenPrec p 25 $ pretty "sizeof" <> parens pexpr
    pprintPrec p (CExprAlignOfType decl _ _) = do
        pdecl <- pprint decl
        return $ parenPrec p 25 $ pretty "_Alignof" <> parens pdecl
    pprintPrec p (CExprAssign obj expr _ _) = do
        pobj <- pprintPrec 2 obj
        pexpr <- pprintPrec 3 expr
        return $ parenPrec p 2 $ pobj <+> pretty "=" <+> pexpr
    pprintPrec p (CExprComma expr1 expr2 _ _) = do
        pexpr1 <- pprintPrec 2 expr1
        pexpr2 <- pprintPrec 2 expr2
        return $ parenPrec p (-1) $ pexpr1 <> comma <+> pexpr2
    pprintPrec p (CExprCall expr args _ _) = do
        pexpr <- pprintPrec 30 expr
        pargs <- mapM (pprintPrec 2) args
        return $ parenPrec p 30 $ pexpr <> parens (align (fillSep (punctuate comma pargs)))

prependLine :: Bool -> DocStyle -> DocStyle
prependLine True doc = line <> doc
prependLine False doc = doc

indentStmt :: Bool -> DocStyle -> DocStyle
indentStmt True doc = indentTab doc
indentStmt False doc = doc

addDebugLine :: Bool -> Location -> DocStyle -> DocStyle
addDebugLine True (Position startPos _) doc = vsep [
        pretty "#line" <+> pretty (sourceLine startPos) <+> pretty "\"" <> pretty (sourceName startPos) <> pretty "\"",
        doc
    ]
addDebugLine _ _ doc = doc

braces' :: DocStyle -> DocStyle
braces' b = braces (line <> b <> line)

indentTab :: DocStyle -> DocStyle
indentTab = indent 4

pprintCTypeDecl :: Ident -> CType -> CPrinter
pprintCTypeDecl ident ty = 
    case ty of
        CTArray {} -> do
            pRootTy <- pprint (rootCType ty)
            pArraySizes <- pprintCTArray ty
            return $ pRootTy <+> pretty ident <> pArraySizes
        CTPointer (CTFunction rTy params) (CQualifier False False False) -> do
            prTy <- pprint rTy
            pparams <- mapM pprint params
            return $ prTy <+> parens (pretty "*" <+> pretty ident) <> parens (align (fillSep (punctuate comma pparams)))
        CTPointer (CTFunction rTy params) qual -> do
            prTy <- pprint rTy
            pqual <- pprint qual
            pparams <- mapM pprint params
            return $ prTy <+> parens (pretty "*" <+> pqual <+> pretty ident) <> parens (align (fillSep (punctuate comma pparams)))
        _ -> do
            pty <- pprint ty
            return $ pty <+> pretty ident

pprintCAttributeList :: [CAttribute] -> CPrinter
pprintCAttributeList [] = return emptyDoc
pprintCAttributeList attrs = do
    pattrs <- mapM pprint attrs
    return $ pretty "__attribute__" <> parens (parens (hsep (punctuate comma pattrs)))

instance CPrint CAttribute where
    pprint (CAttr attrName []) = return $ pretty attrName
    pprint (CAttr attrName attrParams) = do
        pattrParams <- mapM (pprintPrec 25) attrParams
        return $ pretty attrName <> parens (hsep (punctuate comma pattrParams))

instance CPrint CStructureUnion where
    pprint (CStruct tag mid decls attrs) = do
        pdecls <- mapM pprint decls
        case mid of
            Nothing -> do
                case attrs of 
                    [] -> return $ vcat [
                        pretty tag <+> pretty "{",
                        indentTab $ vsep (map (<> semi) pdecls),
                        pretty "}"]
                    _ -> do
                        pattrs <- pprintCAttributeList attrs
                        return $ vcat [
                            pretty tag <+> pretty "{",
                            indentTab $ vsep (map (<> semi) pdecls),
                            pretty "}" <+> pattrs]
            Just ident -> do
                case attrs of 
                    [] -> return $ vcat [
                        pretty tag <+> pretty ident <+> pretty "{",
                        indentTab $ vsep (map (<> semi) pdecls),
                        pretty "}"]
                    _ -> do
                        pattrs <- pprintCAttributeList attrs
                        return $ vcat [
                            pretty tag <+> pretty ident <+> pretty "{",
                            indentTab $ vsep (map (<> semi) pdecls),
                            pretty "}" <+> pattrs] 

instance CPrint CEnum where

    pprint (CEnum mid decls attrs) = do
        pdecls <- mapM p decls
        case mid of 
            Nothing -> do
                case attrs of
                    [] -> return $ vcat [
                        pretty "enum" <+> pretty "{",
                        indentTab $ vsep (punctuate comma pdecls),
                        pretty "}"]
                    _ -> do
                        pattrs <- pprintCAttributeList attrs
                        return $ vcat [
                            pretty "enum" <+> pretty "{",
                            indentTab $ vsep (punctuate comma pdecls),
                            pretty "}" <+> pattrs]
            Just ident -> do
                case attrs of
                    [] -> return $ vcat [
                        pretty "enum" <+> pretty ident <+> pretty "{",
                        indentTab $ vsep (punctuate comma pdecls),
                        pretty "}"]
                    _ -> do
                        pattrs <- pprintCAttributeList attrs
                        return $ vcat [
                            pretty "enum" <+> pretty ident <+> pretty "{",
                            indentTab $ vsep (punctuate comma pdecls),
                            pretty "}" <+> pattrs]
        where
            p :: (Ident, Maybe CExpression) -> CPrinter
            p (ident', Just expr) = do
                pexpr <- pprintPrec 25 expr
                return $ pretty ident' <+> pretty "=" <+> pexpr
            p (ident', Nothing) = return $ pretty ident'


instance CPrint CDeclaration where
    pprint (CDecl (CTypeSpec ty) (Just ident) Nothing)  = pprintCTypeDecl ident ty
    pprint (CDecl (CTypeSpec ty) (Just ident) (Just expr)) = do
        debugLines <- asks printDebugLines
        pty <- pprint ty
        pexpr <- pprint expr
        return . addDebugLine debugLines (location . getAnnotation $ expr) $ pty <+> pretty ident <+> pretty "=" <+> pexpr
    pprint (CDecl (CTSStructUnion stu) Nothing Nothing) = pprint stu
    pprint (CDecl (CTSStructUnion stu) (Just ident) Nothing) = do
        pstruct <- pprint stu
        return $ pstruct <+> pretty ident
    pprint (CDecl (CTSEnum enum) Nothing Nothing) = pprint enum
    pprint (CDecl (CTSEnum enum) (Just ident) Nothing) = do
        penum <- pprint enum
        return $ penum <+> pretty ident
    pprint _ = error "Invalid declaration"

instance CPrint CStatement where
    pprint CSSkip = return emptyDoc
    pprint s@(CSCase expr stat ann) = do
        case itemAnnotation ann of
            CStatementAnn before expand -> do
                pexpr <- pprint expr
                pstat <- pprint stat
                return $ prependLine before $ indentStmt expand $ pretty "case" <+> pexpr <> pretty ":" <> line <> pstat
            _ -> error $ "Invalid annotation: " ++ show s
    pprint s@(CSDefault stat ann) = do
        case itemAnnotation ann of
            CStatementAnn before expand -> do
                pstat <- pprint stat
                return $ prependLine before $ indentStmt expand $ pretty "default:" <> line <> pstat
            _ -> error $ "Invalid annotation: " ++ show s
    pprint s@(CSDo expr ann) = do
        case itemAnnotation ann of
            CStatementAnn before expand -> do
                debugLines <- asks printDebugLines
                pexpr <- pprint expr
                return $ 
                    prependLine before . indentStmt expand . addDebugLine debugLines (location ann) 
                    $ pexpr <> semi
            _ -> error $ "Invalid annotation: " ++ show s
    pprint s@(CSIfThenElse expr stat mestat ann) = do
        case itemAnnotation ann of
            CStatementAnn before expand -> do
                debugLines <- asks printDebugLines
                pexpr <- pprint expr
                pstat <- pprint stat
                pestat <- case mestat of
                    Nothing -> return emptyDoc
                    Just estat -> do
                        palt <- pprint estat
                        if debugLines then
                            return $ pretty " else" <> line <> palt
                        else
                            return $ pretty " else" <+> palt
                return $ prependLine before . indentStmt expand . addDebugLine debugLines (location ann) $
                    pretty "if" <+> parens pexpr
                    <+> pstat
                    <> pestat
            _ -> error $ "Invalid annotation: " ++ show s
    pprint s@(CSSwitch expr stat ann) = do
        case itemAnnotation ann of
            CStatementAnn before expand -> do
                debugLines <- asks printDebugLines
                pexpr <- pprint expr
                pstat <- pprint stat
                return $ prependLine before . indentStmt expand . addDebugLine debugLines (location ann) $
                    pretty "switch" <+> parens pexpr
                    <+> pstat
            _ -> error $ "Invalid annotation: " ++ show s
    pprint s@(CSFor for_init cond step stat ann) = do
        case itemAnnotation ann of
            CStatementAnn before expand -> do
                debugLines <- asks printDebugLines
                pfor_init <- either (maybe (return emptyDoc) pprint) pprint for_init
                pstat <- pprint stat
                case (cond, step) of
                    (Nothing, Nothing) ->
                        return $ prependLine before . indentStmt expand . addDebugLine debugLines (location ann) $
                            pretty "for" <+> parens (pfor_init <> semi <> semi) <+> pstat
                    (Just cond', Nothing) -> do
                        pcond <- pprint cond'
                        return $ prependLine before . indentStmt expand . addDebugLine debugLines (location ann) $
                            pretty "for" <+> parens (pfor_init <> semi <+> pcond <> semi) <+> pstat
                    (Nothing, Just step') -> do
                        pstep <- pprint step'
                        return $ prependLine before . indentStmt expand . addDebugLine debugLines (location ann) $
                            pretty "for" <+> parens (pfor_init <> semi <> semi <+> pstep) <+> pstat
                    (Just cond', Just step') -> do
                        pcond <- pprint cond'
                        pstep <- pprint step'
                        return $ prependLine before . indentStmt expand . addDebugLine debugLines (location ann) $
                            pretty "for" <+> parens (pfor_init <> semi <+> pcond <> semi <+> pstep) <+> pstat
            _ -> error $ "Invalid annotation: " ++ show s
    pprint s@(CSReturn Nothing ann) =
        case itemAnnotation ann of
            CStatementAnn before expand -> do
                debugLines <- asks printDebugLines
                return $ prependLine before . indentStmt expand . addDebugLine debugLines (location ann) $
                    pretty "return" <> semi
            _ -> error $ "Invalid annotation: " ++ show s
    pprint s@(CSReturn (Just e) ann) =
        case itemAnnotation ann of
            CStatementAnn before expand -> do
                debugLines <- asks printDebugLines
                pe <- pprint e
                return $ prependLine before . indentStmt expand . addDebugLine debugLines (location ann) $
                    pretty "return" <+> pe <> semi
            _ -> error $ "Invalid annotation: " ++ show s
    pprint s@(CSCompound items ann) =
        case itemAnnotation ann of
            CCompoundAnn before trailing -> do
                pItems <- mapM pprint items
                return $ prependLine before $
                    if trailing then
                        braces' ((indentTab . align) (vsep pItems <> line))
                    else
                        braces' ((indentTab . align) (vsep pItems))
            _ -> error $ "Invalid annotation: " ++ show s
    pprint s@(CSBreak ann) =
        case itemAnnotation ann of
            CStatementAnn before expand -> do
                debugLines <- asks printDebugLines
                return $ 
                    prependLine before . indentStmt expand . addDebugLine debugLines (location ann) 
                    $ pretty "break" <> semi
            _ -> error $ "Invalid annotation: " ++ show s

instance CPrint CCompoundBlockItem where
    pprint (CBlockStmt stat) = pprint stat
    pprint (CBlockDecl decl ann) = do
        case itemAnnotation ann of 
            CDeclarationAnn before -> do
                debugLines <- asks printDebugLines
                pdecl <- pprint decl
                return $ prependLine before . addDebugLine debugLines (location ann) $ pdecl <> semi
            _ -> error $ "Invalid annotation: " ++ show ann

instance CPrint CPreprocessorDirective where
    pprint (CPPDefine ident Nothing) =
        return $ pretty "#define" <+> pretty ident
    pprint (CPPDefine ident (Just [])) =
        return $ pretty "#define" <+> pretty ident
    pprint (CPPDefine ident (Just [token])) =
        return $ pretty "#define" <+> pretty ident <+> pretty token
    pprint (CPPDefine ident (Just (t : ts))) =
        return $ vcat
            [
                pretty "#define" <+> pretty ident <+> pretty t <+> pretty "\\",
                indentTab $ vcat $ punctuate (pretty "\\") (map pretty ts)
            ]
    pprint (CPPIfDef ident) =
        return $ pretty "#ifdef" <+> pretty ident
    pprint (CPPInclude isSystem path) =
        return $ pretty "#include" <+>
            pretty (if isSystem then "<" else "\"") <>
                pretty path <> pretty
                (if isSystem then ">" else "\"")
    pprint (CPPIfNDef ident) =
        return $ pretty "#ifndef" <+> pretty ident
    pprint CPPEndif =
        return $ pretty "#endif"

instance CPrint CFunction where
    pprint (CFunction ty ident params body) = do
        pty <- pprint ty
        pparams <- mapM pprint params
        pbody <- pprint body
        return $ pty <+> pretty ident <> parens (align (fillSep (punctuate comma pparams))) <+> pbody

instance CPrint CExternalDeclaration where
    pprint (CEDVariable stspec decl) = do
        pdecl <- pprint decl
        return $ pretty stspec <+> pdecl <> semi
    pprint (CEDFunction ty ident params) = do
        pty <- pprint ty
        pparams <- mapM pprint params
        return $ pty <+> pretty ident <> parens (align (fillSep (punctuate comma pparams))) <> semi
    pprint (CEDEnum Nothing enum) = do
        penum <- pprint enum
        return $ penum <> semi
    pprint (CEDEnum (Just typeDefName) enum) = do
        penum <- pprint enum
        return $ pretty "typedef" <+> penum <+> pretty typeDefName <> semi
    pprint (CEDStructUnion Nothing stu) = do
        pstruct <- pprint stu
        return $ pstruct <> semi
    pprint (CEDStructUnion (Just typeDefName) stu) = do
        pstruct <- pprint stu
        return $ pretty "typedef" <+> pstruct <+> pretty typeDefName <> semi
    pprint (CEDTypeDef ident ty) = do
        pty <- pprint ty
        return $ pretty "typedef" <+> pty <+> pretty ident <> semi

instance CPrint CFileItem where
    pprint (CExtDecl decl ann) = 
        case itemAnnotation ann of 
            CDeclarationAnn before -> do
                pdecl <- pprint decl
                return $ prependLine before pdecl
            _ -> error $ "Invalid annotation: " ++ show ann
    pprint (CPPDirective directive ann) = 
        case itemAnnotation ann of 
            CPPDirectiveAnn before -> do
                pdir <- pprint directive
                return $ prependLine before pdir
            _ -> error $ "Invalid annotation: " ++ show ann
    pprint (CFunctionDef Nothing func ann) = 
        case itemAnnotation ann of 
            CDeclarationAnn before -> do
                pfunc <- pprint func
                return $ prependLine before pfunc
            _ -> error $ "Invalid annotation: " ++ show ann
    pprint (CFunctionDef (Just CStatic) func ann) = do
        case itemAnnotation ann of 
            CDeclarationAnn before -> do
                pfunc <- pprint func
                return $ prependLine before $ pretty "static" <+> pfunc
            _ -> error $ "Invalid annotation: " ++ show ann
    pprint (CFunctionDef {}) = error "Invalid function definition"

instance CPrint CFile where
    pprint (CHeaderFile _path items) = do
        pItems <- mapM pprint items
        return $ vsep pItems <> line
    pprint (CSourceFile _path items) = do
        pItems <- mapM pprint items
        return $ vsep pItems <> line

runCPrinter :: Bool -> CFile -> Text
runCPrinter debugLines cFile = render $ runReader (pprint cFile) (CPrinterConfig debugLines False)
