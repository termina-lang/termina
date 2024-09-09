{-# LANGUAGE FlexibleInstances #-}

module Generator.LanguageC.CompCertCPrinter where

import Generator.LanguageC.CompCertC
import Prettyprinter
import Data.Text (Text)
import Prettyprinter.Render.Terminal
import Control.Monad.Reader

type DocStyle = Doc AnsiStyle

render :: DocStyle -> Text
render = renderStrict . layoutSmart defaultLayoutOptions

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

class PPrint a where
    pprint :: a -> CPrinter
    pprintPrec :: Integer -> a -> CPrinter

    pprint = pprintPrec 0
    pprintPrec _ = pprint

instance PPrint CQualifier where
    pprint (CQualifier c v a) = 
        let cv = if c then (pretty "const" <+>) else (emptyDoc <>)
            va = if v then (pretty "volatile" <+>) else (emptyDoc <>)
            at = if a then (pretty "_Atomic" <+>) else (emptyDoc <>) in
        return (cv . va . at $ emptyDoc)

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

instance PPrint CArraySize where
    pprint (CArraySizeK c) = return $ pretty c
    pprint (CArraySizeV v) = return $ pretty v

instance PPrint CConstant where
    pprint (CIntConst i) = return $ pretty i
    pprint (CCharConst c) = return $ pretty c
    pprint (CStrConst s) = return $ pretty s

pprintCTArray :: CType -> CPrinter
pprintCTArray arrayTy  =
    case arrayTy of
        CTArray ty size -> do
            pty <- pprintCTArray ty
            psize <- pprint size
            return $ pty <> brackets psize
        _ -> do
            pty <- pprint arrayTy
            return $ pty <+> parens (pretty "*")


instance PPrint CType where
    pprint CTVoid = return $ pretty "void"
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
                pprintCTArray ty
            _ -> do
                pty <- pprint ty
                return $ pty <+> pretty "*"
    pprint (CTStruct tag ident (CQualifier False False False)) = do
        return $ pretty tag <+> pretty ident
    pprint (CTStruct tag ident qual) = do
        pqual <- pprint qual
        return $ pretty tag <+> pretty ident <+> pqual
    pprint (CTEnum ident (CQualifier False False False)) = return $ pretty "enum" <+> pretty ident
    pprint (CTEnum ident qual) = do
        pqual <- pprint qual
        return $ pretty "enum" <+> pretty ident <+> pqual
    pprint (CTSizeT (CQualifier False False False)) = return $ pretty "size_t"
    pprint (CTSizeT qual) = do
        pqual <- pprint qual
        return $ pretty "size_t" <+> pqual
    pprint (CTBool (CQualifier False False False)) = return $ pretty "_Bool"
    pprint (CTBool qual) = do
        pqual <- pprint qual
        return $ pretty "_Bool" <+> pqual
    pprint (CTTypeDef ident (CQualifier False False False)) = return $ pretty ident
    pprint (CTTypeDef ident qual) = do
        pqual <- pprint qual
        return $ pretty ident <+> pqual
    pprint (CTFunction {}) = error "Priting function types are not supported"

instance PPrint CObject where
    pprintPrec _ (CVar ident _) = return $ pretty ident
    pprintPrec p (CField expr ident _) = do
        pexpr <- pprintPrec 26 expr
        case getCExprType expr of
            CTPointer _ _ -> return $ parenPrec p 26 $ pexpr <> pretty "->" <> pretty ident
            _ -> return $ parenPrec p 26 $ pexpr <> pretty "." <> pretty ident
    pprintPrec p (CDeref expr _) = do
        pexpr <- pprintPrec 25 expr
        return $ parenPrec p 25 $ pretty "*" <> pexpr
    pprintPrec p (CIndexOf obj index _) = do
        pobj <- pprintPrec 26 obj
        pindex <- pprintPrec 26 index
        return $ parenPrec p 26 $ pobj <> brackets pindex

instance PPrint CExpression where
    pprintPrec _ (CExprConstant c _ _) = pprint c
    pprintPrec _ (CExprValOf obj _ _) = pprint obj
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
    pprintPrec p (CExprCast decl expr _) = do
        pdecl <- pprint decl
        pexpr <- pprintPrec 25 expr
        return $ parenPrec p 25 $ parens pdecl <> pexpr
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

braces' :: DocStyle -> DocStyle
braces' b = braces (line <> b <> line)

indentTab :: DocStyle -> DocStyle
indentTab = indent 4

pprintCTypeDecl :: Ident -> CType -> CPrinter
pprintCTypeDecl ident ty = 
    case ty of
        CTArray ty' size -> do
            pty <- pprintCTypeDecl ident ty'
            psize <- pprint size
            return $ pty <+> pretty ident <> brackets psize
        _ -> do
            pty <- pprint ty
            return $ pty <+> pretty ident

pprintCAttributeList :: [CAttribute] -> CPrinter
pprintCAttributeList [] = return emptyDoc
pprintCAttributeList attrs = do
    pattrs <- mapM pprint attrs
    return $ pretty "__attribute__" <> parens (parens (hsep (punctuate comma pattrs)))

instance PPrint CAttribute where
    pprint (CAttr attrName []) = return $ pretty attrName
    pprint (CAttr attrName attrParams) = do
        pattrParams <- mapM (pprintPrec 25) attrParams
        return $ pretty attrName <> parens (hsep (punctuate comma pattrParams))

instance PPrint CStructureUnion where
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

instance PPrint CEnum where

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


instance PPrint CDeclaration where
    pprint (CDecl (CTypeSpec ty) ident Nothing _) = pprintCTypeDecl ident ty
    pprint (CDecl (CTypeSpec ty) ident (Just expr) _) = do
        pty <- pprint ty
        pexpr <- pprint expr
        return $ pty <+> pretty ident <+> pretty "=" <+> pexpr
    pprint (CDecl (CTSStructUnion stu) ident Nothing _) = do
        pstruct <- pprint stu
        return $ pstruct <+> pretty ident
    pprint (CDecl (CTSEnum enum) ident Nothing _) = do
        penum <- pprint enum
        return $ penum <+> pretty ident
    pprint _ = error "Invalid declaration"

instance PPrint CStatement where
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
                pexpr <- pprint expr
                return $ prependLine before $ indentStmt expand $ pexpr <> semi
            _ -> error $ "Invalid annotation: " ++ show s
    pprint s@(CSIfThenElse expr stat estat ann) = do
        case itemAnnotation ann of
            CStatementAnn before expand -> do
                pexpr <- pprint expr
                pstat <- pprint stat
                pestat <- case estat of
                    CSSkip -> return emptyDoc
                    _ -> do
                        palt <- pprint estat
                        return $ pretty " else" <+> palt
                return $ prependLine before $ indentStmt expand $
                    pretty "if" <+> parens pexpr
                    <+> pstat
                    <> pestat
            _ -> error $ "Invalid annotation: " ++ show s
    pprint s@(CSSwitch expr stat ann) = do
        case itemAnnotation ann of
            CStatementAnn before expand -> do
                pexpr <- pprint expr
                pstat <- pprint stat
                return $ prependLine before $ indentStmt expand $
                    pretty "switch" <+> parens pexpr
                    <+> pstat
            _ -> error $ "Invalid annotation: " ++ show s
    pprint s@(CSFor for_init cond step stat ann) = do
        case itemAnnotation ann of
            CStatementAnn before expand -> do
                pfor_init <- either (maybe (return emptyDoc) pprint) pprint for_init
                pstat <- pprint stat
                case (cond, step) of
                    (Nothing, Nothing) ->
                        return $ prependLine before $ indentStmt expand $
                            pretty "for" <+> parens (pfor_init <> semi <> semi) <+> pstat
                    (Just cond', Nothing) -> do
                        pcond <- pprint cond'
                        return $ prependLine before $ indentStmt expand $
                            pretty "for" <+> parens (pfor_init <> semi <+> pcond <> semi) <+> pstat
                    (Nothing, Just step') -> do
                        pstep <- pprint step'
                        return $ prependLine before $ indentStmt expand $
                            pretty "for" <+> parens (pfor_init <> semi <> semi <+> pstep) <+> pstat
                    (Just cond', Just step') -> do
                        pcond <- pprint cond'
                        pstep <- pprint step'
                        return $ prependLine before $ indentStmt expand $
                            pretty "for" <+> parens (pfor_init <> semi <+> pcond <> semi <+> pstep) <+> pstat
            _ -> error $ "Invalid annotation: " ++ show s
    pprint s@(CSReturn Nothing ann) =
        case itemAnnotation ann of
            CStatementAnn before expand -> do
                return $ prependLine before $ indentStmt expand $
                    pretty "return" <> semi
            _ -> error $ "Invalid annotation: " ++ show s
    pprint s@(CSReturn (Just e) ann) =
        case itemAnnotation ann of
            CStatementAnn before expand -> do
                pe <- pprint e
                return $ prependLine before $ indentStmt expand $
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

instance PPrint CCompoundBlockItem where
    pprint (CBlockStmt stat) = pprint stat
    pprint (CBlockDecl decl) = do
        pdecl <- pprint decl
        return $ pdecl <> semi


instance PPrint CPreprocessorDirective where
    pprint (CPPDefine ident Nothing ann) =
        case itemAnnotation ann of
            CPPDirectiveAnn before ->
                return $ prependLine before $ pretty "#define" <+> pretty ident
            _ -> error $ "Invalid annotation: " ++ show ann
    pprint (CPPDefine ident (Just []) ann) =
        case itemAnnotation ann of
            CPPDirectiveAnn before ->
                return $ prependLine before $ pretty "#define" <+> pretty ident
            _ -> error $ "Invalid annotation: " ++ show ann
    pprint (CPPDefine ident (Just [token]) ann) =
        case itemAnnotation ann of
            CPPDirectiveAnn before ->
                return $ prependLine before $ pretty "#define" <+> pretty ident <+> pretty token
            _ -> error $ "Invalid annotation: " ++ show ann
    pprint (CPPDefine ident (Just (t : ts)) ann) =
        case itemAnnotation ann of
            CPPDirectiveAnn before ->
                return $ prependLine before $ vcat
                    [
                        pretty "#define" <+> pretty ident <+> pretty t <+> pretty "\\",
                        indentTab $ vcat $ punctuate (pretty "\\") (map pretty ts)
                    ]
            _ -> error $ "Invalid annotation: " ++ show ann
    pprint (CPPIfDef ident ann) =
        case itemAnnotation ann of
            CPPDirectiveAnn before ->
                return $ prependLine before $ pretty "#ifdef" <+> pretty ident
            _ -> error $ "Invalid annotation: " ++ show ann
    pprint (CPPInclude isSystem path ann) =
        case itemAnnotation ann of
            CPPDirectiveAnn before ->
                return $ prependLine before $ pretty "#include" <+>
                    pretty (if isSystem then "<" else "\"") <>
                        pretty path <> pretty
                            (if isSystem then ">" else "\"")
            _ -> error $ "Invalid annotation: " ++ show ann
    pprint (CPPIfNDef ident ann) =
        case itemAnnotation ann of
            CPPDirectiveAnn before ->
                return $ prependLine before $ pretty "#ifndef" <+> pretty ident
            _ -> error $ "Invalid annotation: " ++ show ann
    pprint (CPPEndif ann) =
        case itemAnnotation ann of
            CPPDirectiveAnn before ->
                return $ prependLine before $ pretty "#endif"
            _ -> error $ "Invalid annotation: " ++ show ann

instance PPrint CFunction where
    pprint (CFunction ty ident params body) = do
        pty <- pprint ty
        pparams <- mapM pprint params
        pbody <- pprint body
        return $ pty <+> pretty ident <> parens (align (fillSep (punctuate comma pparams))) <+> pbody

instance PPrint CExternalDeclaration where
    pprint (CEDVariable stspec decl ann) = do
        case itemAnnotation ann of 
            CDeclarationAnn before -> do
                pdecl <- pprint decl
                return $ prependLine before $ pretty stspec <+> pdecl <> semi
            _ -> error $ "Invalid annotation: " ++ show ann
    pprint (CEDFunction cfunc ann) = do
        case itemAnnotation ann of
            CDeclarationAnn before -> do
                pcfunc <- pprint cfunc
                return $ prependLine before $ pcfunc
            _ -> error $ "Invalid annotation: " ++ show ann
    pprint (CEDEnum enum ann) = do
        case itemAnnotation ann of
            CDeclarationAnn before -> do
                penum <- pprint enum
                return $ prependLine before $ penum <> semi
            _ -> error $ "Invalid annotation: " ++ show ann
    pprint (CEDStructUnion stu ann) = do
        case itemAnnotation ann of
            CDeclarationAnn before -> do
                pstruct <- pprint stu
                return $ prependLine before $ pstruct <> semi
            _ -> error $ "Invalid annotation: " ++ show ann
    pprint (CEDTypeDef ident ty ann) = do
        case itemAnnotation ann of
            CDeclarationAnn before -> do
                pty <- pprint ty
                return $ prependLine before $ pretty "typedef" <+> pty <+> pretty ident <> semi
            _ -> error $ "Invalid annotation: " ++ show ann

instance PPrint CFileItem where
    pprint (CExtDecl decl) = pprint decl
    pprint (CPPDirective directive) = pprint directive

instance PPrint CFile where
    pprint (CHeaderFile _path items) = do
        pItems <- mapM pprint items
        return $ vsep pItems <> line
    pprint (CSourceFile _path items) = do
        pItems <- mapM pprint items
        return $ vsep pItems <> line

runCPrinter :: CFile -> Text
runCPrinter cFile = render $ runReader (pprint cFile) (CPrinterConfig False False)
