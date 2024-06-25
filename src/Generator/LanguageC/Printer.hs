{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Generator.LanguageC.Printer where

import Generator.LanguageC.AST
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

class PPrint a where
    pprint :: a -> CPrinter
    pprintPrec :: Integer -> a -> CPrinter

    pprint = pprintPrec 0
    pprintPrec _ = pprint

instance PPrint CDeclarator where
  pprintPrec prec (CDeclarator name derivedDeclrs _ _) = do
    printCDerivedDeclarator prec (reverse derivedDeclrs)
        where
            printCDerivedDeclarator :: Integer -> [CDerivedDeclarator] -> CPrinter
            printCDerivedDeclarator p (CPtrDeclr quals _ : declrs) = do
                case (declrs, quals) of
                    ([], []) -> do
                        case name of
                            Just n -> return $ parenPrec p 5 $ pretty "*" <+> pretty n
                            Nothing -> return $ parenPrec p 5 $ pretty "*"
                    ([], _) -> do
                        pquals <- mapM pprint quals
                        case name of
                            Just n -> return $ parenPrec p 5 $ pretty "*" <+> hsep pquals <+> pretty n
                            Nothing -> return $ parenPrec p 5 $ pretty "*" <+> hsep pquals
                    (_, []) -> do
                        pdeclrs <- printCDerivedDeclarator 5 declrs
                        return $ parenPrec p 5 $ pretty "*" <+> pdeclrs
                    _ -> do
                        pdeclrs <- printCDerivedDeclarator 5 declrs
                        pquals <- mapM pprint quals
                        return $ parenPrec p 5 $ pretty "*" <+> hsep pquals <+> pdeclrs
            printCDerivedDeclarator p (CArrDeclr quals size _ : declrs) = do
                psize <- pprint size
                case (declrs, quals) of
                    ([], []) -> do
                        case name of
                            Just n -> return $ parenPrec p 6 $ pretty n <> brackets psize
                            Nothing -> return $ parenPrec p 6 $ brackets psize
                    ([], _) -> do
                        pquals <- mapM pprint quals
                        case name of
                            Just n -> return $ parenPrec p 6 $ pretty n <> brackets (hsep pquals <+> psize)
                            Nothing -> return $ parenPrec p 6 $ brackets (hsep pquals <+> psize)
                    (_, []) -> do
                        pdeclrs <- printCDerivedDeclarator 6 declrs
                        return $ parenPrec p 6 $ pdeclrs <> brackets psize
                    _ -> do
                        pdeclrs <- printCDerivedDeclarator 6 declrs
                        pquals <- mapM pprint quals
                        return $ parenPrec p 6 $ pdeclrs <> brackets (hsep pquals <+> psize)
            printCDerivedDeclarator p (CFunDeclr params funAttrs _ : declrs) = do
                pdeclrs <- printCDerivedDeclarator 6 declrs
                pFunAttrs <- pprintCAttributeList funAttrs
                pparams <- printParams params
                return $ parenPrec p 6 $ (if not (null funAttrs) then parens (pFunAttrs <+> pdeclrs) else pdeclrs)
                                    <> parens pparams
            printCDerivedDeclarator _ [] = return $ maybe emptyDoc pretty name

            printParams :: [CDeclaration] -> CPrinter
            printParams decls = do
                pdecls <- mapM pprint decls
                return $ align (fillSep (punctuate comma pdecls))

instance PPrint CDeclarationSpecifier where
    pprint (CStorageSpec sp) = return $ pretty sp
    pprint (CTypeSpec sp) = pprint sp
    pprint (CTypeQual qu) = pprint qu

instance PPrint CTypeSpecifier where
    pprint CVoidType = return $ pretty "void"
    pprint CCharType = return $ pretty "char"
    pprint CUInt8Type = return $ pretty "uint8_t"
    pprint CUInt16Type = return $ pretty "uint16_t"
    pprint CUInt32Type = return $ pretty "uint32_t"
    pprint CUInt64Type = return $ pretty "uint64_t"
    pprint CInt8Type = return $ pretty "int8_t"
    pprint CInt16Type = return $ pretty "int16_t"
    pprint CInt32Type = return $ pretty "int32_t"
    pprint CInt64Type = return $ pretty "int64_t"
    pprint CSizeTType = return $ pretty "size_t"
    pprint CBoolType = return $ pretty "_Bool"
    pprint CInt128Type = return $ pretty "int128_t"
    pprint CUInt128Type = return $ pretty "uint128_t"
    pprint (CSUType stu) = pprint stu
    pprint (CEnumType enum) = pprint enum
    pprint (CTypeDef ident) = return $ pretty ident
    pprint (CAtomicType decl) = do
        pdecl <- pprint decl
        return $ pretty "_Atomic" <> parens pdecl

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

parenPrec :: Integer -> Integer -> DocStyle -> DocStyle
parenPrec prec prec2 t = if prec <= prec2 then t else parens t

instance PPrint CTypeQualifier where
    pprint CConstQual = return $ pretty "const"
    pprint CVolatQual = return $ pretty "volatile"
    pprint CRestrQual = return $ pretty "__restrict"
    pprint CAtomicQual = return $ pretty "_Atomic"
    pprint (CAttrQual a) = pprintCAttributeList [a]

instance PPrint CArraySize where
    pprint (CArrSize staticMod expr) = do
        pexpr <- pprintPrec 25 expr
        return $ if staticMod then pretty "static" else pexpr

instance PPrint CExpression where
    pprintPrec p (CComma exprs _) = do
        pexprs <- mapM (pprintPrec 2) exprs
        return $ parenPrec p (-1) $ hsep (punctuate comma pexprs)
    pprintPrec p (CAssignment expr1 expr2 _) = do
        pexpr1 <- pprintPrec 3 expr1
        pexpr2 <- pprintPrec 2 expr2
        return $ parenPrec p 2 $ pexpr1 <+> pretty "=" <+> pexpr2
    pprintPrec p (CBinary op expr1 expr2 _) = do
        let prec = binPrec op
        pexpr1 <- pprintPrec prec expr1
        pexpr2 <- pprintPrec (prec + 1) expr2
        return $ parenPrec p prec $ pexpr1 <+> pretty op <+> pexpr2
    pprintPrec p (CUnary op expr _) = do
        pexpr <- pprintPrec 25 expr
        return $ parenPrec p 25 $ pretty op <> pexpr
    pprintPrec p (CSizeofExpr expr _) = do
        pexpr <- pprintPrec 25 expr
        return $ parenPrec p 25 $ pretty "sizeof" <> parens pexpr
    pprintPrec p (CSizeofType decl _) = do
        pdecl <- pprint decl
        return $ parenPrec p 25 $ pretty "sizeof" <> parens pdecl
    pprintPrec p (CAlignofExpr expr _) = do
        pexpr <- pprintPrec 25 expr
        return $ parenPrec p 25 $ pretty "_Alignof" <> parens pexpr
    pprintPrec p (CAlignofType decl _) = do
        pdecl <- pprint decl
        return $ parenPrec p 25 $ pretty "_Alignof" <> parens pdecl
    pprintPrec p (CCast decl expr _) = do
        pdecl <- pprint decl
        pexpr <- pprintPrec 25 expr
        return $ parenPrec p 25 $ parens pdecl <> pexpr
    pprintPrec p (CIndex expr1 expr2 _) = do
        pexpr1 <- pprintPrec 26 expr1
        pexpr2 <- pprint expr2
        return $ parenPrec p 26 $ pexpr1 <> brackets pexpr2
    pprintPrec p (CCall expr args _) = do
        pexpr <- pprintPrec 30 expr
        pargs <- mapM pprint args
        return $ parenPrec p 30 $ pexpr <> parens (align (fillSep (punctuate comma pargs)))
    pprintPrec _ (CVar ident _) = return $ pretty ident
    pprintPrec _ (CConst c _) = pprint c
    pprintPrec p (CMember expr ident False _) = do
        pexpr <- pprintPrec 26 expr
        return $ parenPrec p 26 $ pexpr <> pretty "." <> pretty ident
    pprintPrec p (CMember expr ident True _) = do
        pexpr <- pprintPrec 26 expr
        return $ parenPrec p 26 $ pexpr <> pretty "->" <> pretty ident

instance PPrint CConstant where
    pprint (CIntConst i) = return $ pretty i
    pprint (CCharConst c) = return $ pretty c
    pprint (CStrConst s) = return $ pretty s

instance PPrint CStructureUnion where
    pprint (CStruct tag ident Nothing cattrs) = do
        case (cattrs, ident) of
            ([], Nothing) -> return $ pretty tag
            ([], Just ident') -> return $ pretty tag <+> pretty ident'
            (_, Nothing) -> do
                pattrs <- pprintCAttributeList cattrs
                return $ pretty tag <+> pattrs
            (_, Just ident') -> do
                pattrs <- pprintCAttributeList cattrs
                return $ pretty tag <+> pretty ident' <+> pattrs
    pprint (CStruct tag ident (Just []) cattrs) = do
        case (cattrs, ident) of
            ([], Nothing) -> return $ pretty tag <+> pretty "{ }"
            ([], Just ident') -> return $ pretty tag <+> pretty ident' <+> pretty "{ }"
            (_, Nothing) -> do
                pattrs <- pprintCAttributeList cattrs
                return $ pretty tag <+> pretty "{ }" <+> pattrs
            (_, Just ident') -> do
                pattrs <- pprintCAttributeList cattrs
                return $ pretty tag <+> pretty ident' <+> pretty "{ }" <+> pattrs
    pprint (CStruct tag ident (Just decls) cattrs) = do
        case (cattrs, ident) of
            ([], Nothing) -> do
                pdecls <- mapM pprint decls
                return $ vcat [
                    pretty tag <+> pretty "{",
                    indentTab $ vsep (map (<> semi) pdecls),
                    pretty "}"]
            ([], Just ident') -> do
                pdecls <- mapM pprint decls
                return $ vcat [
                    pretty tag <+> pretty ident' <+> pretty "{",
                    indentTab $ vsep (map (<> semi) pdecls),
                    pretty "}"]
            (_, Nothing) -> do
                pattrs <- pprintCAttributeList cattrs
                pdecls <- mapM pprint decls
                return $ vcat [
                    pretty tag  <+> pretty "{",
                    indent 4 $ vsep (map (<> semi) pdecls),
                    pretty "}" <+> pattrs]
            (_, Just ident') -> do
                pattrs <- pprintCAttributeList cattrs
                pdecls <- mapM pprint decls
                return $ vcat [
                    pretty tag <+> pretty ident' <+> pretty "{",
                    indent 4 $ vsep (map (<> semi) pdecls),
                    pretty "}" <+> pattrs]

instance PPrint CPartDesignator where
    pprint (CArrDesig expr _) = do
        pexpr <- pprintPrec 25 expr
        return $ pretty "[" <> pexpr <> pretty "]"
    pprint (CMemberDesig ident _) = return $ pretty "." <> pretty ident


instance PPrint CInitializer where
    pprint (CInitExpr expr _) = pprint expr
    pprint (CInitList initl _) = do
        pinitl <- mapM p initl
        return $ pretty "{" <+> hsep (punctuate comma pinitl) <+> pretty "}"
        where
            p ([], initializer) = pprint initializer
            p (desigs, initializer) = do
                pdesigs <- mapM pprint desigs
                pinit <- pprint initializer
                return $ hsep (punctuate comma pdesigs) <+> pretty "=" <+> pinit

instance PPrint CInitializerList where
    pprint initl = do
        pinitl <- mapM p initl
        return $ pretty "{" <+> hsep (punctuate comma pinitl) <+> pretty "}"
        where
            p ([], initializer) = pprint initializer
            p (desigs, initializer) = do
                pdesigs <- mapM pprint desigs
                pinit <- pprint initializer
                return $ hsep (punctuate comma pdesigs) <+> pretty "=" <+> pinit

instance PPrint CDeclaration where
    pprint d@(CDeclaration specs divs ann) = do
        case itemAnnotation ann of
            CDeclarationAnn before -> do
                pspecs <- mapM pprint specs
                pdivs <- mapM p divs
                case pdivs of
                    [] -> return $ prependLine before $ hsep pspecs
                    _ -> return $ prependLine before $ hsep pspecs <+> hsep (punctuate comma pdivs)
            _ -> error $ "Invalid annotation: " ++ show d

        where
        p (Just declr, Nothing, Nothing) = do
            pdeclr <- pprint declr
            case getAttrs declr of
                [] -> return pdeclr
                _ -> do
                    pattrs <- pprintCAttributeList (getAttrs declr)
                    return $ pdeclr <+> pattrs
        p (Just declr, Just initializer, Nothing) = do
            pdeclr <- pprint declr
            pinit <- pprint initializer
            case getAttrs declr of
                [] -> do
                    return $ pdeclr <+> pretty "=" <+> pinit
                _ -> do
                    pattrs <- pprintCAttributeList (getAttrs declr)
                    return $ pdeclr <+> pattrs <+> pretty "=" <+> pinit
        p (Just declr, Nothing, Just expr) = do
            pdeclr <- pprint declr
            pexpr <- pprint expr
            case getAttrs declr of
                [] -> return $ pdeclr <+> pretty ":" <+> pexpr
                _ -> do
                    pattrs <- pprintCAttributeList (getAttrs declr)
                    return $ pdeclr <+> pattrs <+> pretty ":" <+> pexpr
        p decl = error $ "Invalid CDeclaration: " ++ show decl

        getAttrs :: CDeclarator -> [CAttribute]
        getAttrs (CDeclarator _ _ cattrs _) = cattrs

instance PPrint CEnumeration where
    pprint (CEnum ident Nothing cattrs) = do
        case (cattrs, ident) of
            ([], Nothing) -> return $ pretty "enum"
            ([], Just ident') -> return $ pretty "enum" <+> pretty ident'
            (_, Nothing) -> do
                pattrs <- pprintCAttributeList cattrs
                return $ pretty "enum" <+> pattrs
            (_, Just ident') -> do
                pattrs <- pprintCAttributeList cattrs
                return $ pretty "enum" <+> pattrs <+> pretty ident'
    pprint (CEnum ident (Just vals) cattrs) = do
        pvals <- mapM p vals
        case (cattrs, ident) of
            ([], Nothing) -> return $ vcat [
                pretty "enum" <+> pretty "{",
                indent 4 $ vsep (punctuate comma pvals),
                pretty "}"]
            ([], Just ident') -> return $ vcat [
                pretty "enum" <+> pretty ident' <+> pretty "{",
                indent 4 $ vsep (punctuate comma pvals),
                pretty "}"]
            (_, Nothing) -> do
                pattrs <- pprintCAttributeList cattrs
                return $ vcat [
                    pretty "enum" <+> pattrs <+> pretty "{",
                    indent 4 $ vsep (punctuate comma pvals),
                    pretty "}"]
            (_, Just ident') -> do
                pattrs <- pprintCAttributeList cattrs
                return $ vcat [
                    pretty "enum" <+> pattrs <+> pretty ident' <+> pretty "{",
                    indent 4 $ vsep (punctuate comma pvals),
                    pretty "}"]
        where
            p :: (Ident, Maybe CExpression) -> CPrinter
            p (ident', Just expr) = do
                pexpr <- pprintPrec 25 expr
                return $ pretty ident' <+> pretty "=" <+> pexpr
            p (ident', Nothing) = return $ pretty ident'

prependLine :: Bool -> DocStyle -> DocStyle
prependLine True doc = line <> doc
prependLine False doc = doc

indentStmt :: Bool -> DocStyle -> DocStyle
indentStmt True doc = indentTab doc
indentStmt False doc = doc

instance PPrint CStatement where
    pprint s@(CCase expr stat ann) = do
        case itemAnnotation ann of
            CStatementAnn before expand -> do
                pexpr <- pprint expr
                pstat <- pprint stat
                return $ prependLine before $ indentStmt expand $ pretty "case" <+> pexpr <> pretty ":" <> line <> pstat
            _ -> error $ "Invalid annotation: " ++ show s
    pprint s@(CDefault stat ann) = do
        case itemAnnotation ann of
            CStatementAnn before expand -> do
                pstat <- pprint stat
                return $ prependLine before $ indentStmt expand $ pretty "default:" <> line <> pstat
            _ -> error $ "Invalid annotation: " ++ show s
    pprint s@(CExpr expr ann) = do
        case itemAnnotation ann of
            CStatementAnn before expand -> do
                pexpr <- maybe (return emptyDoc) pprint expr
                return $ prependLine before $ indentStmt expand $ pexpr <> semi
            _ -> error $ "Invalid annotation: " ++ show s
    pprint s@(CIf expr stat estat ann) = do
        case itemAnnotation ann of
            CStatementAnn before expand -> do
                pexpr <- pprint expr
                pstat <- pprint stat
                pestat <- case estat of
                    Nothing -> return emptyDoc
                    Just alt -> do
                        palt <- pprint alt
                        return $ pretty " else" <+> palt
                return $ prependLine before $ indentStmt expand $
                    pretty "if" <+> parens pexpr
                    <+> pstat
                    <> pestat
            _ -> error $ "Invalid annotation: " ++ show s
    pprint s@(CSwitch expr stat ann) = do
        case itemAnnotation ann of
            CStatementAnn before expand -> do
                pexpr <- pprint expr
                pstat <- pprint stat
                return $ prependLine before $ indentStmt expand $
                    pretty "switch" <+> parens pexpr
                    <+> pstat
            _ -> error $ "Invalid annotation: " ++ show s
    pprint s@(CFor for_init cond step stat ann) = do
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
    pprint s@(CCont ann) =
        case itemAnnotation ann of
            CStatementAnn before expand -> do
                return $ prependLine before $ indentStmt expand $
                    pretty "continue" <> semi
            _ -> error $ "Invalid annotation: " ++ show s
    pprint s@(CBreak ann) =
        case itemAnnotation ann of
            CStatementAnn before expand -> do
                return $ prependLine before $ indentStmt expand $
                    pretty "break" <> semi
            _ -> error $ "Invalid annotation: " ++ show s
    pprint s@(CReturn Nothing ann) =
        case itemAnnotation ann of
            CStatementAnn before expand -> do
                return $ prependLine before $ indentStmt expand $
                    pretty "return" <> semi
            _ -> error $ "Invalid annotation: " ++ show s
    pprint s@(CReturn (Just e) ann) =
        case itemAnnotation ann of
            CStatementAnn before expand -> do
                pe <- pprint e
                return $ prependLine before $ indentStmt expand $
                    pretty "return" <+> pe <> semi
            _ -> error $ "Invalid annotation: " ++ show s
    pprint s@(CCompound items ann) =
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

instance PPrint CExternalDeclaration where
    pprint (CDeclExt decl) = do
        pdecl <- pprint decl
        return $ pdecl <> semi
    pprint (CFDefExt fund) = pprint fund

instance PPrint CFunctionDef where
    pprint (CFunDef declspecs declr stat ann) =
        case itemAnnotation ann of
            CDeclarationAnn before -> do
                pdeclspecs <- mapM pprint declspecs
                pdeclr <- pprint declr
                pstat <- pprintPrec 25 stat
                return $ prependLine before $ hsep pdeclspecs <+> pdeclr <+> pstat
            _ -> error $ "Invalid annotation: " ++ show ann


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

-- precedence of C operators
binPrec :: CBinaryOp -> Integer
binPrec CMulOp = 20
binPrec CDivOp = 20
binPrec CRmdOp = 20
binPrec CAddOp = 19
binPrec CSubOp = 19
binPrec CShlOp = 18
binPrec CShrOp = 18
binPrec CLeOp  = 17
binPrec CGrOp  = 17
binPrec CLeqOp = 17
binPrec CGeqOp = 17
binPrec CEqOp  = 16
binPrec CNeqOp = 16
binPrec CAndOp = 15
binPrec CXorOp = 14
binPrec COrOp  = 13
binPrec CLndOp = 12
binPrec CLorOp = 11

braces' :: DocStyle -> DocStyle
braces' b = braces (line <> b <> line)

indentTab :: DocStyle -> DocStyle
indentTab = indent 4