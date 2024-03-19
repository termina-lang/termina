{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Generator.LanguageC.Printer where

import Generator.LanguageC.AST
import Prettyprinter

import Prettyprinter.Render.Terminal
import Control.Monad.Reader


type DocStyle = Doc AnsiStyle

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
  pprintPrec prec (CDeclarator name derivedDeclrs attrs _) = do
    decls <- printCDerivedDeclarator prec derivedDeclrs
    return $ decls
        where
            printCDerivedDeclarator :: Integer -> [CDerivedDeclarator] -> CPrinter
            printCDerivedDeclarator _ [] = return $ maybe emptyDoc pretty name
            printCDerivedDeclarator p (CPtrDeclr quals _ : declrs) = do
                case (declrs, quals) of
                    ([], []) -> return $ parenPrec p 5 $ pretty "*"
                    ([], _) -> do
                        pquals <- mapM pprint quals
                        return $ parenPrec p 5 $ pretty "*" <> hsep pquals
                    (_, []) -> do
                        pdeclrs <- printCDerivedDeclarator 5 declrs
                        return $ parenPrec p 5 $ pretty "*" <> pdeclrs
                    _ -> do
                        pdeclrs <- printCDerivedDeclarator 5 declrs
                        pquals <- mapM pprint quals
                        return $ parenPrec p 5 $ pretty "*" <> hsep pquals <> pdeclrs
            printCDerivedDeclarator p (CArrDeclr quals size _ : declrs) = do
                pdeclrs <- printCDerivedDeclarator 6 declrs
                pquals <- mapM pprint quals
                psize <- pprint size
                return $ parenPrec p 6 $ pdeclrs <> brackets (hsep pquals <+> psize)
            printCDerivedDeclarator p (CFunDeclr params funAttrs _ : declrs) = do
                pdeclrs <- printCDerivedDeclarator 6 declrs
                pFunAttrs <- pprintCAttributeList funAttrs
                pparams <- printParams params
                return $ parenPrec p 6 $ (if not (null funAttrs) then parens (pFunAttrs <+> pdeclrs) else pdeclrs)
                                    <> parens pparams
            
            printParams :: [CDeclaration] -> CPrinter
            printParams decls = do
                pdecls <- mapM pprint decls
                return $ sep (punctuate comma pdecls)

instance PPrint CDeclarationSpecifier where
    pprint (CStorageSpec sp) = return $ pretty sp
    pprint (CTypeSpec sp) = pprint sp
    pprint (CTypeQual qu) = pprint qu
    pprint (CFunSpec fs) = return $ pretty fs

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
    return $ pretty "__attribute__" <> parens (hsep (punctuate comma pattrs))

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
    pprint (CNoArrSize completeType) = return $ if completeType then pretty "*" else emptyDoc
    pprint (CArrSize staticMod expr) = do
        pexpr <- pprintPrec 25 expr
        return $ if staticMod then pretty "static" else emptyDoc <+> pexpr

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
        pexpr <- pprintPrec 26 expr
        return $ parenPrec p 26 $ pretty op <> pexpr
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
        pexpr2 <- pprintPrec 26 expr2
        return $ parenPrec p 25 $ pexpr1 <> brackets pexpr2
    pprintPrec p (CCall expr args _) = do
        pexpr <- pprintPrec 30 expr
        pargs <- mapM pprint args
        return $ parenPrec p 30 $ pexpr <> parens (hsep (punctuate comma pargs))
    pprintPrec _ (CCompoundLit decl initl _) = do
        pdecl <- pprint decl
        pinitl <- pprint initl
        return $ parens pdecl <+> pinitl
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
        pattrs <- pprintCAttributeList cattrs
        return $ pretty tag <+> pattrs <+> maybe emptyDoc pretty ident
    pprint (CStruct tag ident (Just []) cattrs) = do
        pattrs <- pprintCAttributeList cattrs
        return $ pretty tag <+> pattrs <+> maybe emptyDoc pretty ident <+> pretty "{ }"
    pprint (CStruct tag ident (Just decls) cattrs) = do
        pattrs <- pprintCAttributeList cattrs
        pdecls <- mapM pprint decls
        return $ vcat [
            pretty tag <+> pattrs <+> maybe emptyDoc pretty ident <+> pretty "{",
            indent 4 $ sep (map (<> semi) pdecls),
            pretty "}"]

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
    pprint (CDeclaration specs divs _) = do
        pspecs <- mapM pprint specs
        pdivs <- mapM p divs
        case pdivs of
            [] -> return $ hsep pspecs
            _ -> return $ hsep pspecs <+> hsep (punctuate comma pdivs)
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
        pattrs <- pprintCAttributeList cattrs
        return $ pretty "enum" <+> pattrs <+> maybe emptyDoc pretty ident
    pprint (CEnum ident (Just vals) cattrs) = do
        pattrs <- pprintCAttributeList cattrs
        pvals <- mapM p vals
        return $ vcat [
            pretty "enum" <+> pattrs <+> maybe emptyDoc pretty ident <+> pretty "{",
            indent 4 $ sep (punctuate comma pvals),
            pretty "}"]
        where
            p :: (Ident, Maybe CExpression) -> CPrinter
            p (ident', Just expr) = do
                pexpr <- pprintPrec 25 expr
                return $ pretty ident' <+> pretty "=" <+> pexpr
            p (ident', Nothing) = return $ pretty ident'

instance PPrint CStatement where
    pprint (CCase expr stat _) = do
        pexpr <- pprint expr
        pstat <- pprint stat
        return $ pretty "case" <+> pexpr <> pretty ":" <> pstat
    pprint (CDefault stat _) = do
        pstat <- pprint stat
        return $ pretty "default:" <> pstat
    pprint (CExpr expr _) = do
        pexpr <- maybe (return emptyDoc) pprint expr
        return $ pexpr <> semi
    pprint (CIf expr stat estat _) = do
        pexpr <- pprint expr
        pstat <- pprint stat
        pestat <- maybe (return emptyDoc) pprint estat
        return $ pretty "if" <+> parens pexpr
            <+> pstat
            <> pestat
    pprint (CSwitch expr stat _) = do
        pexpr <- pprint expr
        pstat <- pprint stat
        return $ pretty "switch" <+> parens pexpr
            <+> pstat
    pprint (CFor for_init cond step stat _) = do
        pfor_init <- either (maybe (return emptyDoc) pprint) pprint for_init
        pcond <- maybe (return emptyDoc) pprint cond
        pstep <- maybe (return emptyDoc) pprint step
        pstat <- pprint stat
        return $ pretty "for" <+> parens (pfor_init <> semi <+> pcond <> semi <+> pstep) <+> pstat
    pprint (CCont _) = return $ pretty "continue" <> semi
    pprint (CBreak _) = return $ pretty "break" <> semi
    pprint (CReturn Nothing _) = return $ pretty "return" <> semi
    pprint (CReturn (Just e) _) = do
        pe <- pprint e
        return $ pretty "return" <+> pe <> semi
    pprint (CCompound items _) = do
        pItems <- mapM pprint items 
        return $ pretty "{" <+> vcat pItems <+> pretty "}"

instance PPrint CCompoundBlockItem where
    pprint (CBlockStmt stat) = pprint stat
    pprint (CBlockDecl decl) = do
        pdecl <- pprint decl
        return $ pdecl <> semi

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
