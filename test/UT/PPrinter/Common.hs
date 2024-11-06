module UT.PPrinter.Common where

import Utils.Annotations
import Semantic.Types
import Control.Monad.Reader
import Control.Monad.Except
import Generator.CodeGen.Expression
import Command.Configuration
import Generator.Platform
import Generator.CodeGen.Common
import Generator.LanguageC.Printer
import qualified Data.Map as M
import Semantic.AST
import Data.Text
import ControlFlow.BasicBlocks
import Generator.CodeGen.Statement
import Generator.CodeGen.TypeDefinition
import Generator.CodeGen.Function
import Prettyprinter

objSemAnn :: AccessKind -> TerminaType -> SemanticAnn
objSemAnn ak ts = Located (ETy (ObjectType ak ts)) Internal

stmtSemAnn :: SemanticAnn
stmtSemAnn = Located (STy SimpleStmtType) Internal

simpleTySemAnn :: TerminaType -> SemanticAnn
simpleTySemAnn ts = Located (ETy (SimpleType ts)) Internal

matchCaseSemAnn :: [TerminaType] -> SemanticAnn
matchCaseSemAnn ts = Located (STy (MatchCaseStmtType ts)) Internal

unitSemAnn :: SemanticAnn
unitSemAnn = simpleTySemAnn TUnit

uint8ExprSemAnn, uint32ExprSemAnn, uint16ExprSemAnn, uint64ExprSemAnn, int8ExprSemAnn,
    int16ExprSemAnn, int32ExprSemAnn, int64ExprSemAnn,
    usizeExprSemAnn, charExprSemAnn, boolExprSemAnn :: SemanticAnn
uint8ExprSemAnn = simpleTySemAnn TUInt8
uint32ExprSemAnn = simpleTySemAnn TUInt32
uint16ExprSemAnn = simpleTySemAnn TUInt16
uint64ExprSemAnn = simpleTySemAnn TUInt64
int8ExprSemAnn = simpleTySemAnn TInt8
int16ExprSemAnn = simpleTySemAnn TInt16
int32ExprSemAnn = simpleTySemAnn TInt32
int64ExprSemAnn = simpleTySemAnn TInt64
usizeExprSemAnn = simpleTySemAnn TUSize
charExprSemAnn = simpleTySemAnn TChar
boolExprSemAnn = simpleTySemAnn TBool

boxTySemAnn :: TerminaType -> SemanticAnn
boxTySemAnn ts = objSemAnn Mutable (TBoxSubtype ts)

boxUInt8SemAnn, boxUInt16SemAnn, boxUInt32SemAnn, boxUInt64SemAnn, boxInt8SemAnn,
    boxInt16SemAnn, boxInt32SemAnn, boxInt64SemAnn, boxCharSemAnn, boxBoolSemAnn :: SemanticAnn
boxUInt8SemAnn = boxTySemAnn TUInt8
boxUInt16SemAnn = boxTySemAnn TUInt16
boxUInt32SemAnn = boxTySemAnn TUInt32
boxUInt64SemAnn = boxTySemAnn TUInt64
boxInt8SemAnn = boxTySemAnn TInt8
boxInt16SemAnn = boxTySemAnn TInt16
boxInt32SemAnn = boxTySemAnn TInt32
boxInt64SemAnn = boxTySemAnn TInt64
boxCharSemAnn = boxTySemAnn TChar
boxBoolSemAnn = boxTySemAnn TBool

optionBoxObjSemAnn :: AccessKind -> TerminaType -> SemanticAnn
optionBoxObjSemAnn ak ts = objSemAnn ak (TOption (TBoxSubtype ts))

optionBoxExprSemAnn :: TerminaType -> SemanticAnn
optionBoxExprSemAnn ts = simpleTySemAnn (TOption (TBoxSubtype ts))

refSemAnn :: TerminaType -> SemanticAnn
refSemAnn ts = objSemAnn Immutable (TReference Mutable ts)

refUInt8SemAnn, refUInt16SemAnn, refUInt32SemAnn, refUInt64SemAnn, refInt8SemAnn,
    refInt16SemAnn, refInt32SemAnn, refInt64SemAnn, refCharSemAnn, refBoolSemAnn :: SemanticAnn
refUInt8SemAnn = refSemAnn TUInt8
refUInt16SemAnn = refSemAnn TUInt16
refUInt32SemAnn = refSemAnn TUInt32
refUInt64SemAnn = refSemAnn TUInt64
refInt8SemAnn = refSemAnn TInt8
refInt16SemAnn = refSemAnn TInt16
refInt32SemAnn = refSemAnn TInt32
refInt64SemAnn = refSemAnn TInt64
refCharSemAnn = refSemAnn TChar
refBoolSemAnn = refSemAnn TBool

arrayObjSemAnn :: AccessKind -> TerminaType -> Size -> SemanticAnn
arrayObjSemAnn ak ts size = objSemAnn ak (TArray ts size)

arrayExprSemAnn :: TerminaType -> Size -> SemanticAnn
arrayExprSemAnn ts size = simpleTySemAnn $ TArray ts size

boxArrayObjSemAnn :: TerminaType -> Size -> SemanticAnn
boxArrayObjSemAnn ts size = objSemAnn Mutable (TBoxSubtype (TArray ts size))

refArraySemAnn :: TerminaType -> Size -> SemanticAnn
refArraySemAnn ts size = objSemAnn Immutable (TReference Mutable (TArray ts size))

refTwoDymArraySemAnn :: TerminaType -> Size -> Size -> SemanticAnn
refTwoDymArraySemAnn ts size1 size2 = objSemAnn Immutable (TReference Mutable (TArray (TArray ts size1) size2))

uint16VecObjSemAnn, uint32VecObjSemAnn :: AccessKind -> Size -> SemanticAnn
uint16VecObjSemAnn ak = arrayObjSemAnn ak TUInt16
uint32VecObjSemAnn ak = arrayObjSemAnn ak TUInt32

twoDymArrayObjSemAnn :: AccessKind -> TerminaType -> Size -> Size -> SemanticAnn
twoDymArrayObjSemAnn ak ts size1 size2 = objSemAnn ak (TArray (TArray ts size1) size2)

twoDymArrayExprSemAnn :: TerminaType -> Size -> Size -> SemanticAnn
twoDymArrayExprSemAnn ts size1 size2 = simpleTySemAnn (TArray (TArray ts size1) size2)

uint16TwoDymVecObjSemAnn, uint32TwoDymVecObjSemAnn :: AccessKind -> Size -> Size -> SemanticAnn
uint16TwoDymVecObjSemAnn ak = twoDymArrayObjSemAnn ak TUInt16
uint32TwoDymVecObjSemAnn ak = twoDymArrayObjSemAnn ak TUInt32

boxTwoDymArrayObjSemAnn :: TerminaType -> Size -> Size -> SemanticAnn
boxTwoDymArrayObjSemAnn ts size1 size2 = objSemAnn Mutable (TBoxSubtype (TArray (TArray ts size1) size2))

boxThreeDymArrayObjSemAnn :: TerminaType -> Size -> Size -> Size -> SemanticAnn
boxThreeDymArrayObjSemAnn ts size1 size2 size3 = objSemAnn Mutable (TBoxSubtype (TArray (TArray (TArray ts size1) size2) size3))

uint16BoxTwoDymVecObjSemAnn, uint32BoxTwoDymVecObjSemAnn :: Size -> Size -> SemanticAnn
uint16BoxTwoDymVecObjSemAnn = boxTwoDymArrayObjSemAnn TUInt16
uint32BoxTwoDymVecObjSemAnn = boxTwoDymArrayObjSemAnn TUInt32

threeDymArrayObjSemAnn :: AccessKind -> TerminaType -> Size -> Size -> Size -> SemanticAnn
threeDymArrayObjSemAnn ak ts size1 size2 size3 = objSemAnn ak (TArray (TArray (TArray ts size1) size2) size3)

uint16ThreeDymVecObjSemAnn, uint32ThreeDymVecObjSemAnn :: AccessKind -> Size -> Size -> Size -> SemanticAnn
uint16ThreeDymVecObjSemAnn ak = threeDymArrayObjSemAnn ak TUInt16
uint32ThreeDymVecObjSemAnn ak = threeDymArrayObjSemAnn ak TUInt32

structObjSemAnn :: AccessKind -> Identifier -> SemanticAnn
structObjSemAnn ak ident = objSemAnn ak (TStruct ident)

enumObjSemAnn :: AccessKind -> Identifier -> SemanticAnn
enumObjSemAnn ak ident = objSemAnn ak (TEnum ident)

structExprSemAnn :: Identifier -> SemanticAnn
structExprSemAnn ident = simpleTySemAnn (TStruct ident)

enumExprSemAnn :: Identifier -> SemanticAnn
enumExprSemAnn ident = simpleTySemAnn (TEnum ident)

boxStructTypeSemAnn :: Identifier -> SemanticAnn
boxStructTypeSemAnn ident = boxTySemAnn (TStruct ident)

boxEnumTypeSemAnn :: Identifier -> SemanticAnn
boxEnumTypeSemAnn ident = boxTySemAnn (TStruct ident)

refStructSemAnn :: Identifier -> SemanticAnn
refStructSemAnn ident = refSemAnn (TStruct ident)

refGlobalResourceSemAnn :: Identifier -> SemanticAnn
refGlobalResourceSemAnn ident = refSemAnn (TGlobal ResourceClass ident)

resourceObjSemAnn :: AccessKind -> Identifier -> SemanticAnn
resourceObjSemAnn ak ident = objSemAnn ak (TGlobal ResourceClass ident)

refEnumSemAnn :: Identifier -> SemanticAnn
refEnumSemAnn ident = refSemAnn (TEnum ident)

poolSemAnn :: TerminaType -> SemanticAnn
poolSemAnn ts = objSemAnn Mutable (TAccessPort (TAllocator ts))

msgQueueSemAnn :: TerminaType -> SemanticAnn
msgQueueSemAnn ts = objSemAnn Mutable (TOutPort ts)

funSemAnn :: [TerminaType] -> TerminaType -> SemanticAnn
funSemAnn params ts = Located (ETy (AppType params ts)) Internal

renderExpression :: Expression SemanticAnn -> Text
renderExpression expr = 
  let configParams = defaultConfig "test" TestPlatform in
  case runReader (runExceptT (genExpression expr)) (CGeneratorEnv M.empty M.empty configParams) of
    Left err -> pack $ show err
    Right cExpr -> render $ runReader (pprint cExpr) (CPrinterConfig False False)

renderStatement :: Statement SemanticAnn -> Text
renderStatement stmt = 
  case runExcept (genBBlocks [] [stmt]) of
    Left err -> pack $ show err
    Right bBlocks ->
      let configParams = defaultConfig "test" TestPlatform in
      case runReader (runExceptT (Prelude.concat <$> mapM genBlocks bBlocks)) (CGeneratorEnv M.empty M.empty configParams) of
        Left err -> pack $ show err
        Right cStmts -> render $ vsep $ runReader (mapM pprint cStmts) (CPrinterConfig False False)

renderTypeDefinitionDecl :: OptionTypes -> AnnASTElement SemanticAnn -> Text
renderTypeDefinitionDecl opts decl = 
  case runExcept . genBBAnnASTElement $ decl of
    Left err -> pack $ show err
    Right bbDecl ->
      let configParams = defaultConfig "test" TestPlatform in
      case runReader (runExceptT (genTypeDefinitionDecl bbDecl)) (CGeneratorEnv M.empty opts configParams) of
        Left err -> pack $ show err
        Right cDecls -> render $ vsep $ runReader (mapM pprint cDecls) (CPrinterConfig False False)

renderFunctionDecl :: OptionTypes -> AnnASTElement SemanticAnn -> Text
renderFunctionDecl opts decl = 
  case runExcept . genBBAnnASTElement $ decl of
    Left err -> pack $ show err
    Right bbAST -> 
      let configParams = defaultConfig "test" TestPlatform in
      case runReader (runExceptT (genFunctionDecl bbAST)) (CGeneratorEnv M.empty opts configParams) of
        Left err -> pack $ show err
        Right cDecls -> render $ vsep $ runReader (mapM pprint cDecls) (CPrinterConfig False False) 

renderFunction :: AnnASTElement SemanticAnn -> Text
renderFunction func = 
  case runExcept . genBBAnnASTElement $ func of
    Left err -> pack $ show err
    Right bbAST -> 
      let configParams = defaultConfig "test" TestPlatform in
      case runReader (runExceptT (genFunction bbAST)) (CGeneratorEnv M.empty M.empty configParams) of
        Left err -> pack $ show err
        Right cDecls -> render $ vsep $ runReader (mapM pprint cDecls) (CPrinterConfig False False)