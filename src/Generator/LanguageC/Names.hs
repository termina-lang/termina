-- | Names declared by a file of generated C, except struct and union members.
module Generator.LanguageC.Names (
    CNameScope(..), DeclaredName(..), declaredNames
) where

import Generator.LanguageC.AST
import Utils.Annotations

-- | File scope, or block or prototype scope.
data CNameScope = CGlobalName | CLocalName
  deriving (Eq, Show)

-- | Name declared by the generated C, with its scope and its source location.
data DeclaredName = DeclaredName
  {
    declaredIdent :: Ident
  , declaredScope :: CNameScope
  , declaredLocation :: Location
  }
  deriving Show

-- | Names declared by a file, in order of appearance.
declaredNames :: CFile -> [DeclaredName]
declaredNames (CSourceFile _ items) = concatMap fileItem items
declaredNames (CHeaderFile _ items) = concatMap fileItem items

fileItem :: CFileItem -> [DeclaredName]
fileItem (CExtDecl decl ann) = externalDeclaration (getLocation ann) decl
fileItem (CFunctionDef _ (CFunction _ name params body) ann) =
    let loc = getLocation ann in
    DeclaredName name CGlobalName loc
        : concatMap (declaration CLocalName loc) params
        ++ statement body
fileItem (CPPDirective (CPPDefine name _) ann) = [DeclaredName name CGlobalName (getLocation ann)]
fileItem (CPPDirective _ _) = []

externalDeclaration :: Location -> CExternalDeclaration -> [DeclaredName]
externalDeclaration loc decl = case decl of
    CEDVariable _ d -> declaration CGlobalName loc d
    CEDMacroVariable _ d -> declaration CGlobalName loc d
    CEDFunction _ _ _ name params ->
        DeclaredName name CGlobalName loc : concatMap (declaration CLocalName loc) params
    CEDEnum tag e -> tagged tag ++ enumerators loc e
    CEDStructUnion tag s -> tagged tag ++ structUnion loc s
    CEDTypeDef name _ -> [DeclaredName name CGlobalName loc]

  where

    tagged = maybe [] (\t -> [DeclaredName t CGlobalName loc])

-- | Name of a declaration and enumerators of an enum declared in its type.
declaration :: CNameScope -> Location -> CDeclaration -> [DeclaredName]
declaration scope loc (CDecl ty mName _) =
    maybe [] (\name -> [DeclaredName name scope loc]) mName ++ typeNames loc ty

typeNames :: Location -> CTerminaType -> [DeclaredName]
typeNames loc ty = case ty of
    CTSEnum e -> enumerators loc e
    CTSStructUnion s -> structUnion loc s
    CTypeSpec _ -> []

-- | Tag of a nested struct or union and enumerators declared in its members.
structUnion :: Location -> CStructureUnion -> [DeclaredName]
structUnion loc (CStruct _ tag members _) =
    maybe [] (\t -> [DeclaredName t CGlobalName loc]) tag
        ++ concatMap (\(CMember (CDecl ty _ _) _) -> typeNames loc ty) members

enumerators :: Location -> CEnum -> [DeclaredName]
enumerators _ (CEnum _ values _) =
    [DeclaredName name CGlobalName (getLocation ann) | CEnumerator name _ ann <- values]

statement :: CStatement -> [DeclaredName]
statement stmt = case stmt of
    CSSkip -> []
    CSCase _ s _ -> statement s
    CSDefault s _ -> statement s
    CSDo _ _ -> []
    CSCompound items _ -> concatMap blockItem items
    CSIfThenElse _ s mElse _ -> statement s ++ maybe [] statement mElse
    CSFor (Right d) _ _ s ann -> declaration CLocalName (getLocation ann) d ++ statement s
    CSFor (Left _) _ _ s _ -> statement s
    CSReturn _ _ -> []
    CSSwitch _ s _ -> statement s
    CSBreak _ -> []

blockItem :: CCompoundBlockItem -> [DeclaredName]
blockItem (CBlockStmt s) = statement s
blockItem (CBlockDecl d ann) = declaration CLocalName (getLocation ann) d
