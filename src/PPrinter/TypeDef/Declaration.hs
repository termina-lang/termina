module PPrinter.TypeDef.Declaration where

import Prettyprinter

import AST.Seman
import PPrinter.Common
import Semantic.Monad

ppStructField :: FieldDefinition -> DocStyle
ppStructField (FieldDefinition identifier ts) = ppTypeSpecifier ts <+> pretty identifier <> ppDimension ts <> semi

-- | Pretty prints a class field
-- This function is only used when generating the class structure.
-- It takes as argument the class member to print.
-- It returns the pretty printed class field.
ppClassField :: ClassMember a -> DocStyle
ppClassField (ClassField (FieldDefinition identifier ts) _) = ppTypeSpecifier ts <+> pretty identifier <> ppDimension ts <> semi
ppClassField _ = error "invalid class member"

-- | Pretty prints the ID field of a class depending on its kind
ppClassIDFieldDeclaration :: ClassKind -> DocStyle
ppClassIDFieldDeclaration ResourceClass = resourceID <+> ppResourceClassIDField <> semi
ppClassIDFieldDeclaration TaskClass = taskID <+> ppTaskClassIDField <> semi
ppClassIDFieldDeclaration HandlerClass = handlerID <+> ppHandlerClassIDField <> semi

filterStructModifiers :: [Modifier] -> [Modifier]
filterStructModifiers = filter (\case
      Modifier "packed" Nothing -> True
      Modifier "align" _ -> True
      _ -> False)

ppSelfParameter :: Identifier -> DocStyle
ppSelfParameter classId = pretty classId <+> pretty "*" <+> pretty "const" <+> pretty "self"

ppClassFunctionDeclaration :: Identifier -> ClassMember SemanticAnns -> DocStyle
ppClassFunctionDeclaration classId (ClassProcedure identifier parameters _ _) =
  vsep $
  ([ppParameterVectorValueStructureDecl (classFunctionName classId identifier) (pretty pid) ts <> line | (Parameter pid ts@(Vector {})) <- parameters]) ++
  [
    ppCFunctionPrototype (classFunctionName classId identifier)
      (ppSelfParameter classId : (ppParameterDeclaration (classFunctionName classId identifier) <$> parameters)) Nothing <> semi,
    emptyDoc
  ]
ppClassFunctionDeclaration classId (ClassMethod identifier mrts _ _) =
  vsep $
  (case mrts of
    Just ts@(Vector {}) ->
      [ppReturnVectorValueStructureDecl (classFunctionName classId identifier) ts <> line]
    _ -> []) ++
  [
    ppCFunctionPrototype (classFunctionName classId identifier)
      [ppSelfParameter classId]
      (ppReturnType (classFunctionName classId identifier) <$> mrts) <> semi,
    emptyDoc
  ]
ppClassFunctionDeclaration classId (ClassViewer identifier parameters rts _ _) =
  vsep $
  ([ppParameterVectorValueStructureDecl (classFunctionName classId identifier) (pretty pid) ts <> line | (Parameter pid ts@(Vector {})) <- parameters]) ++
  (case rts of
    Vector {} -> [ppReturnVectorValueStructureDecl (classFunctionName classId identifier) rts <> line]
    _ -> []) ++
  [
    ppCFunctionPrototype (pretty identifier)
      (pretty "const" <+> ppSelfParameter classId : (ppParameterDeclaration (pretty identifier) <$> parameters))
      (Just (ppReturnType (pretty identifier) rts)) <> semi,
    emptyDoc
  ]
ppClassFunctionDeclaration classId member = error $ "member of class " ++ classId ++ " not a function: " ++ show member


-- | Pretty prints the name of an enum variant
-- This function is only used when generating the variant enumeration
-- It takes as arguments the variant and its index.
ppEnumVariant :: Identifier -> EnumVariant -> DocStyle
ppEnumVariant identifier (EnumVariant variant _) = pretty (namefy (identifier ++ "_" ++ variant))

ppTypeAttributes :: [Modifier] -> DocStyle
ppTypeAttributes [] = emptyDoc
ppTypeAttributes mods = attribute <> parens (parens (encloseSep emptyDoc emptyDoc (comma <> space) (map ppModifier mods))) <> space

ppEnumVariantParameter :: TypeSpecifier -> Integer -> DocStyle
ppEnumVariantParameter ts index =
  ppTypeSpecifier ts <+> pretty (namefy (show index)) <> ppDimension ts <> semi

ppEnumVariantParameterStructName :: Identifier -> EnumVariant -> DocStyle
ppEnumVariantParameterStructName identifier (EnumVariant variant _) = enumIdentifier (identifier ++ "_" ++ variant ++ "_" ++ "params")

ppEnumVariantParameterStruct :: Identifier -> EnumVariant -> DocStyle
ppEnumVariantParameterStruct identifier enumVariant@(EnumVariant _variant params) =
  typedefC <+> structC <+> braces' (
    indentTab . align $ vsep $
      zipWith
        ppEnumVariantParameter params [0..]
      ) <+> ppEnumVariantParameterStructName identifier enumVariant <> semi

classifyClassMembers :: [ClassMember SemanticAnns] -> ([ClassMember SemanticAnns], [ClassMember SemanticAnns], [ClassMember SemanticAnns], [ClassMember SemanticAnns])
classifyClassMembers = foldr (\member (fs,ms,prs,vws) ->
              case member of
                ClassField {} -> (member : fs, ms, prs, vws)
                ClassMethod {} -> (fs, member : ms, prs, vws)
                ClassProcedure {} -> (fs, ms, member : prs, vws)
                ClassViewer {} -> (fs, ms, prs, member : vws)
          ) ([],[], [], [])

-- | TypeDef pretty printer.
ppTypeDefDeclaration :: TypeDef SemanticAnns -> DocStyle
ppTypeDefDeclaration typeDef =
  case typeDef of
    -- | Struct declaration pretty pr_¨^ç+
    (Struct identifier fls modifiers) ->
      let structModifiers = filterStructModifiers modifiers in
      vsep [
        typedefC <+> structC <+> braces' (
          indentTab . align $ vsep $
            map ppStructField fls
            ) <+> ppTypeAttributes structModifiers <> pretty identifier <> semi,
        emptyDoc]
    -- | Enum declaration pretty printer  
    (Enum identifier variants _) ->
      let variantsWithParams = filter (not . null . assocData) variants
      in
      vsep $ [
        -- | Print the declaration of the enum that defines the variants
        typedefC <+> enumC <+> braces' (
            indentTab . align $ vsep $ punctuate comma $
              map (ppEnumVariant identifier) variants
          ) <+> enumIdentifier identifier <> semi, emptyDoc] ++
        -- | Print the declaration of the enum variant parameter structs
        concatMap (\enumVariant ->
            [ppEnumVariantParameterStruct identifier enumVariant,
            emptyDoc]) variantsWithParams ++
        -- | Print the main enumeration struct
        [typedefC <+> structC <+> braces' (line <> (indentTab . align $
          vsep [
            enumIdentifier identifier <+> enumVariantsField <> semi,
            case variantsWithParams of
              [] -> emptyDoc
              [enumVariant@(EnumVariant variant _)] -> 
                  vsep [
                    emptyDoc, -- empty line
                    ppEnumVariantParameterStructName identifier enumVariant
                      <+> pretty variant <> semi,
                    emptyDoc -- empty line
                  ]
              _ -> 
                  vsep [
                    emptyDoc, -- empty line
                    unionC <+> braces' (indentTab . align $
                      vsep $ map (\enumVariant@(EnumVariant variant _) ->
                        ppEnumVariantParameterStructName identifier enumVariant
                          <+> pretty variant <> semi) variantsWithParams
                    ) <> semi,
                    emptyDoc -- empty line
                  ]
          ]
        )) <+> pretty identifier <> semi,
        emptyDoc ]
    (Class clsKind identifier members modifiers) ->
      let
        structModifiers = filterStructModifiers modifiers
        (fields, methods, procedures, viewers) = classifyClassMembers members
      in
        vsep $ [
              typedefC <+> structC <+> braces' (
                indentTab . align $
                vsep (
                  -- | Map the regular fields
                  map ppClassField fields ++
                  -- | Map the ID field. The type of this field
                  -- | depends on the class kind.
                  [ppClassIDFieldDeclaration clsKind]))
                    <+> ppTypeAttributes structModifiers <> pretty identifier <> semi,
              emptyDoc
          ] ++
          -- | Print the declaration of the class methods, procedures and viewers
          map (ppClassFunctionDeclaration identifier) (methods ++ procedures ++ viewers)
