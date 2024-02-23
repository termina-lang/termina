module PPrinter.TypeDef.Declaration where

import Prettyprinter

import AST.Seman
import PPrinter.Common
import Semantic.Monad
import Semantic.Option (OptionMap)
import qualified Data.Set as S
import qualified Data.Map as M

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
ppClassIDFieldDeclaration cls = error $ "invalid class kind: " ++ show cls

filterStructModifiers :: [Modifier] -> [Modifier]
filterStructModifiers = filter (\case
      Modifier "packed" Nothing -> True
      Modifier "align" _ -> True
      _ -> False)

ppSelfParameter :: Identifier -> DocStyle
ppSelfParameter classId = pretty classId <+> pretty "*" <+> pretty "const" <+> pretty "self"

ppInterfaceProcedureField :: InterfaceMember SemanticAnns -> DocStyle
ppInterfaceProcedureField (InterfaceProcedure identifier parameters _) =
  ppCFunctionPointer (voidC) (pretty identifier)
    (voidC <+> pretty "*" <+> pretty "__self" : (ppParameterDeclaration (pretty identifier) <$> parameters)) <> semi

ppClassFunctionDeclaration :: Identifier -> ClassMember SemanticAnns -> DocStyle
ppClassFunctionDeclaration classId (ClassProcedure identifier parameters _ _) =
  let clsFuncName = classFunctionName (pretty classId) (pretty identifier) in
  vsep $
  ([ppParameterVectorValueStructureDecl clsFuncName (pretty pid) ts <> line | (Parameter pid ts@(Vector {})) <- parameters]) ++
  [
    ppCFunctionPrototype clsFuncName
      (ppSelfParameter classId : (ppParameterDeclaration clsFuncName <$> parameters)) Nothing <> semi,
    emptyDoc
  ]
ppClassFunctionDeclaration classId (ClassMethod identifier mrts _ _) =
  let clsFuncName = classFunctionName (pretty classId) (pretty identifier) in
  vsep $
  (case mrts of
    Just ts@(Vector {}) ->
      [ppReturnVectorValueStructureDecl clsFuncName ts <> line]
    _ -> []) ++
  [
    ppCFunctionPrototype clsFuncName
      [ppSelfParameter classId]
      (ppReturnType clsFuncName <$> mrts) <> semi,
    emptyDoc
  ]
ppClassFunctionDeclaration classId (ClassViewer identifier parameters rts _ _) =
  let clsFuncName = classFunctionName (pretty classId) (pretty identifier) in
  vsep $
  ([ppParameterVectorValueStructureDecl clsFuncName (pretty pid) ts <> line | (Parameter pid ts@(Vector {})) <- parameters]) ++
  (case rts of
    Vector {} -> [ppReturnVectorValueStructureDecl clsFuncName rts <> line]
    _ -> []) ++
  [
    ppCFunctionPrototype clsFuncName
      (pretty "const" <+> ppSelfParameter classId : (ppParameterDeclaration clsFuncName <$> parameters))
      (Just (ppReturnType clsFuncName rts)) <> semi,
    emptyDoc
  ]
ppClassFunctionDeclaration classId (ClassAction identifier parameter rts _ _) =
  let clsFuncName = classFunctionName (pretty classId) (pretty identifier) in
  vsep $
  (case rts of
    Vector {} ->
      [ppReturnVectorValueStructureDecl clsFuncName rts <> line]
    _ -> []) ++
  (case parameter of
    Parameter pid ts@(Vector {}) ->
      [ppParameterVectorValueStructureDecl clsFuncName (pretty pid) ts <> line]
    _ -> []) ++
  [
    ppCFunctionPrototype clsFuncName
      [ppSelfParameter classId, ppParameterDeclaration clsFuncName parameter] (Just (ppReturnType clsFuncName rts)) <> semi,
    emptyDoc
  ]
ppClassFunctionDeclaration classId member = error $ "member of class " ++ classId ++ " not a function: " ++ show member


-- | Pretty prints the name of an enum variant
-- This function is only used when generating the variant enumeration
-- It takes as arguments the variant and its index.
ppEnumVariant :: Identifier -> EnumVariant -> DocStyle
ppEnumVariant identifier (EnumVariant variant _) = pretty identifier <::> pretty variant

ppTypeAttributes :: [Modifier] -> DocStyle
ppTypeAttributes [] = emptyDoc
ppTypeAttributes mods = attribute <> parens (parens (encloseSep emptyDoc emptyDoc (comma <> space) (map ppModifier mods))) <> space

ppEnumVariantParameter :: TypeSpecifier -> Integer -> DocStyle
ppEnumVariantParameter ts index =
  ppTypeSpecifier ts <+> namefy (pretty (show index)) <> ppDimension ts <> semi

ppEnumVariantParameterStructName :: Identifier -> EnumVariant -> DocStyle
ppEnumVariantParameterStructName identifier (EnumVariant variant _) = enumIdentifier (pretty identifier <::> pretty variant <> pretty "_params")

ppEnumVariantParameterStruct :: Identifier -> EnumVariant -> DocStyle
ppEnumVariantParameterStruct identifier enumVariant@(EnumVariant _variant params) =
  typedefC <+> structC <+> braces' (
    indentTab . align $ vsep $
      zipWith
        ppEnumVariantParameter params [0..]
      ) <+> ppEnumVariantParameterStructName identifier enumVariant <> semi

classifyClassMembers :: [ClassMember SemanticAnns] -> ([ClassMember SemanticAnns], [ClassMember SemanticAnns], [ClassMember SemanticAnns], [ClassMember SemanticAnns], [ClassMember SemanticAnns])
classifyClassMembers = foldr (\member (fs,ms,prs,vws,acts) ->
              case member of
                -- We are filtering out the fields that are not relevant for the class structure 
                ClassField (FieldDefinition _ (SinkPort {})) _ -> (fs, ms, prs, vws, acts)
                ClassField (FieldDefinition _ (InPort {})) _ -> (fs, ms, prs, vws, acts)
                ClassField {} -> (member : fs, ms, prs, vws, acts)
                ClassMethod {} -> (fs, member : ms, prs, vws, acts)
                ClassProcedure {} -> (fs, ms, member : prs, vws, acts)
                ClassViewer {} -> (fs, ms, prs, member : vws, acts)
                ClassAction {} -> (fs, ms, prs, vws, member : acts)
          ) ([],[], [], [], [])

ppOptionDefinition :: TypeSpecifier -> DocStyle
ppOptionDefinition (Option ts) =
  vsep [
    ppOptionSomeParameterStruct ts,
    emptyDoc,
    ppOptionStruct ts,
    emptyDoc
  ]
ppOptionDefinition ts = error $ "invalid option type: " ++ show ts

-- | TypeDef pretty printer.
ppTypeDefDeclaration :: OptionMap -> TypeDef SemanticAnns -> DocStyle
ppTypeDefDeclaration opts typeDef =
  case typeDef of
    -- | Struct declaration pretty pr_¨^ç+
    (Struct identifier fls modifiers) ->
      let structModifiers = filterStructModifiers modifiers in
      vsep $ (typedefC <+> structC <+> braces' (
          indentTab . align $ vsep $
            map ppStructField fls
            ) <+> ppTypeAttributes structModifiers <> pretty identifier <> semi) : 
          maybe [emptyDoc] (\s -> emptyDoc : map ppOptionDefinition (S.toList s)) (M.lookup (DefinedType identifier) opts)
    -- | Enum declaration pretty printer  
    (Enum identifier variants _) ->
      let variantsWithParams = filter (not . null . assocData) variants
      in
      vsep $ [
        -- | Print the declaration of the enum that defines the variants
        typedefC <+> enumC <+> braces' (
            indentTab . align $ vsep $ punctuate comma $
              map (ppEnumVariant identifier) variants
          ) <+> enumIdentifier (pretty identifier) <> semi, emptyDoc] ++
        -- | Print the declaration of the enum variant parameter structs
        concatMap (\enumVariant ->
            [ppEnumVariantParameterStruct identifier enumVariant,
            emptyDoc]) variantsWithParams ++
        -- | Print the main enumeration structs
        [typedefC <+> structC <+> braces' (line <> (indentTab . align $
          vsep [
            enumIdentifier (pretty identifier) <+> enumVariantsField <> semi,
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
        )) <+> pretty identifier <> semi] ++
        maybe [emptyDoc] (\s -> emptyDoc : map ppOptionDefinition (S.toList s)) (M.lookup (DefinedType identifier) opts)
    (Interface identifier members _) ->
      vsep [
              typedefC <+> structC <+> braces' (
                indentTab . align $
                vsep $
                  -- | Print the that field
                  voidC <+> pretty "*" <+> ppInterfaceThatField <> semi :
                  -- | Print the procedure fields
                  map ppInterfaceProcedureField members)
                    <+> pretty identifier <> semi,
              emptyDoc
      ]
    (Class clsKind identifier members _provides modifiers) ->
      let
        structModifiers = filterStructModifiers modifiers
        (fields, methods, procedures, viewers, actions) = classifyClassMembers members
      in
        vsep $ [
              typedefC <+> structC <+> braces' (
                indentTab . align $
                vsep (
                  -- | Map the regular fields except for the sink and in ports
                  map ppClassField (filter (\case {
                    ClassField (FieldDefinition _ (SinkPort {})) _ -> False;
                    ClassField (FieldDefinition _ (InPort {})) _ -> False;
                    _ -> True}) fields) ++
                  -- | Map the ID field. The type of this field
                  -- | depends on the class kind.
                  [ppClassIDFieldDeclaration clsKind]))
                    <+> ppTypeAttributes structModifiers <> pretty identifier <> semi,
              emptyDoc
          ] ++
          -- | Print the declaration of the class methods, procedures and viewers
          map (ppClassFunctionDeclaration identifier) (methods ++ procedures ++ viewers ++ actions)
