module PPrinter.TypeDef where

import Prettyprinter

import SemanAST
import PPrinter.Common
import Semantic.Monad (SemanticAnns)


ppStructField :: FieldDefinition -> DocStyle
ppStructField (FieldDefinition identifier ts) = ppTypeSpecifier ts <+> pretty identifier <> ppDimension ts <> semi

classMethodName :: Identifier -> Identifier -> DocStyle
classMethodName = methodName

ppClassMethodDeclaration :: Identifier -> ClassMember a -> DocStyle
ppClassMethodDeclaration classId (ClassMethod methodId parameters _ _ _) =
  ppCFunctionDeclaration (classMethodName classId methodId)
    (map (ppParameterDeclaration (pretty (classId ++ "_" ++ methodId))) parameters) Nothing <> semi
ppClassMethodDeclaration _ _ = error "invalid class member"

-- | Pretty prints a class field
-- This function is only used when generating the class structure.
-- It takes as argument the class member to print.
-- It returns the pretty printed class field.
ppClassField :: ClassMember a -> DocStyle
ppClassField (ClassField identifier ts _) = ppTypeSpecifier ts <+> pretty identifier <> ppDimension ts <> semi
ppClassField _ = error "invalid class member"

-- | Pretty prints a class mutex field
-- This function is only used when generating the class structure.
-- It returns the pretty printed class mutex field.
-- This field is only generated if the no_handler modifier is set.
ppClassMutexField :: DocStyle
ppClassMutexField = mutex <+> pretty "__mutex_id" <> semi

ppClassDummyField :: DocStyle
ppClassDummyField = uint32C <+> pretty "__dummy" <> semi

filterStructModifiers :: [Modifier] -> [Modifier]
filterStructModifiers = filter (\case
      Modifier "packed" Nothing -> True
      Modifier "align" _ -> True
      _ -> False)
    
filterClassModifiers :: [Modifier] -> [Modifier]
filterClassModifiers = filter (\case
      Modifier "no_handler" Nothing -> True
      _ -> False)

hasNoHandler :: [Modifier] -> Bool
hasNoHandler modifiers = case modifiers of
  [] -> False
  (m : ms) -> case m of
    Modifier "no_handler" Nothing -> True
    _ -> hasNoHandler ms

-- | Pretty prints an enum variant
-- This function is only used when generating the variant enumeration
-- It takes as arguments the variant and its index.
ppEnumVariant :: EnumVariant -> DocStyle
ppEnumVariant (EnumVariant identifier _) = pretty identifier

ppTypeAttributes :: [Modifier] -> DocStyle
ppTypeAttributes [] = emptyDoc
ppTypeAttributes mods = attribute <> parens (parens (encloseSep emptyDoc emptyDoc (comma <> space) (map ppModifier mods))) <> space

ppEnumVariantParameter :: TypeSpecifier -> Integer -> DocStyle
ppEnumVariantParameter ts index =
  ppTypeSpecifier ts <+> pretty (namefy (show index)) <> ppDimension ts <> semi

ppEnumVariantParameterStruct :: EnumVariant -> DocStyle
ppEnumVariantParameterStruct (EnumVariant identifier params) =
  structC <+> braces' (
    indentTab . align $ vsep $
      zipWith
        ppEnumVariantParameter params [0..]
      ) <+> pretty (namefy identifier) <> semi

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
    -- | Union declaration pretty printer (TO BE REMOVED)
    (Union identifier fls modifiers) ->
      let structModifiers = filterStructModifiers modifiers in
      vsep [
        typedefC <+> unionC <+> braces' (
          indentTab . align $ vsep $
            map ppStructField fls
            ) <+> ppTypeAttributes structModifiers <> pretty identifier <> semi,
        emptyDoc
      ]
    -- | Enum declaration pretty printer  
    (Enum identifier variants _) ->
      let variantsWithParams = filter (not . null . assocData) variants
      in
      vsep [
        typedefC <+> enumC <+> braces' (
          indentTab . align $ vsep $ punctuate comma $
            map ppEnumVariant variants
            ) <+> enumIdentifier identifier <> semi,
        emptyDoc, -- empty line
        typedefC <+> structC <+> braces' (
          vsep [
            emptyDoc, -- empty line
            indentTab . align $ enumIdentifier identifier <+> enumVariantsField <> semi,
            if null variantsWithParams then emptyDoc else
              indentTab . align $
              vsep [
                emptyDoc, -- empty line
                unionC <+> braces' (
                  vsep $ map (indentTab . align
                    . ppEnumVariantParameterStruct) variantsWithParams
                ) <> semi,
                emptyDoc -- empty line
              ]
          ]
        ) <+> pretty identifier <> semi,
        emptyDoc ]
    (Class identifier members modifiers) ->
      let structModifiers = filterStructModifiers modifiers in
      let classModifiers = filterClassModifiers modifiers in
      let (fields, methods) =
              foldr (\member (fs,ms) ->
                  case member of
                    ClassField {} -> (member : fs, ms)
                    ClassMethod {} -> (fs, member : ms)
              ) ([],[]) members
      in
        vsep $ (
          if not (null fields) then
            [
              typedefC <+> structC <+> braces' (
                indentTab . align $
                vsep (
                  -- | Map the regular fields
                  map ppClassField fields ++
                  -- | If the no_handler modifier is set, then we may use
                  -- a mutex sempahore to handle mutual exclusion
                  ([ppClassMutexField | hasNoHandler classModifiers])))
                    <+> ppTypeAttributes structModifiers <> pretty identifier <> semi,
              emptyDoc
            ]
          else if hasNoHandler classModifiers then
            [
              typedefC <+> structC <+> braces' (
                  (indentTab . align) ppClassMutexField) <+> pretty identifier <> semi,
              emptyDoc
            ]
          else
            [
              -- | If the class had no fields, then we must enter a dummy field:
              -- | this is because the C language does not allow empty structs
              typedefC <+> structC <+> braces' (
                  (indentTab . align) ppClassDummyField) <+> pretty identifier <> semi,
              emptyDoc
            ])
          ++ map (\m -> ppClassMethodDeclaration identifier m <> line) methods
