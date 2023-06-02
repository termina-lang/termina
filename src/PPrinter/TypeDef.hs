module PPrinter.TypeDef where

import Prettyprinter

import AST
import PPrinter.Common

ppField :: Identifier -> TypeSpecifier a -> DocStyle
ppField identifier ts = ppDeclaration identifier ts <> semi

ppStructField :: FieldDefinition a -> DocStyle
ppStructField (FieldDefinition identifier ts) = ppField identifier ts

ppClassField :: ClassMember a -> DocStyle
ppClassField (ClassField identifier ts) = ppField identifier ts
ppClassField _ = error "invalid class member"

ppClassMutexField :: DocStyle
ppClassMutexField = mutex <+> pretty "__mutex_id" <> semi

filterStructModifiers :: [Modifier a] -> [Modifier a]
filterStructModifiers = foldr (\modifier ms ->
    case modifier of
      packedStructModifier -> modifier : ms
      Modifier "align" _ -> modifier : ms
      _ -> ms
    ) []

filterClassModifiers :: [Modifier a] -> [Modifier a]
filterClassModifiers = foldr (\modifier ms ->
    case modifier of
      Modifier "no_handler" Nothing -> modifier : ms
      _ -> ms) []

hasNoHandler :: [Modifier a] -> Bool
hasNoHandler modifiers = Modifier "no_handler" Nothing `elem` modifiers

-- | Pretty prints an enum variant
-- This function is only used when generating the variant enumeration
-- It takes as arguments the variant and its index.
ppEnumVariant :: EnumVariant a -> DocStyle
ppEnumVariant (EnumVariant identifier _) = pretty identifier

ppTypeAttributes :: [Modifier a] -> DocStyle
ppTypeAttributes [] = emptyDoc
ppTypeAttributes mods = attribute <> parens (parens (encloseSep emptyDoc emptyDoc (comma <> space) (map ppModifier mods))) <> space

ppEnumVariantParameter :: TypeSpecifier a -> Integer -> DocStyle
ppEnumVariantParameter ts index =
  ppDeclaration (namefy (show index)) ts <> semi

ppEnumVariantParameterStruct :: EnumVariant a -> DocStyle
ppEnumVariantParameterStruct (EnumVariant identifier params) =
  structC <+> braces' (
    vsep $
      zipWith
        (\x y -> indentTab . align $ ppEnumVariantParameter x y)
          params [0..]
      ) <+> pretty (namefy identifier) <> semi

-- | TypeDef pretty printer.
ppTypeDef :: Printer TypeDef a
-- | Struct declaration pretty printer
ppTypeDef before after (Struct identifier fls modifiers anns) =
  let structModifiers = filterStructModifiers modifiers in
  vsep [
    before anns,
    typedefC <+> structC <+> braces' (
      vsep $
        map (indentTab
          . align
          . ppStructField) fls
        ) <+> ppTypeAttributes structModifiers <> pretty identifier <> semi,
    after anns
  ]
-- | Union declaration pretty printer
ppTypeDef before after (Union identifier fls modifiers anns) =
  let structModifiers = filterStructModifiers modifiers in
  vsep [
    before anns,
    typedefC <+> unionC <+> braces' (
      vsep $
        map (indentTab
          . align
          . ppStructField) fls
        ) <+> ppTypeAttributes structModifiers <> pretty identifier <> semi,
    after anns
  ]
-- | Enum declaration pretty printer  
ppTypeDef before after (Enum identifier variants _ anns) =
  let variantsWithParams = filter (not . null . assocData) variants
  in
  vsep [
    before anns,
    typedefC <+> enumC <+> braces' (
      vsep $ punctuate comma $
        map (indentTab
          . align
          . ppEnumVariant) variants
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
    after anns
  ]
ppTypeDef before after (Class identifier members modifiers anns) =
  let structModifiers = filterStructModifiers modifiers in
  let classModifiers = filterClassModifiers modifiers in
  let (fields, methods) =
          foldr (\member (fs,ms) ->
               case member of
                 ClassField {} -> (member : fs, ms)
                 ClassMethod {} -> (fs, member : ms)
          ) ([],[]) members
  in
    if not (null fields) then
      vsep [
        before anns,
        typedefC <+> structC <+> braces' (
          vsep (
            -- | Map the regular fields
            map (indentTab
              . align
              . ppClassField) fields ++
            -- | If the no_handler modifier is set, then we may use
            -- a mutex sempahore to handle mutual exclusion
            ([(indentTab . align)
                ppClassMutexField | hasNoHandler classModifiers]))) 
              <+> ppTypeAttributes structModifiers <> pretty identifier <> semi,
        after anns
      ]
    else if hasNoHandler classModifiers then
      vsep [
        before anns,
        typedefC <+> structC <+> braces' (
            (indentTab . align) ppClassMutexField) <+> pretty identifier <> semi,
        after anns
      ]
    else emptyDoc
