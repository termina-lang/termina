module PPrinter.TypeDef where

import Prettyprinter

import AST
import PPrinter.Common


ppStructField :: FieldDefinition -> DocStyle
ppStructField (FieldDefinition identifier ts) = ppDeclaration identifier ts <> semi

classMethodName :: String -> String -> String
classMethodName classId methodId = "__" <> classId <> "_" <> methodId

ppClassMethodDeclaration :: Identifier -> ClassMember a -> DocStyle
ppClassMethodDeclaration classId (ClassMethod methodId parameters rTS _ _) =
  ppPrinterFunctionDeclaration (classMethodName classId methodId) parameters rTS <> semi
ppClassMethodDeclaration _ _ = error "invalid class membeer"

ppClassField :: ClassMember a -> DocStyle
ppClassField (ClassField identifier ts) = ppDeclaration identifier ts <> semi
ppClassField _ = error "invalid class member"

ppClassMutexField :: DocStyle
ppClassMutexField = mutex <+> pretty "__mutex_id" <> semi

filterStructModifiers :: [Modifier a] -> [Modifier a]
filterStructModifiers = foldr (\modifier ms ->
    case modifier of
      Modifier "packed" Nothing -> modifier : ms
      Modifier "align" _ -> modifier : ms
      _ -> ms
    ) []

filterClassModifiers :: [Modifier a] -> [Modifier a]
filterClassModifiers = foldr (\modifier ms ->
    case modifier of
      Modifier "no_handler" Nothing -> modifier : ms
      _ -> ms) []

hasNoHandler :: [Modifier a] -> Bool
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

ppTypeAttributes :: [Modifier a] -> DocStyle
ppTypeAttributes [] = emptyDoc
ppTypeAttributes mods = attribute <> parens (parens (encloseSep emptyDoc emptyDoc (comma <> space) (map ppModifier mods))) <> space

ppEnumVariantParameter :: TypeSpecifier -> Integer -> DocStyle
ppEnumVariantParameter ts index =
  ppDeclaration (namefy (show index)) ts <> semi

ppEnumVariantParameterStruct :: EnumVariant -> DocStyle
ppEnumVariantParameterStruct (EnumVariant identifier params) =
  structC <+> braces' (
    indentTab . align $ vsep $
      zipWith
        ppEnumVariantParameter params [0..]
      ) <+> pretty (namefy identifier) <> semi

-- | TypeDef pretty printer.
ppTypeDef :: Printer TypeDef a
-- | Struct declaration pretty printer
ppTypeDef before after (Struct identifier fls modifiers anns) =
  let structModifiers = filterStructModifiers modifiers in
  vsep [
    before anns,
    typedefC <+> structC <+> braces' (
      indentTab . align $ vsep $
        map ppStructField fls
        ) <+> ppTypeAttributes structModifiers <> pretty identifier <> semi,
    after anns
  ]
-- | Union declaration pretty printer
ppTypeDef before after (Union identifier fls modifiers anns) =
  let structModifiers = filterStructModifiers modifiers in
  vsep [
    before anns,
    typedefC <+> unionC <+> braces' (
      indentTab . align $ vsep $
        map ppStructField fls
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
    vsep $ (
      if not (null fields) then
        [
          before anns,
          typedefC <+> structC <+> braces' (
            indentTab . align $
            vsep (
              -- | Map the regular fields
              map ppClassField fields ++
              -- | If the no_handler modifier is set, then we may use
              -- a mutex sempahore to handle mutual exclusion
              ([ppClassMutexField | hasNoHandler classModifiers])))
                <+> ppTypeAttributes structModifiers <> pretty identifier <> semi,
          after anns
        ]
      else if hasNoHandler classModifiers then
        [
          before anns,
          typedefC <+> structC <+> braces' (
              (indentTab . align) ppClassMutexField) <+> pretty identifier <> semi,
          after anns
        ]
      else
        [emptyDoc])
      ++ map (\m -> ppClassMethodDeclaration identifier m <> line) methods

