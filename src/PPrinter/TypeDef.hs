module PPrinter.TypeDef where

import Prettyprinter

import AST
import PPrinter.Common


ppStructField :: FieldDefinition -> DocStyle
ppStructField (FieldDefinition identifier ts) = ppDeclaration identifier ts <> semi

classMethodName :: Identifier -> Identifier -> Identifier
classMethodName classId methodId = "__" <> classId <> "_" <> methodId

ppClassMethodDeclaration :: Identifier -> ClassMember a -> DocStyle
ppClassMethodDeclaration classId (ClassMethod methodId parameters rTS _ _) =
  ppCFunctionDeclaration (pretty (classMethodName classId methodId))
    (map ppParameter parameters) (ppRootType <$> rTS) <> semi
ppClassMethodDeclaration _ _ = error "invalid class membeer"

-- | Pretty prints a class field
-- This function is only used when generating the class structure.
-- It takes as argument the class member to print.
-- It returns the pretty printed class field.
ppClassField :: ClassMember a -> DocStyle
ppClassField (ClassField identifier ts) = ppDeclaration identifier ts <> semi
ppClassField _ = error "invalid class member"

-- | Pretty prints a class mutex field
-- This function is only used when generating the class structure.
-- It returns the pretty printed class mutex field.
-- This field is only generated if the no_handler modifier is set.
ppClassMutexField :: DocStyle
ppClassMutexField = mutex <+> pretty "__mutex_id" <> semi

filterStructModifiers :: [Modifier] -> [Modifier]
filterStructModifiers = foldr (\modifier ms ->
    case modifier of
      Modifier "packed" Nothing -> modifier : ms
      Modifier "align" _ -> modifier : ms
      _ -> ms
    ) []

filterClassModifiers :: [Modifier] -> [Modifier]
filterClassModifiers = foldr (\modifier ms ->
    case modifier of
      Modifier "no_handler" Nothing -> modifier : ms
      _ -> ms) []

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
  ppDeclaration (namefy (show index)) ts <> semi

ppEnumVariantParameterStruct :: EnumVariant -> DocStyle
ppEnumVariantParameterStruct (EnumVariant identifier params) =
  structC <+> braces' (
    indentTab . align $ vsep $
      zipWith
        ppEnumVariantParameter params [0..]
      ) <+> pretty (namefy identifier) <> semi

ppTypeDefEq :: Identifier -> DocStyle
ppTypeDefEq identifier = 
  ppCFunctionDeclaration (typeDefEqFunctionName identifier)
    (map ppParameter [
    Parameter "__lhs" (Reference (DefinedType identifier)),
    Parameter "__rhs" (Reference (DefinedType identifier))])
  (ppRootType <$> Just UInt8)
  
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
    after anns,
    ppTypeDefEq identifier <> semi,
    emptyDoc
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
    after anns,
    ppTypeDefEq identifier <> semi,
    emptyDoc
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
    after anns,
    ppTypeDefEq identifier <> semi,
    emptyDoc
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
      ++ [ppTypeDefEq identifier <> semi, emptyDoc]
      ++ map (\m -> ppClassMethodDeclaration identifier m <> line) methods
