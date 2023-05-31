-- | Module to pretty-print Termina Programs

module PPrinter where

import Prelude hiding (id)

import AST
-- https://hackage.haskell.org/package/prettyprinter
import Prettyprinter
import Prettyprinter.Render.Terminal

import Parsing (Annotation)

import Data.Text (Text)

type DocStyle = Doc AnsiStyle

-- | Type of the pretty printers
type Printer a =
  (Annotation -> DocStyle)
  -- ^ Function that pretty prints an annotation BEFORE printing the construct
  -> (Annotation -> DocStyle)
  -- ^ Function that pretty prints an annotation AFTER printing the construct
  -> a Annotation
  -- ^ The annotated element to pretty print
  -> DocStyle

--------------------------------------------------------------------------------
-- C pretty keywords
-- Creturn, C_typedef, C_enum, C_struct, C_union :: DocStyle
returnC, typedefC, enumC, structC, unionC :: DocStyle
returnC = pretty "return"
typedefC = pretty "typedef"
enumC = pretty "enum"
structC = pretty "struct"
unionC = pretty "union"

-- C pretty unsigned integer types
uint8C, uint16C, uint32C, uint64C :: DocStyle
uint8C = pretty "uint8_t"
uint16C = pretty "uint16_t"
uint32C = pretty "uint32_t"
uint64C = pretty "uint64_t"

-- C pretty signed integer types
int8C, int16C, int32C, int64C :: DocStyle
int8C = pretty "int8_t"
int16C = pretty "int16_t"
int32C = pretty "int32_t"
int64C = pretty "int64_t"

-- C pretty char
charC :: DocStyle
charC = pretty "char"

-- C attribute pragma
attribute :: DocStyle
attribute = pretty "__attribute__"

-- Termina's pretty builtin types
pool, msgQueue :: DocStyle
pool = pretty "__termina_pool_t"
msgQueue = pretty "__termina_msg_queue_id_t"

enumIdentifier :: String -> DocStyle
enumIdentifier identifier = pretty "__enum_" <> pretty identifier

enumVariantsField :: DocStyle
enumVariantsField = pretty "__variants"

braces' :: DocStyle -> DocStyle
braces' b = braces (line <> b <> line)
{-
function
  -- Type
  :: DocStyle
  -- Name
  -> DocStyle
  -- Params
  -> [DocStyle]
  -- Body
  -> DocStyle -> DocStyle
function ty nm ps bd =
  vsep [
    ty <+> nm <+> parens ( sep (punctuate comma ps) ) ,
    braces' ( align (indentTab bd) )
       ]
-}
indentTab :: DocStyle -> DocStyle
indentTab = indent 4

-- | This function is used to create the names of temporal variables
-- and symbols.
namefy :: String -> String
namefy = ("__" ++)

ppRootType :: TypeSpecifier -> DocStyle
ppRootType UInt8 = uint8C
ppRootType UInt16 = uint16C
ppRootType UInt32 = uint32C
ppRootType UInt64 = uint64C
ppRootType Int8 = int8C
ppRootType Int16 = int16C
ppRootType Int32 = int32C
ppRootType Int64 = int64C
ppRootType Bool = uint8C
ppRootType Char = charC
ppRootType (DefinedType typeIdentifier) = pretty typeIdentifier
ppRootType (Vector ts _) = ppRootType ts
ppRootType (Option ts) = case ts of
  Option ts' -> ppRootType ts'
  Vector ts' _ -> ppRootType ts'
  Reference ts' -> ppRootType ts'
  DynamicSubtype ts' -> ppRootType ts'
  Unit -> error "unsupported type"
  _ -> ppRootType ts <+> pretty "*"
ppRootType (Pool _ _) = pool
ppRootType (Reference ts) = case ts of
  Option ts' -> ppRootType ts'
  Vector ts' _ -> ppRootType ts'
  Reference ts' -> ppRootType ts'
  DynamicSubtype ts' -> ppRootType ts'
  Unit -> error "unsupported type"
  _ -> ppRootType ts <+> pretty "*"
ppRootType (MsgQueue _ _) = msgQueue
ppRootType (DynamicSubtype ts) = case ts of
  Option ts' -> ppRootType ts'
  Vector ts' _ -> ppRootType ts'
  Reference ts' -> ppRootType ts'
  DynamicSubtype ts' -> ppRootType ts'
  Unit -> error "unsupported type"
  _ -> ppRootType ts <+> pretty "*"
ppRootType Unit = error "unsupported type"

ppSize :: TypeSpecifier -> DocStyle
ppSize (Vector ts (K size)) = ppSize ts <> brackets (pretty size)
ppSize _ = emptyDoc

ppDeclaration :: String -> TypeSpecifier -> DocStyle
ppDeclaration identifier ts = ppRootType ts <+> pretty identifier <> ppSize ts

ppFieldDefinition :: FieldDefinition -> DocStyle
ppFieldDefinition (FieldDefinition identifier typeSpecifier) =
  ppDeclaration identifier typeSpecifier <> semi

-- | Pretty prints an enum variant
-- This function is only used when generating the variant enumeration
-- It takes as arguments the variant and its index.
ppEnumVariant :: EnumVariant -> DocStyle
ppEnumVariant (EnumVariant identifier _) = pretty identifier

--------------------------------------------------------------------------------

{- 
ppClassMemDef :: Printer ClassMember a
ppClassMemDef ppa (ClassField ident tyspec _mbdef anns) =
  vsep [
  -- commented $ vsep $ map ppa anns,
  ppType ppa tyspec <+> pretty ident
  ]
ppClassMemDef ppa (ClassMethod ident params mbty blocks anns) =
  let nmPreffix = pretty (namefy ident) in
  vsep [
  -- commented $ vsep $ map ppa anns,
  -- TODO Nothing ClassMethod Ty
  function
    (maybe (pretty "void") (ppType ppa) mbty)
    nmPreffix
    (map (ppParameter ppa) params)
    (vsep (punctuate semi
           ( -- Format body of block
             map (ppCStmt ppa) (blockBody blocks)
             --
             ++
             -- Format return
             maybe [PPrinter.return] (const [pretty "TODO EXP"]) (fst (blockRet blocks)))))
       ]

-}

ppModifier :: Modifier a -> DocStyle
ppModifier (Modifier identifier (Just (KC (I _ integer) _))) = pretty identifier <> parens (pretty integer)
ppModifier (Modifier identifier (Just (KC (B True) _))) = pretty identifier <> parens (pretty "1")
ppModifier (Modifier identifier (Just (KC (B False) _))) = pretty identifier <> parens (pretty "0")
ppModifier (Modifier identifier (Just (KC (C char) _))) = pretty identifier <> parens (pretty "'" <> pretty char <> pretty "'")
ppModifier (Modifier identifier Nothing) = pretty identifier

ppTypeAttributes :: [Modifier a] -> DocStyle
ppTypeAttributes [] = emptyDoc
ppTypeAttributes mods = attribute <> parens (parens (encloseSep emptyDoc emptyDoc (comma <> space) (map ppModifier mods))) <> space

ppEnumVariantParameter :: TypeSpecifier -> Integer -> DocStyle
ppEnumVariantParameter ts index = 
  ppDeclaration (namefy (show index)) ts <> semi

ppEnumVariantParameterStruct :: EnumVariant -> DocStyle
ppEnumVariantParameterStruct (EnumVariant id params) =
  structC <+> braces' (
    vsep $
      zipWith 
        (\x y -> indentTab . align $ ppEnumVariantParameter x y) 
          params [0..]
      ) <+> pretty (namefy id) <> semi

-- | TypeDef pretty printer.
ppTypeDef :: Printer TypeDef
-- | Struct declaration pretty printer
ppTypeDef _ _ (Struct id fls modifiers _) =
  typedefC <+> structC <+> braces' (
    vsep $
      map (indentTab
        . align
        . ppFieldDefinition) fls
      ) <+> ppTypeAttributes modifiers <> pretty id <> semi
-- | Union declaration pretty printer
ppTypeDef _ _ (Union id fls modifiers _) =
  typedefC <+> unionC <+> braces' (
    vsep $
      map (indentTab
        . align
        . ppFieldDefinition) fls
      ) <+> ppTypeAttributes modifiers <> pretty id <> semi
-- | Enum declaration pretty printer  
ppTypeDef _ _ (Enum id variants _ _) =
  let variantsWithParams = filter (not . null . assocData) variants
  in
  vsep [
    typedefC <+> enumC <+> braces' (
      vsep $ punctuate comma $
        map (indentTab
          . align
          . ppEnumVariant) variants
        ) <+> enumIdentifier id <> semi,
    emptyDoc, -- empty line
    typedefC <+> structC <+> braces' (
      vsep [
        emptyDoc, -- empty line
        indentTab . align $ enumIdentifier id <+> enumVariantsField <> semi,
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
    ) <+> pretty id <> semi
  ]
ppTypeDef _ _ _ = pretty "vaya"

{-
ppTypeDef ppa (Union ident fls anns) =
  vsep [
  --commented (align $ vsep $ map ppa anns),
  typedef <+> union,
  braces' $ indentTab $ align $ declarationList $ map (ppFieldDefinition ppa) fls,
  pretty ident <> semi
       ]
ppTypeDef ppa (Enum ident enns anns) =
  let (enums, unions) = foldr (\(a, p) (es, us) ->
                                 ( hsep (map pretty [variantIdentifier a, "=", show p])
                                  : es
                                 , if null (assocData a)
                                   then us
                                   else ppEnumVariant ppa a : us
                                 )) ([], []) (zip enns ([0 ..] :: [Int]))
  in
    vsep [
        --commented (align $ vsep $ map ppa anns),
        typedef <+> enum,
        braces' $ indentTab $ align $ vsep $ punctuate comma enums,
        if null unions
        then -- Just enum
        pretty ident <> semi
        else -- Struct enum
        let enumid = namefy ("enum_" ++ ident) in
                vsep [
                  pretty enumid <> semi,
                  typedef <+> struct,
                  braces' (indentTab (align(vsep [
                                  pretty enumid <+> pretty (namefy "variant") <> semi,
                                  union,
                                  (braces' $ indentTab $ align $ vsep $ punctuate comma unions) <> semi
                                  ])))
                  ],
        pretty ident <> semi
        ]
ppTypeDef ppa (Class ident clsm anns) =
  let (fields, methods) =
        foldl (\(fs,ms) clmem ->
                 let ppDef = ppClassMemDef ppa clmem in
                 case clmem of
                   ClassField {} -> (ppDef : fs, ms)
                   ClassMethod {} -> (fs, ppDef : ms)
              )
            ([],[]) clsm
  in
  vsep [
  -- commented (align $ vsep $ map ppa anns),
  -- Struct
  typedef <+> struct,
  braces' $ indentTab $ align $ declarationList fields,
  pretty ident <> semi,
  -- Methods
  vsep methods
       ]

-- | Compound Statement Printer
ppCStmt :: Printer Statement a
ppCStmt _ _ = pretty "TODO"
-- ppCStmt annP (Compound ldecs stmts) =
--   vsep $ map ppLocalDec ldecs ++ map ppStmt stmts
--   where
--     ppStmt Assign = pretty "Assign"
--     ppStmt (Conditional ann) = pretty "Conditional " <+> annP ann
--     ppStmt (Skip ann) = pretty "Skip" <+> annP ann
--     ppLocalDec (LDecl (nm, ty, c, ann)) =
--       hsep
--         [ pretty "let",
--           pretty nm,
--           colon,
--           viaShow ty,
--           pretty "=",
--           viaShow c,
--           annP ann
--         ]

-- prettyPrintElem' :: (a -> DocStyle) -> AASTElem a -> DocStyle
-- prettyPrintElem' annP (Task nm param cstmt ann) =
--   vsep
--     [ hsep [pretty "Task", parens (ppParam param), align (braces (ppCStmt annP cstmt))],
--       pretty "Ann: " <+> annP ann
--     ]
-- prettyPrintElem' annP (Proc nm (p, ps) pty cstmt ann) = pretty "TODO"
-- prettyPrintElem' annP (Handler nm ps cstmt ann) = pretty "TODO"
-- prettyPrintElem' annP (GlbDec glb ann) = pretty "TODO"
-}
render :: DocStyle -> Text
render = renderStrict . layoutSmart defaultLayoutOptions

ppAnnAST :: Printer AnnASTElement
ppAnnAST before after (TypeDefinition t) = ppTypeDef before after t
ppAnnAST _ _ _ = pretty "vaya"

ppProgram :: AnnotatedProgram Annotation -> Text
ppProgram = render . vsep . map (ppAnnAST undefined undefined)

-- ppProgramDebug :: AnnotatedProgram Annotation -> Text
-- ppAnnonProgram = render . vsep . map (ppAnnAST (pretty . show))
