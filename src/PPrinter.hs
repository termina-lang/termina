-- | Module to pretty-print Termina Programs
module PPrinter where

import Prelude hiding (id)

import AST
import Prettyprinter
import Prettyprinter.Render.Terminal

import Data.Text (Text)

type DocStyle = Doc AnsiStyle

type Printer b a = (a -> DocStyle) -> b a -> DocStyle
--------------------------------------------------------------------------------
-- C pretty keywords
return, typedef, enum, struct, union :: DocStyle
typedef = pretty "typedef"
enum = pretty "enum"
struct = pretty "struct"
union = pretty "union"
return = pretty "return"


declarationList :: [DocStyle] -> DocStyle
declarationList =
  encloseSep
    emptyDoc
    semi
    (semi <> line)

braces' :: DocStyle -> DocStyle
braces' b = braces (line <> b <> line)

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



indentTab :: DocStyle -> DocStyle
indentTab = indent 4

namefy :: String -> String
namefy = ("__" ++)

commented :: DocStyle -> DocStyle
commented c = vsep [ pretty "/*", c , pretty "*/"]

--------------------------------------------------------------------------------

-- | Basic Type Printer
ppType :: Printer TypeSpecifier a
ppType _ppa Int8 = pretty "int8_t"
ppType _ppa Int16 = pretty "int16_t"
ppType _ppa Int32 = pretty "int32_t"
ppType _ppa Int64 = pretty "int64_t"
ppType _ppa UInt8 = pretty "uint8_t"
ppType _ppa UInt16 = pretty "uint16_t"
ppType _ppa UInt32 = pretty "uint32_t"
ppType _ppa UInt64 = pretty "uint64_t"
ppType _ppa Bool = pretty "uint8_t"
ppType _ppa Char = pretty "char"
ppType _ppa (DefinedType ident) = pretty ident
-- Missing Cases
ppType _ppa (Vector {}) = pretty "TODO:VEC"
ppType _ppa (MsgQueue {}) = pretty "TODO:MsgQueue"
ppType _ppa (Pool {}) = pretty "TODO:POOL"
ppType _ppa (Option {}) = pretty "TODO:Option"
ppType _ppa (Reference {}) = pretty "TODO:Reference"
ppType _ppa (DynamicSubtype {}) = pretty "TODO:Dynamic"

ppEnumVariant :: Printer EnumVariant a
ppEnumVariant ppa a = hsep (map (ppType ppa) (assocData a)) <+> pretty (namefy (variantIdentifier a))

ppFieldDefinition :: Printer FieldDefinition a
ppFieldDefinition ppa fld = hsep [
  ppType ppa $ fieldTypeSpecifier fld,
  pretty $ fieldIdentifier fld
  ]

ppParameter :: Printer Parameter a
ppParameter ppa (Parameter ident ty) = ppType ppa ty <+> pretty ident

ppClassMemDef :: Printer ClassMember a
ppClassMemDef ppa (ClassField ident tyspec _mbdef anns) =
  vsep [
  commented $ vsep $ map ppa anns,
  ppType ppa tyspec <+> pretty ident
  ]
ppClassMemDef ppa (ClassMethod ident params mbty blocks anns) =
  let nmPreffix = pretty (namefy ident) in
  vsep [
  commented $ vsep $ map ppa anns,
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


-- | TypeDef pretty printer.
ppTypeDef :: Printer TypeDef a
ppTypeDef ppa (Struct id fls anns) =
  vsep [
  commented (align $ vsep $ map ppa anns),
  typedef <+> struct,
  braces' $ indentTab $ align $ declarationList $ map (ppFieldDefinition ppa) fls,
  pretty id <> semi
       ]
ppTypeDef ppa (Union ident fls anns) =
  vsep [
  commented (align $ vsep $ map ppa anns),
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
        commented (align $ vsep $ map ppa anns),
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
  commented (align $ vsep $ map ppa anns),
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

render :: DocStyle -> Text
render = renderStrict . layoutSmart defaultLayoutOptions

ppAnnAST :: Printer AnnASTElement a
ppAnnAST _ppa (TypeDefinition tydef) = ppTypeDef _ppa tydef
ppAnnAST _ppa _ = error "TODO"

ppProgram :: Program -> [Text]
ppProgram = map (render . ppAnnAST undefined)

ppAnnonProgram :: Show a => AnnotatedProgram a -> Text
ppAnnonProgram = render . vsep . map (ppAnnAST (pretty . show))
