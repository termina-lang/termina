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
typedef, enum, struct, union :: DocStyle
typedef = pretty "typedef"
enum = pretty "enum"
struct = pretty "struct"
union = pretty "union"

namefy :: String -> String
namefy = ("__" ++)
--------------------------------------------------------------------------------

ppType :: Printer TypeSpecifier a
ppType _ppa _ = pretty "TODO"

ppEnumVariant :: Printer EnumVariant a
ppEnumVariant ppa a = hsep (map (ppType ppa) (assocData a)) <+> pretty (namefy (variantIdentifier a))

ppFieldDefinition :: Printer FieldDefinition a
ppFieldDefinition ppa fld = hsep [
  ppType ppa $ fieldTypeSpecifier fld,
  pretty $ fieldIdentifier fld
  ]

-- | TypeDef pretty printer.
ppTypeDef :: Printer TypeDef a
ppTypeDef _ppa (Struct id fls _anns) =
  vsep [
  typedef <+> struct,
  braces $ indent 4 $ align $ vsep $ punctuate semi $ map (ppFieldDefinition _ppa) fls,
  pretty (namefy id) <> semi
       ]
ppTypeDef _ppa (Union _id _fls _anns) = pretty "TODO : Union Type declaration"
ppTypeDef _ppa (Enum id enns _anns) =
  let (enums, unions) = foldr (\(a, p) (es, us) ->
                                 ( hsep (map pretty [variantIdentifier a, "=", show p])
                                  : es
                                 , if null (assocData a)
                                   then us
                                   else ppEnumVariant _ppa a : us
                                 )) ([], []) (zip enns ([0 ..] :: [Int]))
  in
    vsep [
        typedef <+> pretty id,
        braces $ indent 4 $ align $ vsep $ punctuate comma enums,
        if null unions
        then -- Just enum
        pretty id <> semi
        else -- Struct enum
        let enumid = namefy ("enum_" ++ id) in
                vsep [
                  pretty enumid <> semi,
                  typedef <+> struct,
                  braces (indent 4 (align(vsep [
                                  pretty enumid <+> pretty (namefy "variant") <> semi,
                                  union,
                                  braces $ indent 4 $ align $ vsep $ punctuate comma unions
                                  ])))
                  ]
        ]
ppTypeDef _ppa (Class _id _clsm _anns) = pretty "TODO : Class definition"

-- | Compound Statement Printer
ppCStmt :: (a -> DocStyle) -> Statement a -> DocStyle
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
