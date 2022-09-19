-- | Module to pretty-print Termina Programs
module PPrinter where

import AST
import Prettyprinter

import Prettyprinter.Render.Terminal
import Data.Text (Text)

type DocStyle = Doc AnsiStyle

-- | Parameter pretty printer
ppParam :: Param -> DocStyle
ppParam (Param (nm, ty)) = pretty nm <+> colon <+> viaShow ty

-- | Compound Statement Printer
ppCStmt :: (a -> DocStyle) -> CompoundStmt a -> DocStyle
ppCStmt annP (Compound ldecs stmts) =
  vsep $ map ppLocalDec ldecs ++ map ppStmt stmts
  where
    ppStmt Assign = pretty "Assign"
    ppStmt (Conditional ann) = pretty "Conditional " <+> annP ann
    ppStmt (Skip ann) = pretty "Skip" <+> annP ann
    ppLocalDec (LDecl (nm, ty, c, ann)) =
      hsep
        [ pretty "let",
          pretty nm,
          colon,
          viaShow ty,
          pretty "=",
          viaShow c,
          annP ann
        ]

prettyPrintElem' :: (a -> DocStyle) -> AASTElem a -> DocStyle
prettyPrintElem' annP (Task nm param cstmt ann) =
  vsep
    [ hsep [pretty "Task", parens (ppParam param), align (braces (ppCStmt annP cstmt))],
      pretty "Ann: " <+> annP ann
    ]
prettyPrintElem' annP (Proc nm (p, ps) pty cstmt ann) = pretty "TODO"
prettyPrintElem' annP (Handler nm ps cstmt ann) = pretty "TODO"
prettyPrintElem' annP (GlbDec glb ann) = pretty "TODO"

render :: DocStyle -> Text
render = renderStrict . layoutSmart defaultLayoutOptions
