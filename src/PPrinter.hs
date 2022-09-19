-- | Module to pretty-print Termina Programs
module PPrinter where

import AST
import Prettyprinter

-- import PrettyPrinter.Render.Terminal

-- | Parameter pretty printer
ppParam :: Param -> Doc ann
ppParam (Param (nm, ty)) = pretty nm <+> colon <+> viaShow ty

-- | Compound Statement Printer
ppCStmt :: (a -> Doc ann) -> CompoundStmt a -> Doc ann
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

prettyPrintElem' :: (a -> Doc ann) -> AASTElem a -> Doc ann
prettyPrintElem' annP (Task nm param cstmt ann) =
  vsep
    [ hsep [pretty "Task", parens (ppParam param), align (braces (ppCStmt annP cstmt))],
      pretty "Ann: " <+> annP ann
    ]
prettyPrintElem' annP (Proc nm (p, ps) pty cstmt ann) = pretty "TODO"
prettyPrintElem' annP (Handler nm ps cstmt ann) = pretty "TODO"
prettyPrintElem' annP (GlbDec glb ann) = pretty "TODO"
