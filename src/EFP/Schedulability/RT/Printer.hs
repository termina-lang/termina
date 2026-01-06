module EFP.Schedulability.RT.Printer where

import EFP.Schedulability.RT.AST
import Prettyprinter
import Text.Parsec.Pos
import Data.Text (Text)
import Data.Char
import Numeric
import Utils.Printer

class RTPrinter a where
    pprint :: a -> DocStyle

(<::>) :: DocStyle -> DocStyle -> DocStyle
d1 <::> d2 = d1 <> pretty "::" <> d2

indentTab :: DocStyle -> DocStyle
indentTab = indent 4

brackets' :: DocStyle -> DocStyle
brackets' b = brackets (line <> b <> line)

braces' :: DocStyle -> DocStyle
braces' b = braces (line <> b <> line)

instance RTPrinter SourcePos where
    pprint pos = pretty (sourceLine pos) <> colon <> pretty (sourceColumn pos)

instance RTPrinter TInteger where

    pprint (TInteger i DecRepr) = pretty i
    pprint (TInteger i HexRepr) = pretty "0x" <> pretty (toUpper <$> showHex i "")
    pprint (TInteger i OctalRepr) = pretty "0" <> pretty (showOct i "")

instance RTPrinter (ConstExpression a) where
    pprint (ConstInt intVal _) =
        pprint intVal
    pprint (ConstDouble d _) =
        pretty (show d)
    pprint (ConstObject ident _) =
        pretty ident
    pprint (ConstBinOp op left right _) =
        let ppLeft = pprint left
            ppRight = pprint right
            ppOp = pretty (show op)
        in
            parens (ppLeft <+> ppOp <+> ppRight)

instance RTPrinter BlockPosition where
    pprint (BlockPosition startLine startColumn endLine endColumn) =
        pretty "@" <> parens (pretty startLine <> colon <> pretty startColumn <> comma <> pretty endLine <> colon <> pretty endColumn)

instance RTPrinter (ConstFieldValue a) where

    pprint (ConstStructFieldValue structInit) =
        pprint structInit
    pprint (ConstStructSimpleValue expr) =
        pprint expr

instance RTPrinter (ConstFieldAssignment a) where

    pprint (ConstFieldAssignment fieldName fieldValue _) =
        pretty fieldName <+> pretty "=" <+> pprint fieldValue

instance RTPrinter (ConstStructInitializer a) where

    pprint (ConstStructInitializer fieldAssignments _) =
        let ppAssignments = map pprint fieldAssignments
        in
            braces' $ (indentTab . align) (vsep (punctuate comma ppAssignments))

instance RTPrinter (RTElement a) where

    pprint (RTTransaction transName firstStep _) = 
        pretty "transaction" <+> pretty transName <+> pretty "=" <> line <>
            (indentTab . align) (pprint firstStep) <> pretty ";" <> line
    pprint (RTSituation sitName initializer _) =
        pretty "rts" <+> pretty sitName <+> pretty "=" <+> pprint initializer <> pretty ";" <> line

instance RTPrinter (RTTransStep a) where

    pprint (RTTransStepAction stepName taskName actionName pathName nextStep _) =
        case nextStep of 
            Just step -> 
                pretty stepName <> pretty "#" <> pretty taskName <> pretty "." <> pretty actionName <> pretty "::" <> pretty pathName 
                <> line <> pretty "->" <+> pprint step
            Nothing -> pretty stepName <> pretty "#" <> pretty taskName <> pretty "." <> pretty actionName <> pretty "::" <> pretty pathName
            
    pprint (RTTransStepMuticast steps _) =
        let ppSteps = map pprint steps
        in
            pretty "{|" <> line <> (indentTab . align) (vsep (punctuate comma ppSteps)) <> line <> pretty "|}"
    pprint (RTTransStepConditional branches _) =
        let ppBranches = map pprintBranch branches
            pprintBranch (condExpr, step) =
                pprint condExpr <> pretty "!" <> pprint step
        in
            angles (align (vsep (punctuate comma ppBranches)))

runRTPrinter :: [RTElement a] -> Text
runRTPrinter wceps = render $ line <> vsep (map pprint wceps)