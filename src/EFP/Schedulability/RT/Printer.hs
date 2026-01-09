module EFP.Schedulability.RT.Printer where

import EFP.Schedulability.RT.Semantic.AST
import Prettyprinter
import Data.Text (Text)
import Data.Char
import Numeric
import Utils.Printer
import qualified Data.Map.Strict as M

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

pprintDeadline :: (Identifier, Double) -> DocStyle
pprintDeadline (pathId, deadlineExpr) =
    pretty pathId <+> pretty "=" <+> pretty deadlineExpr

instance RTPrinter (RTEvent a) where

    pprint (RTEventBursty eventId emitterId transId interval arrivals deadlines _) =
        pretty "bursty" <+> pretty eventId <+> pretty "=" <+>
            braces' ((indentTab . align) (vsep (punctuate comma [
                pretty "emitter" <+> pretty "=" <+> pretty emitterId,
                pretty "transaction" <+> pretty "=" <+> pretty transId,
                pretty "interval" <+> pretty "=" <+> pprint interval,
                pretty "arrivals" <+> pretty "=" <+> pprint arrivals,
                pretty "deadlines" <+> braces' ((indentTab . align) (vsep (map pprintDeadline (M.toList deadlines))))
            ])))
    pprint (RTEventPeriodic eventId emitterId transId deadlines _) =
        pretty "periodic" <+> pretty eventId <+> pretty "=" <+>
            braces' ((indentTab . align) (vsep (punctuate comma [
                pretty "emitter" <+> pretty "=" <+> pretty emitterId,
                pretty "transaction" <+> pretty "=" <+> pretty transId,
                pretty "deadlines" <+> braces' ((indentTab . align) (vsep (map pprintDeadline (M.toList deadlines))))
            ])))

instance RTPrinter (RTElement a) where

    pprint (RTTransaction transName firstStep _) =
        pretty "transaction" <+> pretty transName <+> pretty "=" <> line <>
            (indentTab . align) (pprint firstStep) <> pretty ";" <> line
    pprint (RTSituation sitName evMap _) =
        pretty "rts" <+> pretty sitName <+> pretty "=" <+> 
            braces' ((indentTab . align) (vsep (punctuate comma (map pprint (M.elems evMap)))))
            <> pretty ";" <> line

instance RTPrinter (RTTransStep a) where

    pprint (RTTransStepAction stepName taskName actionName pathName nextStep _) =
        pretty stepName <> pretty "#" <> pretty taskName <> pretty "." <> pretty actionName <> pretty "::" <> pretty pathName
        <> line <> pretty "->" <+> pprint nextStep
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
    pprint (RTTransStepEnd stepName _) =
        pretty stepName <> pretty "#" <> pretty "end"

runRTPrinter :: [RTElement a] -> Text
runRTPrinter wceps = render $ line <> vsep (map pprint wceps)