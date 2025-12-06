module EFP.Schedulability.TransPath.Printer where
import Generator.Utils
import EFP.Schedulability.TransPath.AST
import Prettyprinter
import Text.Parsec.Pos
import Data.Text (Text)
import Data.Char
import Numeric

class WCEPathPrinter a where
    pprint :: a -> DocStyle

(<::>) :: DocStyle -> DocStyle -> DocStyle
d1 <::> d2 = d1 <> pretty "::" <> d2

indentTab :: DocStyle -> DocStyle
indentTab = indent 4

brackets' :: DocStyle -> DocStyle
brackets' b = brackets (line <> b <> line)

instance WCEPathPrinter SourcePos where
    pprint pos = pretty (sourceLine pos) <> colon <> pretty (sourceColumn pos)

instance WCEPathPrinter TInteger where

    pprint (TInteger i DecRepr) = pretty i
    pprint (TInteger i HexRepr) = pretty "0x" <> pretty (toUpper <$> showHex i "")
    pprint (TInteger i OctalRepr) = pretty "0" <> pretty (showOct i "")

instance WCEPathPrinter (ConstExpression a) where
    pprint (ConstInt intVal _) =
        pprint intVal
    pprint (ConstObject ident _) =
        pretty ident
    pprint (ConstBinOp op left right _) =
        let ppLeft = pprint left
            ppRight = pprint right
            ppOp = pretty (show op)
        in
            parens (ppLeft <+> ppOp <+> ppRight)

instance WCEPathPrinter BlockPosition where
    pprint (BlockPosition startLine startColumn endLine endColumn) =
        pretty "@" <> parens (pretty startLine <> colon <> pretty startColumn <> comma <> pretty endLine <> colon <> pretty endColumn)

instance WCEPathPrinter (WCEPathBlock a) where

    pprint (WCEPRegularBlock loc _) =
        pretty "block" <> pprint loc
    pprint (WCEPReturn loc _) =
        pretty "return" <> pprint loc
    pprint (WCEPContinue actionName loc _) =
        pretty "continue" <> parens (pretty actionName) <> pprint loc
    pprint (WCEPReboot loc _) =
        pretty "reboot" <> pprint loc
    pprint (WCEPSystemCall sysCallName argExprs loc _) =
        let ppArgExprs = fmap pprint argExprs
        in
            case ppArgExprs of
                [] -> pretty "syscall" <> parens (pretty sysCallName) <> pprint loc
                _  ->
                    pretty "syscall" <> parens (pretty sysCallName) <> parens (hsep (punctuate comma ppArgExprs)) <> pprint loc
    pprint (WCEPathCondIf blocks loc _) =
        let ppBlocks = fmap pprint blocks
        in
            pretty "if" <> pprint loc <> brackets' ((indentTab . align) (vsep (punctuate comma ppBlocks)))
    pprint (WCEPathCondElseIf blocks loc _) =
        let ppBlocks = fmap pprint blocks
        in
            pretty "elif" <> pprint loc <> brackets' ((indentTab . align) (vsep (punctuate comma ppBlocks)))
    pprint (WCEPathCondElse blocks loc _) =
        let ppBlocks = fmap pprint blocks
        in
            pretty "else" <> pprint loc <> brackets' ((indentTab . align) (vsep (punctuate comma ppBlocks)))
    pprint (WCEPathForLoop initExpr finalExpr blocks loc _) =
        let ppBlocks = fmap pprint blocks
            ppInitExpr = pprint initExpr
            ppFinalExpr = pprint finalExpr
        in 
            pretty "for" <> parens (ppInitExpr <> pretty ".." <> ppFinalExpr) <> pprint loc <>
                brackets' ((indentTab . align) (vsep (punctuate comma ppBlocks)))
    pprint (WCEPathMatchCase blocks loc _) =
        let ppBlocks = fmap pprint blocks
        in
            pretty "case" <> pprint loc <> brackets' ((indentTab . align) (vsep (punctuate comma ppBlocks))) 
    pprint (WCEPSendMessage portName loc _) =
        pretty "send" <> parens (pretty portName) <> pprint loc
    pprint (WCEPathMemberFunctionCall funcName argExprs loc _) =
        let ppArgExprs = fmap pprint argExprs
        in
            case ppArgExprs of
                [] -> pretty "call" <> parens (pretty funcName) <> pprint loc
                _  ->
                    pretty "call" <> parens (pretty funcName) <> parens (hsep (punctuate comma ppArgExprs)) <> pprint loc
    pprint (WCEPProcedureInvoke portName procName argExprs loc _) =
        let ppArgExprs = fmap pprint argExprs
        in
            case ppArgExprs of
                [] -> pretty "invoke" <> parens (pretty portName <> pretty "." <> pretty procName) <> pprint loc
                _  ->
                    pretty "invoke" <> parens (pretty portName <> pretty "." <> pretty procName) <> parens (hsep (punctuate comma ppArgExprs)) <> pprint loc
    pprint (WCEPAllocBox portName loc _) =
        pretty "alloc" <> parens (pretty portName) <> pprint loc
    pprint (WCEPFreeBox portName loc _) =
        pretty "free" <> parens (pretty portName) <> pprint loc

instance WCEPathPrinter (TransactionalWCEPath a) where
    pprint (TransactionalWCEPath taskName actionName pathName constParams blocks _) =
        let ppBlocks = map pprint blocks
            pParams = map pretty constParams
        in
        pretty "twcep" <+> pretty taskName <::> pretty actionName <::> pretty pathName <> parens (align (fillSep (punctuate comma pParams))) <+> pretty "=" <+> 
            brackets' ((indentTab . align) (vsep (punctuate comma ppBlocks))) <> line

runWCEPathPrinter :: [TransactionalWCEPath a] -> Text
runWCEPathPrinter wceps = render $ line <> vsep (map pprint wceps)