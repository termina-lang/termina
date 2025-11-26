module EFP.Schedulability.TransPath.Printer where
import Generator.Utils
import EFP.Schedulability.TransPath.AST
import Prettyprinter
import Utils.Annotations
import Text.Parsec.Pos
import ControlFlow.BasicBlocks.AST (Expression)
import Data.Text (Text)

class WCEPathPrinter a where
    pprint :: a -> DocStyle

(<::>) :: DocStyle -> DocStyle -> DocStyle
d1 <::> d2 = d1 <> pretty "::" <> d2

indentTab :: DocStyle -> DocStyle
indentTab = indent 4

braces' :: DocStyle -> DocStyle
braces' b = braces (line <> b <> line)

instance WCEPathPrinter SourcePos where
    pprint pos = pretty (sourceLine pos) <> colon <> pretty (sourceColumn pos)

instance WCEPathPrinter (Expression a) where
    pprint _ = pretty "<expr>"

instance WCEPathPrinter Location where
    pprint (Position _ startPos endPos) =
        pretty "@" <> parens (pprint startPos <> comma <> pprint endPos)
    pprint Builtin = pretty "@builtin"
    pprint Internal = pretty "@internal"

instance WCEPathPrinter (WCEPathBlock a) where

    pprint (WCEPRegularBlock loc) =
        pretty "block" <> pprint loc
    pprint (WCEPReturn loc) =
        pretty "return" <> pprint loc
    pprint (WCEPContinue actionName loc) =
        pretty "continue" <> parens (pretty actionName) <> pprint loc
    pprint (WCEPReboot loc) =
        pretty "reboot" <> pprint loc
    pprint (WCEPSystemCall sysCallName loc) =
        pretty "syscall" <> parens (pretty sysCallName) <> pprint loc
    pprint (WCEPathCondIf blocks loc) =
        let ppBlocks = fmap pprint blocks
        in
            pretty "if" <> pprint loc <> braces' ((indentTab . align) (vsep (punctuate comma ppBlocks)))
    pprint (WCEPathCondElseIf blocks loc) =
        let ppBlocks = fmap pprint blocks
        in
            pretty "elif" <> pprint loc <> braces' ((indentTab . align) (vsep (punctuate comma ppBlocks)))
    pprint (WCEPathCondElse blocks loc) =
        let ppBlocks = fmap pprint blocks
        in
            pretty "else" <> pprint loc <> braces' ((indentTab . align) (vsep (punctuate comma ppBlocks)))
    pprint (WCEPathForLoop initExpr finalExpr blocks loc) =
        let ppBlocks = fmap pprint blocks
            ppInitExpr = pprint initExpr
            ppFinalExpr = pprint finalExpr
        in 
            pretty "for" <> parens (ppInitExpr <> pretty " to " <> ppFinalExpr) <> pprint loc <>
                braces' ((indentTab . align) (vsep (punctuate comma ppBlocks)))
    pprint (WCEPathMatchCase blocks loc) =
        let ppBlocks = fmap pprint blocks
        in
            pretty "case" <> pprint loc <> braces' ((indentTab . align) (vsep (punctuate comma ppBlocks))) 
    pprint (WCEPSendMessage portName loc) =
        pretty "send" <> parens (pretty portName) <> pprint loc
    pprint (WCEPathMemberFunctionCall funcName argExprs loc) =
        let ppArgExprs = fmap pprint argExprs
        in
            case ppArgExprs of
                [] -> pretty "call" <> parens (pretty funcName) <> pprint loc
                _  ->
                    pretty "call" <> parens (pretty funcName <> comma <> hsep (punctuate comma ppArgExprs)) <> pprint loc
    pprint (WCEPProcedureInvoke portName procName argExprs loc) =
        let ppArgExprs = fmap pprint argExprs
        in
            case ppArgExprs of
                [] -> pretty "invoke" <> parens (pretty portName <> pretty "." <> pretty procName) <> pprint loc
                _  ->
                    pretty "invoke" <> parens (pretty portName <> pretty "." <> pretty procName <> comma <> hsep (punctuate comma ppArgExprs)) <> pprint loc
    pprint (WCEPAllocBox portName loc) =
        pretty "alloc" <> parens (pretty portName) <> pprint loc
    pprint (WCEPFreeBox portName loc) =
        pretty "free" <> parens (pretty portName) <> pprint loc

instance WCEPathPrinter (TransactionalWCEPath a) where
    pprint (TransactionalWCEPath taskName actionName pathName blocks) =
        let pBlocks = map ((<> comma) . pprint) blocks in
        pretty "twcep" <+> pretty taskName <::> pretty actionName <::> pretty pathName <+> pretty "=" <+> 
            braces' ((indentTab . align) (vsep pBlocks)) <> line

runWCEPathPrinter :: [TransactionalWCEPath a] -> Text
runWCEPathPrinter wceps = render $ line <> vsep (map pprint wceps)