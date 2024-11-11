module Generator.Makefile.Printer where
import Generator.Utils
import Generator.Makefile.AST
import Prettyprinter
import Data.Text (Text)


class MakePrint a where
  pprint :: a -> DocStyle

instance MakePrint Makefile where
  pprint (Makefile stmts) = vsep $ map pprint stmts

instance MakePrint MakeStatement where
  pprint (MVariable name cmd) = pretty name <> colon <> equals <> hsep (map pprint cmd) <> line
  pprint (MRule target deps cmd) = pretty target <> colon <+> hsep (map pretty deps) <> line <> vsep (map (\c -> pretty "\t" <> pprint c) cmd) <> line
  pprint (MInclude file) = pretty "include" <+> pretty file <> line

instance MakePrint MakeCommand where
  pprint (MakeCommand True cmd) = pretty "@" <> hsep (map pprint cmd)
  pprint (MakeCommand False cmd) = hsep (map pprint cmd)

instance MakePrint Fragment where
  pprint (MFragment s) = pretty s
  pprint (MSubstitution s) = pretty "$(" <> pretty s <> pretty ")"

runMakefilePrinter :: Makefile -> Text
runMakefilePrinter = render . pprint