module Generator.Makefile.Printer where
import Generator.Makefile.AST
import Prettyprinter
import Data.Text (Text)
import Utils.Printer


class MakePrint a where
  pprint :: a -> DocStyle

instance MakePrint Makefile where
  pprint (Makefile blocks) = vsep $ map (\s -> pprint s <> line) blocks

instance MakePrint MakeBlock where
  pprint (MakeBlock statements) = vsep $ map pprint statements

instance MakePrint MakeStatement where
  pprint (MVariable MSimple name cmd) = pretty name <> colon <> equals <> hsep (map pprint cmd)
  pprint (MVariable MRecursive name cmd) = pretty name <> equals <> hsep (map pprint cmd)
  pprint (MVariable MAppend name cmd) = pretty name <> pretty "+" <> equals <> hsep (map pprint cmd)
  pprint (MRule target deps cmd) = pretty target <> colon <+> hsep (map pretty deps) <> line <> vsep (map (\c -> pretty "\t" <> pprint c) cmd) <> line
  pprint (MInclude True file) = pretty "include" <+> pretty file
  pprint (MInclude False file) = pretty "-include" <+> pretty file

instance MakePrint MakeCommand where
  pprint (MakeCommand True cmd) = pretty "@" <> hsep (map pprint cmd)
  pprint (MakeCommand False cmd) = hsep (map pprint cmd)

instance MakePrint MFragment where
  pprint (MFragment s) = pretty s
  pprint (MFunction name args) = pretty "$" <> parens (pretty name <+> hsep (punctuate comma (map pprint args)))

runMakefilePrinter :: Makefile -> Text
runMakefilePrinter = render . pprint