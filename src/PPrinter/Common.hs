module PPrinter.Common where

import AST
import Text.Parsec.Pos

import Prettyprinter
import Prettyprinter.Render.Terminal

type DocStyle = Doc AnsiStyle

class PrinterAnnotation a where
  location :: a -> SourcePos
  typeSpecifier :: a -> TypeSpecifier

-- | Type of the pretty printers
type Printer a b =
  (b -> DocStyle)
  -- ^ Function that pretty prints an annotation BEFORE printing the construct
  -> (b -> DocStyle)
  -- ^ Function that pretty prints an annotation AFTER printing the construct
  -> a b
  -- ^ The annotated element to pretty print
  -> DocStyle

braces' :: DocStyle -> DocStyle
braces' b = braces (line <> b <> line)

indentTab :: DocStyle -> DocStyle
indentTab = indent 4

-- | This function is used to create the names of temporal variables
-- and symbols.
namefy :: String -> String
namefy = ("__" ++)

--------------------------------------------------------------------------------
-- C pretty keywords
-- Creturn, C_typedef, C_enum, C_struct, C_union :: DocStyle
returnC, typedefC, enumC, structC, unionC, voidC :: DocStyle
returnC = pretty "return"
typedefC = pretty "typedef"
enumC = pretty "enum"
structC = pretty "struct"
unionC = pretty "union"
voidC = pretty "void";

-- C pretty unsigned integer types
uint8C, uint16C, uint32C, uint64C :: DocStyle
uint8C = pretty "uint8_t"
uint16C = pretty "uint16_t"
uint32C = pretty "uint32_t"
uint64C = pretty "uint64_t"

-- C pretty signed integer types
int8C, int16C, int32C, int64C :: DocStyle
int8C = pretty "int8_t"
int16C = pretty "int16_t"
int32C = pretty "int32_t"
int64C = pretty "int64_t"

-- C pretty char
charC :: DocStyle
charC = pretty "char"

-- C attribute pragma
attribute :: DocStyle
attribute = pretty "__attribute__"

-- | Termina's pretty builtin types
pool, msgQueue, mutex :: DocStyle
pool = pretty "__termina_pool_t"
msgQueue = pretty "__termina_msg_queue_id_t"
mutex = pretty "__termina_mutex_id_t"

enumIdentifier :: String -> DocStyle
enumIdentifier identifier = pretty "__enum_" <> pretty identifier

enumVariantsField :: DocStyle
enumVariantsField = pretty "__variant"

ppRootType :: TypeSpecifier -> DocStyle
ppRootType UInt8 = uint8C
ppRootType UInt16 = uint16C
ppRootType UInt32 = uint32C
ppRootType UInt64 = uint64C
ppRootType Int8 = int8C
ppRootType Int16 = int16C
ppRootType Int32 = int32C
ppRootType Int64 = int64C
ppRootType Bool = uint8C
ppRootType Char = charC
ppRootType (DefinedType typeIdentifier) = pretty typeIdentifier
ppRootType (Vector ts _) = ppRootType ts
ppRootType (Option ts) = case ts of
  Option ts' -> ppRootType ts'
  Vector ts' _ -> ppRootType ts'
  Reference ts' -> ppRootType ts'
  DynamicSubtype ts' -> ppRootType ts'
  Unit -> error "unsupported type"
  _ -> ppRootType ts <+> pretty "*"
ppRootType (Pool _ _) = pool
ppRootType (Reference ts) = case ts of
  Option ts' -> ppRootType ts'
  Vector ts' _ -> ppRootType ts'
  Reference ts' -> ppRootType ts'
  DynamicSubtype ts' -> ppRootType ts'
  Unit -> error "unsupported type"
  _ -> ppRootType ts <+> pretty "*"
ppRootType (MsgQueue _ _) = msgQueue
ppRootType (DynamicSubtype ts) = case ts of
  Option ts' -> ppRootType ts'
  Vector ts' _ -> ppRootType ts'
  Reference ts' -> ppRootType ts'
  DynamicSubtype ts' -> ppRootType ts'
  Unit -> error "unsupported type"
  _ -> ppRootType ts <+> pretty "*"
ppRootType Unit = error "unsupported type"

ppSize :: TypeSpecifier -> DocStyle
ppSize (Vector ts (K size)) = ppSize ts <> brackets (pretty size)
ppSize _ = emptyDoc

ppDeclaration :: Identifier -> TypeSpecifier -> DocStyle
ppDeclaration identifier ts = ppRootType ts <+> pretty identifier <> ppSize ts

ppParameter :: Parameter -> DocStyle
ppParameter (Parameter identifier ts) = ppDeclaration identifier ts

ppPrinterFunctionDeclaration ::
    Identifier -> -- ^ function identifier (name)
    [Parameter] -> -- ^ list of parameters (possibly empty)
    Maybe TypeSpecifier -> -- ^ type of the return value (optional)
    DocStyle
ppPrinterFunctionDeclaration identifier parameters rTS =
  maybe voidC ppRootType rTS <+> 
    pretty identifier <> 
      parens (align (fillSep (punctuate comma (map ppParameter parameters))))

ppModifier :: Modifier a -> DocStyle
ppModifier (Modifier identifier (Just (KC (I _ integer) _))) = pretty identifier <> parens (pretty integer)
ppModifier (Modifier identifier (Just (KC (B True) _))) = pretty identifier <> parens (pretty "1")
ppModifier (Modifier identifier (Just (KC (B False) _))) = pretty identifier <> parens (pretty "0")
ppModifier (Modifier identifier (Just (KC (C char) _))) = pretty identifier <> parens (pretty "'" <> pretty char <> pretty "'")
ppModifier (Modifier identifier Nothing) = pretty identifier