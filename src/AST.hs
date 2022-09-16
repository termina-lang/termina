-- | Module where ASTs are defined.

module AST where


-- | Annotated AST
data AASTElem a
  -- | A task takes an `Identifier`, at least one parameter, a type and its body
  = Task Identifier Param (CompoundStmt a) a
  | Proc Identifier (Param, [Param]) BType (CompoundStmt a) a
  | Handler Identifier [Param] (CompoundStmt a) a
  | GlbDec Global a

-- | Identifiers as `String`
type Identifier = String
-- | Addresses as `String`
type Address = Integer

-- | Basic Types
data BType
  = UInt8 | UInt16 | UInt32 | UInt64
  | Int8 | Int16 | Int32 | Int64
  | Bool | Char

----------------------------------------
-- | Datatype representing Global Declarations
data Global
  = Atom BType Identifier
  | Volatile BType Identifier Address
  | MsgQueue BType Int Identifier
  | Pool BType Int Identifier

----------------------------------------
newtype Param = Param { unParam :: (Identifier, BType)}
paramIdent :: Param -> Identifier
paramIdent = fst . unParam
paramBType :: Param -> BType
paramBType = snd . unParam

data CompoundStmt a = Compound
  {localDecl :: [LocalDecl a]
  , stms :: [Stmt a]
  }

newtype LocalDecl a = LDecl {unLDecl :: (Identifier, BType, Const , a)}

data Const = B Bool | I Int | C Char

-- | Simple datatype enumerating some of the constructions of our language
data Stmt a
  = Assign
  | Conditional a
  | ForLoop a
  | Skip a

type AnnProgram a = [AASTElem a]

-- When annotations are just `()` we get a normal ASTs and Programs
type AST = AASTElem ()
type Program = AnnProgram ()

----------------------------------------
-- Extra Info

-- | Raw Data-types, Basic - Raw = {Bool, Char} declared as synonyms.
data RawType
  = RawUInt8 | RawUInt16 | RawUInt32 | RawUInt64
  | RawInt8 | RawInt16 | RawInt32 | RawInt64

basicToRaw :: BType -> RawType
basicToRaw Bool = RawInt8
basicToRaw Char = RawInt8
basicToRaw UInt8 = RawUInt8
basicToRaw UInt16 = RawUInt16
basicToRaw UInt32 = RawUInt32
basicToRaw UInt64 = RawUInt64
basicToRaw Int8 = RawInt8
basicToRaw Int16 = RawInt16
basicToRaw Int32 = RawInt32
basicToRaw Int64 = RawInt64
