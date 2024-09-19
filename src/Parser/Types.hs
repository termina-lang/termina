module Parser.Types where
import Utils.Annotations
import AST.Parser

{- | Type of the parsing annotations
This type is used to identify the annotations made on the different
elements of the AST. In this case, the annotations will only include
the position in the source file where the element is located.
-}

type ParserAnn = Location

----------------------------------------

type Expression = Expression' Object

type ReturnStmt = ReturnStmt' Expression
type BlockRet = BlockRet' Expression Object
type AnnASTElement = AnnASTElement' Expression Object
type FieldAssignment = FieldAssignment' Expression
type Global = Global' Expression

type TypeDef a = TypeDef' Expression Object a

type ClassMember = ClassMember' Expression Object

type MatchCase = MatchCase' Expression Object
type ElseIf = ElseIf' Expression Object
type Statement = Statement' Expression Object

type AnnotatedProgram = [AnnASTElement' Expression Object ParserAnn]
type Block a = Block' Expression Object a

type Module = Module' [String]
type TerminaModule = TerminaModule' Expression Object [String]
