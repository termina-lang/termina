{-# LANGUAGE TupleSections #-}
-- | Module dedicated to Parsing

module Parser.Parsing where

import           AST.Parser                  hiding (blockRet)
-- Importing position from Parsec
import           Text.Parsec.Pos
-- Importing parser combinators
import           Text.Parsec
import           Text.Parsec.String
import Text.Parsec.Char
-- Importing tokenizer
import qualified Text.Parsec.Expr     as Ex
import qualified Text.Parsec.Language as Lang
import qualified Text.Parsec.Token    as Tok

import           Data.Functor
import Text.Parsec.Expr
import qualified Data.List as L
import Control.Monad

{- | Type of the parsing annotations

This type is used to identify the annotations made on the different
elements of the AST. In this case, the annotations will only include
the position in the source file where the element is located.

-}

data Annotation =
  Position SourcePos -- ^ Source code position
  | Internal
  -- ^ Internal error position. Used for debugging, internals shoulnd't happen
  deriving Show

----------------------------------------
-- Lexer
----------------------------------------

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef
  where
    reservedNames =
      -- Basic Types
      ["u8","u16","u32","u64"
      ,"i8","i16","i32","i64"
      ,"bool","char"]
      ++ -- Polymorphic Types
      ["MsgQueue", "Pool", "Option"]
      ++ -- Struct and Union Types
      ["struct","union"]
      ++ -- Dynamic Subtyping
      ["dyn"]
      ++ -- Fixed Location Subtyping
      ["loc"]
      ++ -- Port Subtyping
      ["port"]
      ++ -- Private reference typing
      ["&priv"]
      ++ -- Global declarations
      ["task", "function", "handler", "resource", "const"]
      ++ -- Stmt
      ["var", "match", "for", "if", "else", "return", "while"]
      ++ -- Free
      ["free"]
      ++ -- Constants
      ["true", "false"]
      ++ -- Modules
      ["import"]
      ++ -- Class methods
      ["procedure", "viewer", "method"]

    langDef =
      Lang.emptyDef{ Tok.commentStart = "/*"
                   , Tok.commentEnd = "*/"
                   , Tok.commentLine = "//"
                   , Tok.nestedComments = False
                   -- | Here we define that identifiers being with a letter
                   , Tok.identStart = letter
                   -- | Rest of identifiers accepted characters
                   , Tok.identLetter = alphaNum <|> char '_'
                   -- | Operators begin with
                   , Tok.opStart = oneOf ".*/+-<>=!&|^,;(["
                   , Tok.reservedNames = reservedNames
                   , Tok.reservedOpNames = [
                       "." -- MemberAccess
                      ,"*" -- Multiplication
                      ,"/" -- Division
                      ,"+" -- Addition
                      ,"-" -- Substraction
                      ,"<<" -- BitwiseLeftShift
                      ,">>" -- BitwiseRightShift
                      ,"<" -- RelationalLT
                      ,"<=" -- RelationalLTE
                      ,">" -- RelationalGT
                      ,">=" -- RelationalGTE
                      ,"==" -- RelationalEqual
                      ,"!=" -- RelationalNotEqual
                      ,"&" -- BitwiseAnd / reference creation
                      ,"|" -- BitwiseOr
                      ,"^" -- BitwiseXor
                      ,"&&" -- LogicalAnd
                      ,"||" -- LogicalOr
                      ,"#" -- Attribute
                      ,":" -- Type annotation
                      ,"::" -- Enum variant
                      ,"=" -- Assignment
                      ,"->" -- Function return type
                      ,"=>" -- Match case
                      ,"[" -- Vector init
                      ,"]" -- Vector init
                      ,"{" -- Field values assignments
                      ,"}" -- Field values assignments
                      ,"(" -- Parens
                      ,")" -- Parens
                      ,".." -- Vector slice and for loop range
                      ,"&mut" -- Mutable reference creation
                      ,"@" -- Field address assignment
                      ,"<-" -- Field connection assignment
                    ]
                   -- | Is the language case sensitive? It should be
                   , Tok.caseSensitive = True
                   }

-- Lexemes

wspcs :: Parsec String () ()
wspcs = Tok.whiteSpace lexer

braces :: Parsec String () a -> Parsec String () a
braces = Tok.braces lexer

brackets :: Parsec String () a -> Parsec String () a
brackets = Tok.brackets lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

angles :: Parser a -> Parser a
angles = Tok.angles lexer

comma :: Parser String
comma = Tok.comma lexer

semi :: Parser String
semi = Tok.semi lexer

dot :: Parser String
dot = Tok.dot lexer

stringLit :: Parser String
stringLit = Tok.stringLiteral lexer

charLit :: Parser Char
charLit = Tok.charLiteral lexer

reserved :: String -> Parsec String () ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parsec String () ()
reservedOp = Tok.reservedOp lexer

identifierParser :: Parser String
identifierParser = Tok.identifier lexer

hexa :: Parser Integer
hexa = char '0' >> Tok.hexadecimal lexer

tails :: [a] -> [[a]]
tails = L.tails

-- | Parser for natural numbers (positive integers + 0)
-- This parser is used solely when declaring a new vector to specify
-- its dimension, i.e. the parser checks that we specify the dimension
-- as a natural number.
natural :: Parser Integer
natural = Tok.natural lexer

-- | Parser for integer numbers
-- This parser is used when defining regular integer literals
-- TODO: Change it to support Termina's hex format.
integer :: Parser Integer
integer = Tok.integer lexer

----------------------------------------
-- Parser
----------------------------------------

-- | Types
typeSpecifierParser :: Parser TypeSpecifier
typeSpecifierParser =
  msgQueueParser
  <|> poolParser
  <|> vectorParser
  <|> mutableReferenceParser
  <|> referenceParser
  <|> dynamicSubtypeParser
  <|> locationSubtypeParser
  <|> portSubtypeParser
  <|> optionParser
  <|> (DefinedType <$> identifierParser)
  <|> (reserved "u8" >> return UInt8)
  <|> (reserved "u16" >> return UInt16)
  <|> (reserved "u32" >> return UInt32)
  <|> (reserved "u64" >> return UInt64)
  <|> (reserved "i8" >> return Int8)
  <|> (reserved "i16" >> return Int16)
  <|> (reserved "i32" >> return Int32)
  <|> (reserved "i64" >> return Int64)
  <|> (reserved "bool" >> return Bool)
  <|> (reserved "char" >> return Char)

parameterParser :: Parser Parameter
parameterParser = do
  identifier <- identifierParser
  reservedOp ":"
  Parameter identifier <$> typeSpecifierParser

-- | Parser for a field value assignments expression
-- This expression is used to create annonymous structures to serve as right
-- hand side of an assignment expression.
-- Examples of this expression:
-- { field0 = 0 : u32, field1 = 0 : u16 } : StructIdentifier
fieldAssignmentsExpressionParser :: Parser (Expression Annotation)
fieldAssignmentsExpressionParser = do
    assignments <- braces (sepBy (try flValues <|> try flAddresses <|> flConnection) comma)
    p <- getPosition
    _ <- reservedOp ":"
    identifier <- identifierParser
    return $ FieldAssignmentsExpression identifier assignments (Position p)
    where
      flValues = do
            identifier <- identifierParser
            _ <- reservedOp "="
            FieldValueAssignment identifier <$> expressionParser
      flAddresses = do
            identifier <- identifierParser
            _ <- reservedOp "@"
            FieldAddressAssignment identifier <$> hexa
      flConnection = do
            identifier <- identifierParser
            _ <- reservedOp "<-"
            FieldPortConnection identifier <$> identifierParser

-- | Parser for an element modifier
-- A modifier is of the form:
-- #[identifier(expression)]
-- where:
-- - identifier: is a mandatory identifier that names the attribute or modifier
-- - expresssion: is an optional expression between parenthesis.AST
-- Examples of a modifier:
-- #[priority(5)] for a task
-- #[packed] for a struct or union

modifierParser :: Parser Modifier
modifierParser = do
  _ <- reservedOp "#"
  _ <- reservedOp "["
  identifier <- identifierParser
  initializer <- optionMaybe (parens constExprParser')
  _ <- reservedOp "]"
  return $ Modifier identifier (KC <$> initializer)

msgQueueParser :: Parser TypeSpecifier
msgQueueParser = do
  reserved "MsgQueue"
  _ <- reservedOp "<"
  typeSpecifier <- typeSpecifierParser
  _ <- semi
  size <- K <$> natural
  _ <- reservedOp ">"
  return $ MsgQueue typeSpecifier size

poolParser :: Parser TypeSpecifier
poolParser = do
  reserved "Pool"
  _ <- reservedOp "<"
  typeSpecifier <- typeSpecifierParser
  _ <- semi
  size <- K <$> natural
  _ <- reservedOp ">"
  return $ Pool typeSpecifier size

vectorParser :: Parser TypeSpecifier
vectorParser = do
  _ <- reservedOp "["
  typeSpecifier <- typeSpecifierParser
  _ <- semi
  size <- KC <$> constExprParser'
  _ <- reservedOp "]"
  return $ Vector typeSpecifier size

referenceParser :: Parser TypeSpecifier
referenceParser = reservedOp "&" >> Reference Immutable <$> typeSpecifierParser

mutableReferenceParser :: Parser TypeSpecifier
mutableReferenceParser = reservedOp "&mut" >> Reference Mutable <$> typeSpecifierParser

dynamicSubtypeParser :: Parser TypeSpecifier
dynamicSubtypeParser = reservedOp "dyn" >> DynamicSubtype <$> typeSpecifierParser

locationSubtypeParser :: Parser TypeSpecifier
locationSubtypeParser = reservedOp "loc" >> Location <$> typeSpecifierParser

portSubtypeParser :: Parser TypeSpecifier
portSubtypeParser = reservedOp "port" >> Port <$> typeSpecifierParser

optionParser :: Parser TypeSpecifier
optionParser = reserved "Option" >> Option <$> angles typeSpecifierParser

-- Expression Parser
expressionParser' :: Parser (Expression Annotation)
expressionParser' = buildPrattParser -- New parser
    [[castingPostfix]
    ,[binaryInfix "*" Multiplication Ex.AssocLeft,
      binaryInfix "/" Division Ex.AssocLeft]
    ,[binaryInfix "+" Addition Ex.AssocLeft,
      binaryInfix "-" Subtraction Ex.AssocLeft]
    ,[binaryInfix "<<" BitwiseLeftShift Ex.AssocLeft,
      binaryInfix ">>" BitwiseRightShift Ex.AssocLeft]
    ,[binaryInfix "<" RelationalLT Ex.AssocLeft,
      binaryInfix "<=" RelationalLTE Ex.AssocLeft,
      binaryInfix ">" RelationalGT Ex.AssocLeft,
      binaryInfix ">=" RelationalGTE Ex.AssocLeft]
    ,[binaryInfix "==" RelationalEqual Ex.AssocLeft,
      binaryInfix "!=" RelationalNotEqual Ex.AssocLeft]
    ,[binaryInfix "&" BitwiseAnd Ex.AssocLeft]
    ,[binaryInfix "|" BitwiseOr Ex.AssocLeft]
    ,[binaryInfix "^" BitwiseXor Ex.AssocLeft]
    ,[binaryInfix "&&" LogicalAnd Ex.AssocLeft]
    ,[binaryInfix "||" LogicalOr Ex.AssocLeft]
    ]
    expressionTermParser
  where binaryInfix s f = Ex.Infix (do
          _ <- reservedOp s
          p <- getPosition
          return $ \l r -> BinOp f l r (Position p))
        castingPostfix = Ex.Postfix (do
          _ <- reserved "as"
          p <- getPosition
          typeSpecificer <- typeSpecifierParser
          return $ \parent -> Casting parent typeSpecificer (Position p))

functionCallParser :: Parser (Expression Annotation)
functionCallParser =
  FunctionExpression
  <$> identifierParser
  <*> parens (sepBy (try expressionParser) comma)
  <*> (Position <$> getPosition)


optionVariantExprParser :: Parser (Expression Annotation)
optionVariantExprParser =
  (do
    p <- getPosition
    _ <- reserved "None"
    return $ OptionVariantExpression None (Position p)) <|>
  (do
    p <- getPosition
    _ <- reserved "Some"
    someExpr <- parens expressionParser
    return $ OptionVariantExpression (Some someExpr) (Position p))

enumVariantExprParser :: Parser (Expression Annotation)
enumVariantExprParser = do
  p <- getPosition
  enum <- identifierParser
  _ <- reservedOp "::"
  variant <- identifierParser
  parameterList <-
    option [] (parens (sepBy (try expressionParser) comma))
  return $ EnumVariantExpression enum variant parameterList (Position p)

expressionParser :: Parser (Expression Annotation)
expressionParser = try optionVariantExprParser
  <|> try enumVariantExprParser
  <|> try mutableReferenceExprParser
  <|> referenceExprParser
  <|> vectorInitParser
  <|> fieldAssignmentsExpressionParser
  <|> expressionParser'

mutableReferenceExprParser :: Parser (Expression Annotation)
mutableReferenceExprParser = do
  p <- getPosition
  _ <- reservedOp "&mut"
  object <- objectParser
  return $ ReferenceExpression Mutable object (Position p)

referenceExprParser :: Parser (Expression Annotation)
referenceExprParser = do
  p <- getPosition
  _ <- reservedOp "&"
  object <- objectParser
  return $ ReferenceExpression Immutable object (Position p)

expressionTermParser :: Parser (Expression Annotation)
expressionTermParser = constExprParser
  <|> try functionCallParser
  <|> try accessObjectParser
  <|> parensExprParser

parensExprParser :: Parser (Expression Annotation)
parensExprParser = parens expressionParser

parensObjectParser :: Parser (Object  Annotation)
parensObjectParser = parens objectParser

----------------------------------------
-- Object Parsing

-- Object term parser.
objectTermParser :: Parser (Object Annotation)
objectTermParser = Variable <$> identifierParser <*> (Position <$> getPosition)
  <|> parensObjectParser

-- Expression parser
-- This parser is a variation of the original parser that allows us to chain
-- two or more unary expressions together. This is useful when parsing expressions
-- such as vector[0][1], where the postfix operator [] is used twice. This code
-- has been directly extracted from this StackOverflow answer:
-- https://stackoverflow.com/questions/33214163/parsec-expr-repeated-prefix-with-different-priority/33534426#33534426
-- We now use it to parse expressions and object operations.
buildPrattParser :: (Stream s m t)
                      => OperatorTable s u m a
                      -> ParsecT s u m a
                      -> ParsecT s u m a
buildPrattParser table termP = parser precs where
  precs = reverse table
  prefixP = choice prefixPs <|> termP where
    prefixPs = do
      precsR@(ops:_) <- tails precs
      Prefix opP <- ops
      return $ opP <*> parser precsR
  infixP precs' lhs = choice infixPs <|> pure lhs where
    infixPs = do
      precsR@(ops:precsL) <- tails precs'
      op <- ops
      p <- case op of
        Infix opP assoc -> do
          let p precs'' = opP <*> pure lhs <*> parser precs''
          return $ case assoc of
            AssocNone  -> error "Non associative operators are not supported"
            AssocLeft  -> p precsL
            AssocRight -> p precsR
        Postfix opP ->
          return $ opP <*> pure lhs
        Prefix _ -> mzero
      return $ p >>= infixP precs'
  parser precs' = prefixP >>= infixP precs'

objectParser :: Parser (Object Annotation)
objectParser = objectParser' objectTermParser
  where
    objectParser'
      = buildPrattParser -- New parser
      [[dereferenceMemberAccessPostfix, memberAccessPostfix, vectorOpPostfix]
      ,[dereferencePrefix]]
    vectorOpPostfix
      = Ex.Postfix (try (do
            _ <- reservedOp "["
            low <- KC <$> constExprParser'
            _ <- reservedOp ".."
            up <- KC <$> constExprParser'
            _ <- reservedOp "]"
            p <- getPosition
            return $ \parent ->  VectorSliceExpression parent low up (Position p)
          ) <|> (do
            index <- brackets expressionParser
            p <- getPosition
            return $ \parent ->  VectorIndexExpression parent index (Position p)
          ))
    dereferenceMemberAccessPostfix
      = Ex.Postfix (do
      _ <- reservedOp "->"
      p <- getPosition
      member <- identifierParser
      return $ \parent ->   DereferenceMemberAccess parent member (Position p))
    memberAccessPostfix
      = Ex.Postfix (do
      _ <- reservedOp "."
      p <- getPosition
      member <- identifierParser
      return $ \parent ->  MemberAccess parent member (Position p))
    dereferencePrefix
      = Ex.Prefix (do
      p <- getPosition
      _ <- reservedOp "*"
      return $ flip Dereference (Position p))
----------------------------------------

accessObjectParser :: Parser (Expression Annotation)
accessObjectParser = accessObjectParser' (AccessObject <$> objectTermParser)
  where
    accessObjectParser'
      = buildPrattParser -- New parser
      [[dereferenceMemberAccessPostfix, memberAccessPostfix, vectorOpPostfix]
      ,[dereferencePrefix]]
    vectorOpPostfix
      = Ex.Postfix (try (do
            _ <- reservedOp "["
            low <- KC <$> constExprParser'
            _ <- reservedOp ".."
            up <- KC <$> constExprParser'
            _ <- reservedOp "]"
            p <- getPosition
            return $ \parent -> case parent of
              AccessObject obj -> (AccessObject (VectorSliceExpression obj low up (Position p)))
              _ -> error "Unexpected member access to a non object"
          ) <|> (do
            index <- brackets expressionParser
            p <- getPosition
            return $ \parent -> case parent of
              AccessObject obj -> (AccessObject (VectorIndexExpression obj index (Position p)))
              _ -> error "Unexpected member access to a non object"
          ))
    dereferenceMemberAccessPostfix
      = Ex.Postfix (do
      _ <- reservedOp "->"
      p <- getPosition
      member <- identifierParser
      return (\parent -> case parent of
        AccessObject obj -> (AccessObject (DereferenceMemberAccess obj member (Position p)))
        _ -> error "Unexpected member access to a non object"))
    memberAccessPostfix
      = Ex.Postfix (do
      _ <- reservedOp "."
      p <- getPosition
      member <- identifierParser
      params <- optionMaybe (parens (sepBy (try expressionParser) comma))
      return (\parent -> case parent of
        AccessObject obj ->
          maybe (AccessObject (MemberAccess obj member (Position p))) (flip (MemberFunctionAccess obj member) (Position p)) params
        _ -> error "Unexpected member access to a non object"))
    dereferencePrefix
      = Ex.Prefix (do
      p <- getPosition
      _ <- reservedOp "*"
      return (\parent -> case parent of
        AccessObject obj -> (AccessObject (Dereference obj (Position p)))
        _ -> error "Unexpected member access to a non object"))

vectorInitParser :: Parser (Expression Annotation)
vectorInitParser = do
  _ <- reservedOp "["
  p <- getPosition
  value <- expressionParser
  _ <- semi
  size <- KC <$> constExprParser'
  _ <- reservedOp "]"
  return $ VectorInitExpression value size (Position p)

-- -- Task Definition

blockParser :: Parser (BlockRet Annotation)
blockParser = BlockRet  <$> many blockItemParser <*> returnStmtParser

returnStmtParser :: Parser (ReturnStmt Annotation)
returnStmtParser = do
  p <- getPosition
  _ <- reserved "return"
  ret <- optionMaybe expressionParser
  _ <- semi
  return $ ReturnStmt ret (Position p)

emptyReturn :: Parser ()
emptyReturn = returnStmtParser >>= maybe mempty (const (fail "Expected Empty return")) . returnExpression

functionParser :: Parser (AnnASTElement  Annotation)
functionParser = do
  modifiers <- many modifierParser
  p <- getPosition
  reserved "function"
  name <- identifierParser
  params <- parens (sepBy parameterParser comma)
  typeSpec <- optionMaybe (do
    reservedOp "->"
    typeSpecifierParser)
  blockRet <- braces blockParser
  return $ Function name params typeSpec blockRet modifiers (Position p)

constExprParser' :: Parser Const
constExprParser' = parseLitInteger <|> parseLitBool <|> parseLitChar
  where
    parseLitInteger =
      do
        num <- integer
        reservedOp ":"
        ty <- typeSpecifierParser
        return (I ty num)
    parseLitBool = (reserved "true" >> return (B True)) <|> (reserved "false" >> return (B False))
    parseLitChar = C <$> charLit

constExprParser :: Parser (Expression Annotation)
constExprParser = flip Constant . Position  <$> getPosition <*> constExprParser'
    -- parseLitString = S <$> stringLit

mutableObjDeclarationParser :: Parser (Statement Annotation)
mutableObjDeclarationParser = do
  p <- getPosition
  reserved "var"
  name <- identifierParser
  reservedOp ":"
  ty <- typeSpecifierParser
  _ <- reservedOp "="
  initializer <-  expressionParser
  _ <- semi
  return $ Declaration name Mutable ty initializer (Position p)

immutableObjDeclarationParser :: Parser (Statement Annotation)
immutableObjDeclarationParser = do
  p <- getPosition
  reserved "let"
  name <- identifierParser
  reservedOp ":"
  ty <- typeSpecifierParser
  _ <- reservedOp "="
  initializer <-  expressionParser
  _ <- semi
  return $ Declaration name Immutable ty initializer (Position p)

singleExprStmtParser :: Parser (Statement Annotation)
singleExprStmtParser = do
  p <- getPosition
  expression <- expressionParser
  _ <- semi
  return $ SingleExpStmt expression (Position p)

freeStmtParser :: Parser (Statement Annotation)
freeStmtParser = reserved "free" >>
 Free <$> parens objectParser <*> (Position <$> getPosition)
 >>= \t -> semi >> return t


blockItemParser :: Parser (Statement Annotation)
blockItemParser
  =   try ifElseIfStmtParser
  <|> try mutableObjDeclarationParser
  <|> try immutableObjDeclarationParser
  <|> try assignmentStmtPaser
  <|> try forLoopStmtParser
  <|> try matchStmtParser
  <|> try freeStmtParser
  <|> singleExprStmtParser

assignmentStmtPaser :: Parser (Statement Annotation)
assignmentStmtPaser = do
  p <- getPosition
  lval <- objectParser
  _ <- reservedOp "="
  rval <- expressionParser
  _ <- semi
  return $ AssignmentStmt lval rval (Position p)

matchCaseParser :: Parser (MatchCase Annotation)
matchCaseParser = do
  reserved "case"
  p <- getPosition
  cons <- identifierParser
  args <- try (parens (sepBy identifierParser comma)) <|> return []
  reservedOp "=>"
  compound <- braces $ many blockItemParser
  return $ MatchCase cons args compound (Position p)

matchStmtParser :: Parser (Statement Annotation)
matchStmtParser = do
  reserved "match"
  p <- getPosition
  matchExpression <- expressionParser
  cases <- braces (many1 $ try matchCaseParser)
  return $ MatchStmt matchExpression cases (Position p)

elseIfParser :: Parser (ElseIf Annotation)
elseIfParser = do
  p <- getPosition
  _ <- reserved "else"
  _ <- reserved "if"
  expression <- expressionParser
  compound <- braces $ many blockItemParser
  return $ ElseIf expression compound (Position p)

ifElseIfStmtParser :: Parser (Statement Annotation)
ifElseIfStmtParser = do
  p <- getPosition
  _ <- reserved "if"
  expression <- expressionParser
  ifCompound <- braces $ many blockItemParser
  elseIfs <- many $ try elseIfParser
  elseCompound <- option [] (do
    _ <- reserved "else"
    braces $ many $ try blockItemParser)
  return $ IfElseStmt expression ifCompound elseIfs elseCompound (Position p)

forLoopStmtParser :: Parser (Statement Annotation)
forLoopStmtParser = do
  p <- getPosition
  _ <- reserved "for"
  identifier <- identifierParser
  reservedOp ":"
  ty <- typeSpecifierParser
  _ <- reserved "in"
  start <- expressionParser
  _ <- reservedOp ".."
  end <- expressionParser
  breakCondition <- optionMaybe (do
    reserved "while"
    expressionParser)
  compound <- braces $ many blockItemParser
  return $ ForLoopStmt identifier ty start end breakCondition compound (Position p)

taskDeclParser :: Parser (Global Annotation)
taskDeclParser = do
  modifiers <- many modifierParser
  p <- getPosition
  reserved "task"
  identifier <- identifierParser
  reservedOp ":"
  typeSpecifier <- typeSpecifierParser
  initializer <- optionMaybe (do
    reservedOp "="
    expressionParser)
  _ <- semi
  return $ Task identifier typeSpecifier initializer modifiers (Position p)

resourceDeclParser :: Parser (Global Annotation)
resourceDeclParser = do
  modifiers <- many modifierParser
  p <- getPosition
  reserved "resource"
  identifier <- identifierParser
  reservedOp ":"
  typeSpecifier <- typeSpecifierParser
  initializer <- optionMaybe (do
    reservedOp "="
    expressionParser)
  _ <- semi
  return $ Resource identifier typeSpecifier initializer modifiers (Position p)

handlerDeclParser :: Parser (Global Annotation)
handlerDeclParser = do
  modifiers <- many modifierParser
  p <- getPosition
  reserved "handler"
  identifier <- identifierParser
  reservedOp ":"
  typeSpecifier <- typeSpecifierParser
  initializer <- optionMaybe (do
    reservedOp "="
    expressionParser)
  _ <- semi
  return $ Handler identifier typeSpecifier initializer modifiers (Position p)

constDeclParser :: Parser (Global Annotation)
constDeclParser = do
  modifiers <- many modifierParser
  p <- getPosition
  reserved "const"
  identifier <- identifierParser
  reservedOp ":"
  typeSpecifier <- typeSpecifierParser
  _ <- reservedOp "="
  initializer <- expressionParser
  _ <- semi
  return $ Const identifier typeSpecifier initializer modifiers (Position p)

globalDeclParser :: Parser (AnnASTElement  Annotation)
globalDeclParser = do
  g <- taskDeclParser <|> resourceDeclParser <|> handlerDeclParser <|> constDeclParser
  return $ GlobalDeclaration g

typeDefintionParser :: Parser (AnnASTElement  Annotation)
typeDefintionParser = flip TypeDefinition
  -- First we get the position, at the beginning of the definition
  <$> (Position <$> getPosition)
  <*> (structDefinitionParser <|> enumDefinitionParser <|> classDefinitionParser)

fieldDefinitionParser :: Parser FieldDefinition
fieldDefinitionParser = do
  identifier <- identifierParser
  _ <- reservedOp ":"
  typeSpecifier <- typeSpecifierParser
  _ <- semi
  return $ FieldDefinition identifier typeSpecifier

structDefinitionParser :: Parser (TypeDef Annotation)
structDefinitionParser = do
  modifiers <- many modifierParser
  -- p <- getPosition
  reserved "struct"
  identifier <- identifierParser
  fields <- braces (many1 $ try fieldDefinitionParser)
  _ <- semi
  return $ Struct identifier fields modifiers -- (Position p)

classFieldDefinitionParser :: Parser (ClassMember Annotation)
classFieldDefinitionParser = do
  p <- getPosition
  field <- fieldDefinitionParser
  return $ ClassField field (Position p)

classMethodParser :: Parser (ClassMember Annotation)
classMethodParser = do
  p <- getPosition
  reserved "method"
  name <- identifierParser
  parens (reserved "&priv" >> reserved "self")
  typeSpec <- optionMaybe (reservedOp "->" >>  typeSpecifierParser)
  blockRet <- braces blockParser
  return $ ClassMethod name typeSpec blockRet (Position p)
      
classProcedureParser :: Parser (ClassMember Annotation)
classProcedureParser = do
  p <- getPosition
  reserved "procedure"
  name <- identifierParser
  params <- parens procedureParamsParser
  reservedOp "{"
  block <- many blockItemParser
  emptyReturn
  reservedOp "}"
  return $ ClassProcedure name params block (Position p)
  where
    procedureParamsParser :: Parser [Parameter]
    procedureParamsParser =
      reserved "&priv" >> reserved "self" >> option [] (comma >> sepBy parameterParser comma)

classViewerParser :: Parser (ClassMember Annotation)
classViewerParser = do
  p <- getPosition
  reserved "viewer"
  name <- identifierParser
  params <- parens viewerParamsParser
  typeSpec <- reservedOp "->" >> typeSpecifierParser
  blockRet <- braces blockParser
  return $ ClassViewer name params typeSpec blockRet (Position p)
  where
    viewerParamsParser :: Parser [Parameter]
    viewerParamsParser =
      reserved "&self" >> option [] (comma >> sepBy parameterParser comma)

classDefinitionParser :: Parser (TypeDef Annotation)
classDefinitionParser = do
  modifiers <- many modifierParser
  classKind <- classKindParser
  -- p <- getPosition
  reserved "class"
  identifier <- identifierParser
  fields <- braces (many1 $ classMethodParser <|> classViewerParser <|> classProcedureParser <|> classFieldDefinitionParser)
  _ <- semi
  return $ Class classKind identifier fields modifiers
  where
    classKindParser :: Parser ClassKind
    classKindParser = 
      (reserved "task" >> return TaskClass)
      <|> (reserved "resource" >> return ResourceClass)
      <|> (reserved "handler" >> return HandlerClass)

variantDefinitionParser :: Parser EnumVariant
variantDefinitionParser = identifierParser >>= \identifier ->
  try (parens (sepBy1 typeSpecifierParser comma) <&> EnumVariant identifier)
  <|> return (EnumVariant identifier [])

enumDefinitionParser :: Parser (TypeDef Annotation)
enumDefinitionParser = do
  modifiers <- many modifierParser
  reserved "enum"
  identifier <- identifierParser
  variants <- braces (sepBy1 (try variantDefinitionParser) comma)
  _ <- semi
  return $ Enum identifier variants modifiers

-- | Top Level parser
topLevel :: Parser (AnnotatedProgram Annotation)
topLevel = many1 $
  try functionParser <|> try globalDeclParser
  <|> try typeDefintionParser
  -- <|> moduleInclusionParser

moduleIdentifierParser :: Parser [ String ]
moduleIdentifierParser = sepBy1 firstCapital dot
  where
    firstCapital = (:)
      <$> (upper <?> "Module paths begin with capital letters.")
      <*> (many letter <?> "Module names only accept letters.")

singleModule :: Parser ([ Modifier ], [String], Annotation )
singleModule = (,,) <$> many modifierParser <*> moduleIdentifierParser <*> (Position <$> getPosition)

moduleInclusionParser :: Parser [ Module Annotation]
moduleInclusionParser = do
  reserved "import"
  modules <- braces (sepBy1 (wspcs *> singleModule <* wspcs) comma)
  return $ map (\(mod, ident, ann) -> ModInclusion ident mod ann) modules

contents :: Parser a -> Parser a
contents p = wspcs *> p <* eof

terminaProgram :: Parser (TerminaProgram Annotation)
terminaProgram = Termina <$> option [] moduleInclusionParser <*> contents topLevel

-- | Simple function to test parsers
strParse :: String -> Either ParseError (AnnotatedProgram Annotation)
strParse = parse topLevel ""
