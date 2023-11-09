module PPrinter.TypeDef.Definition where

import Prettyprinter

import AST.Seman
import PPrinter.Common
import PPrinter.Statement
import PPrinter.TypeDef.Declaration
import Semantic.Monad (SemanticAnns)
import Data.Map (empty)


ppClassProcedureOnEntry :: DocStyle
ppClassProcedureOnEntry = ppCFunctionCall resourceLock [pretty "self->__resource_id"] <> semi

ppClassProcedureOnExit :: DocStyle
ppClassProcedureOnExit = ppCFunctionCall resourceUnlock [pretty "self->__resource_id"] <> semi

ppClassFunctionDefinition :: Identifier -> ClassMember SemanticAnns -> DocStyle
ppClassFunctionDefinition classId (ClassProcedure identifier parameters blk _) =
    -- | Function prototype
    ppCFunctionPrototype clsFuncName
      (
        -- | Print the self parameter
        ppSelfParameter classId :
        -- | Print the rest of the function parameters
        (ppParameterDeclaration clsFuncName <$> parameters)
      )
      -- | Class procedures do not return anything
      Nothing
    <+>
    -- | Function body
    braces' (line <>
      (indentTab . align $
        vsep (
          -- | Print the resource lock call
          [ppClassProcedureOnEntry, emptyDoc] ++
          -- | Print the function body
          [ppStatement subs s <> line | s <- blk] ++
          -- | Print the resource unlock call
          [ppClassProcedureOnExit, emptyDoc]
        )
        -- | Print the empty return statement
        <> line <> returnC <> semi <> line)
    ) <> line
  where
    clsFuncName = (classFunctionName (pretty classId) (pretty identifier))
    subs = ppParameterSubstitutions parameters
ppClassFunctionDefinition classId (ClassViewer identifier parameters rts body _) =
    -- | Function prototype
    ppCFunctionPrototype clsFuncName
      (
        -- | Print the self parameter
        pretty "const" <+> ppSelfParameter classId :
        -- | Print the rest of the function parameters
        (ppParameterDeclaration clsFuncName <$> parameters)
      )
      -- | Class viewer return type
      (Just (ppReturnType (pretty identifier) rts))
    <+> ppBlockRet (ppParameterSubstitutions parameters) clsFuncName body <> line
  where
    clsFuncName = (classFunctionName (pretty classId) (pretty identifier))
ppClassFunctionDefinition classId (ClassMethod identifier mrts body _) =
    -- | Function prototype
    ppCFunctionPrototype clsFuncName
      -- | Print the self parameter
      [ppSelfParameter classId]
      -- | Class viewer return type
      (ppReturnType (pretty identifier) <$> mrts)
    <+> ppBlockRet empty clsFuncName body <> line
  where
    clsFuncName = (classFunctionName (pretty classId) (pretty identifier))
ppClassFunctionDefinition _ _ = error "invalid class member"

ppClassDefinition :: TypeDef SemanticAnns -> DocStyle
ppClassDefinition (Class _ identifier members _) =
  let (_fields, methods, procedures, viewers) = classifyClassMembers members in
    vsep $ map (ppClassFunctionDefinition identifier) (methods ++ procedures ++ viewers)
ppClassDefinition _ = error "AST element is not a class"
