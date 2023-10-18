module PPrinter.TypeDef where

import Prettyprinter

import AST.Seman
import PPrinter.Common
import PPrinter.Statement
import Semantic.Monad (SemanticAnns)
import Data.Map (empty)


ppStructField :: FieldDefinition -> DocStyle
ppStructField (FieldDefinition identifier ts) = ppTypeSpecifier ts <+> pretty identifier <> ppDimension ts <> semi

-- | Pretty prints a class field
-- This function is only used when generating the class structure.
-- It takes as argument the class member to print.
-- It returns the pretty printed class field.
ppClassField :: ClassMember a -> DocStyle
ppClassField (ClassField (FieldDefinition identifier ts) _) = ppTypeSpecifier ts <+> pretty identifier <> ppDimension ts <> semi
ppClassField _ = error "invalid class member"

-- | Pretty prints the ID field of a class depending on its kind
ppClassIDFieldDeclaration :: ClassKind -> DocStyle
ppClassIDFieldDeclaration ResourceClass = resourceID <+> ppResourceClassIDField <> semi
ppClassIDFieldDeclaration TaskClass = taskID <+> ppTaskClassIDField <> semi
ppClassIDFieldDeclaration HandlerClass = handlerID <+> ppHandlerClassIDField <> semi

filterStructModifiers :: [Modifier] -> [Modifier]
filterStructModifiers = filter (\case
      Modifier "packed" Nothing -> True
      Modifier "align" _ -> True
      _ -> False)

ppClassProcedureOnEntry :: DocStyle
ppClassProcedureOnEntry = ppCFunctionCall resourceLock [pretty "self->__resource_id"] <> semi

ppClassProcedureOnExit :: DocStyle
ppClassProcedureOnExit = ppCFunctionCall resourceUnlock [pretty "self->__resource_id"] <> semi

ppSelfParameter :: Identifier -> DocStyle
ppSelfParameter classId = pretty classId <+> pretty "*" <+> pretty "self"

ppClassFunctionDefinition :: Identifier -> ClassMember SemanticAnns -> DocStyle
ppClassFunctionDefinition classId (ClassProcedure identifier parameters blk _) =
    -- | Function prototype
    ppCFunctionPrototype (classFunctionName classId identifier)
      (
        -- | Print the self parameter
        [ppSelfParameter classId] ++ 
        -- | Print the rest of the function parameters
        (ppParameterDeclaration (classFunctionName classId identifier) <$> parameters)
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
    subs = ppParameterSubstitutions parameters
ppClassFunctionDefinition classId (ClassViewer identifier parameters rts body _) =
    -- | Function prototype
    ppCFunctionPrototype (classFunctionName classId identifier)
      (
        -- | Print the self parameter
        [ppSelfParameter classId] ++ 
        -- | Print the rest of the function parameters
        (ppParameterDeclaration (classFunctionName classId identifier) <$> parameters)
      ) 
      -- | Class viewer return type
      (Just (ppReturnType (pretty identifier) rts))
    <+> ppBlockRet (ppParameterSubstitutions parameters) (classFunctionName classId identifier) body <> line
ppClassFunctionDefinition classId (ClassMethod identifier mrts body _) =
    -- | Function prototype
    ppCFunctionPrototype (classFunctionName classId identifier)
      -- | Print the self parameter
      [ppSelfParameter classId]
      -- | Class viewer return type
      (ppReturnType (pretty identifier) <$> mrts)
    <+> ppBlockRet empty (classFunctionName classId identifier) body <> line
ppClassFunctionDefinition _ _ = error "invalid class member"

ppClassFunctionDeclaration :: Identifier -> ClassMember SemanticAnns -> DocStyle
ppClassFunctionDeclaration classId (ClassProcedure identifier parameters _ _) =
  vsep $ 
  ([ppParameterVectorValueStructureDecl (classFunctionName classId identifier) (pretty pid) ts <> line | (Parameter pid ts@(Vector {})) <- parameters]) ++
  [
    ppCFunctionPrototype (classFunctionName classId identifier)
      ([ppSelfParameter classId] ++ (ppParameterDeclaration (classFunctionName classId identifier) <$> parameters)) Nothing <> semi,
    emptyDoc
  ]
ppClassFunctionDeclaration classId (ClassMethod identifier mrts _ _) =
  vsep $ 
  (case mrts of
    Just ts@(Vector {}) -> 
      [ppReturnVectorValueStructureDecl (classFunctionName classId identifier) ts <> line]
    _ -> []) ++
  [
    ppCFunctionPrototype (classFunctionName classId identifier) 
      [ppSelfParameter classId] 
      (ppReturnType (classFunctionName classId identifier) <$> mrts) <> semi,
    emptyDoc
  ]
ppClassFunctionDeclaration classId (ClassViewer identifier parameters rts _ _) =
  vsep $ 
  ([ppParameterVectorValueStructureDecl (classFunctionName classId identifier) (pretty pid) ts <> line | (Parameter pid ts@(Vector {})) <- parameters]) ++
  (case rts of
    Vector {} -> [ppReturnVectorValueStructureDecl (classFunctionName classId identifier) rts <> line]
    _ -> []) ++
  [
    ppCFunctionPrototype (pretty identifier)
      ([ppSelfParameter classId] ++ ((ppParameterDeclaration (pretty identifier)) <$> parameters))
      (Just (ppReturnType (pretty identifier) rts)) <> semi,
    emptyDoc
  ]
ppClassFunctionDeclaration classId member = error $ "member of class " ++ classId ++ " not a function: " ++ show member


-- | Pretty prints an enum variant
-- This function is only used when generating the variant enumeration
-- It takes as arguments the variant and its index.
ppEnumVariant :: EnumVariant -> DocStyle
ppEnumVariant (EnumVariant identifier _) = pretty identifier

ppTypeAttributes :: [Modifier] -> DocStyle
ppTypeAttributes [] = emptyDoc
ppTypeAttributes mods = attribute <> parens (parens (encloseSep emptyDoc emptyDoc (comma <> space) (map ppModifier mods))) <> space

ppEnumVariantParameter :: TypeSpecifier -> Integer -> DocStyle
ppEnumVariantParameter ts index =
  ppTypeSpecifier ts <+> pretty (namefy (show index)) <> ppDimension ts <> semi

ppEnumVariantParameterStruct :: EnumVariant -> DocStyle
ppEnumVariantParameterStruct (EnumVariant identifier params) =
  structC <+> braces' (
    indentTab . align $ vsep $
      zipWith
        ppEnumVariantParameter params [0..]
      ) <+> pretty (namefy identifier) <> semi

ppClassDefinition :: TypeDef SemanticAnns -> DocStyle
ppClassDefinition (Class _ identifier members _) =
  let (_fields, methods, procedures, viewers) =
          foldr (\member (fs,ms,prs,vws) ->
              case member of
                ClassField {} -> (member : fs, ms, prs, vws)
                ClassMethod {} -> (fs, member : ms, prs, vws)
                ClassProcedure {} -> (fs, ms, member : prs, vws)
                ClassViewer {} -> (fs, ms, prs, member : vws)
          ) ([],[], [], []) members
  in
    vsep $ map (\m -> ppClassFunctionDefinition identifier m) (methods ++ procedures ++ viewers)
ppClassDefinition _ = error "AST element is not a class"

-- | TypeDef pretty printer.
ppTypeDefDeclaration :: TypeDef SemanticAnns -> DocStyle
ppTypeDefDeclaration typeDef =
  case typeDef of
    -- | Struct declaration pretty pr_¨^ç+
    (Struct identifier fls modifiers) ->
      let structModifiers = filterStructModifiers modifiers in
      vsep [
        typedefC <+> structC <+> braces' (
          indentTab . align $ vsep $
            map ppStructField fls
            ) <+> ppTypeAttributes structModifiers <> pretty identifier <> semi,
        emptyDoc]
    -- | Enum declaration pretty printer  
    (Enum identifier variants _) ->
      let variantsWithParams = filter (not . null . assocData) variants
      in
      vsep [
        typedefC <+> enumC <+> braces' (
          indentTab . align $ vsep $ punctuate comma $
            map ppEnumVariant variants
            ) <+> enumIdentifier identifier <> semi,
        emptyDoc, -- empty line
        typedefC <+> structC <+> braces' (
          vsep [
            emptyDoc, -- empty line
            indentTab . align $ enumIdentifier identifier <+> enumVariantsField <> semi,
            if null variantsWithParams then emptyDoc else
              indentTab . align $
              vsep [
                emptyDoc, -- empty line
                unionC <+> braces' (
                  vsep $ map (indentTab . align
                    . ppEnumVariantParameterStruct) variantsWithParams
                ) <> semi,
                emptyDoc -- empty line
              ]
          ]
        ) <+> pretty identifier <> semi,
        emptyDoc ]
    (Class clsKind identifier members modifiers) ->
      let structModifiers = filterStructModifiers modifiers in
      let (fields, methods, procedures, viewers) =
              foldr (\member (fs,ms,prs,vws) ->
                  case member of
                    ClassField {} -> (member : fs, ms, prs, vws)
                    ClassMethod {} -> (fs, member : ms, prs, vws)
                    ClassProcedure {} -> (fs, ms, member : prs, vws)
                    ClassViewer {} -> (fs, ms, prs, member : vws)
              ) ([],[], [], []) members
      in
        vsep $ [
              typedefC <+> structC <+> braces' (
                indentTab . align $
                vsep (
                  -- | Map the regular fields
                  map ppClassField fields ++
                  -- | If the no_handler modifier is set, then we may use
                  -- a mutex sempahore to handle mutual exclusion
                  [ppClassIDFieldDeclaration clsKind]))
                    <+> ppTypeAttributes structModifiers <> pretty identifier <> semi,
              emptyDoc
          ]
          ++ map (\m -> ppClassFunctionDeclaration identifier m) (methods ++ procedures ++ viewers)
