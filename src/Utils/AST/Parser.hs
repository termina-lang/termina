-- | Utility AST functions

module Utils.AST.Parser where

import           AST.Parser
import Data.Maybe

-- Ground Type equality
groundTyEq :: TypeSpecifier -> TypeSpecifier -> Bool
groundTyEq  UInt8  UInt8 = True
groundTyEq  UInt16  UInt16 = True
groundTyEq  UInt32  UInt32 = True
groundTyEq  UInt64  UInt64 = True
groundTyEq  Int8  Int8 = True
groundTyEq  Int16  Int16 = True
groundTyEq  Int32  Int32 = True
groundTyEq  Int64  Int64 = True
groundTyEq  USize  USize = True
groundTyEq  Bool  Bool = True
groundTyEq  Unit Unit = True
groundTyEq  (Option _) (Option Unit) = True
groundTyEq  (Option Unit) (Option _) = True
groundTyEq  (Option tyspecl) (Option tyspecr) = groundTyEq tyspecl tyspecr
groundTyEq  (Reference Mutable tyspecl) (Reference Mutable tyspecr) = groundTyEq tyspecl tyspecr
groundTyEq  (Reference Immutable tyspecl) (Reference Immutable tyspecr) = groundTyEq tyspecl tyspecr
groundTyEq  (DynamicSubtype tyspecl) (DynamicSubtype tyspecr) = groundTyEq tyspecl tyspecr
-- TODO: These are considered complex types and should be handled differently
-- TODO: We are delaying the checking of the size of the vectors to a further stage
groundTyEq  (Array typespecl _sizel) (Array typespecr _sizer) = groundTyEq typespecl typespecr
groundTyEq  (DefinedType idl) (DefinedType idr) = idl == idr
-- Location subtypes
groundTyEq  (Location tyspecl) (Location tyspecr) = groundTyEq tyspecl tyspecr
groundTyEq  (Location tyspecl) tyspecr = groundTyEq tyspecl tyspecr
groundTyEq  tyspecl (Location tyspecr) = groundTyEq tyspecl tyspecr
--
groundTyEq  _ _ = False

constExprEq :: ConstExpression a -> ConstExpression a -> Bool
constExprEq (KC (I intl (Just tyspecl)) _) (KC (I intr (Just tyspecr)) _) = groundTyEq tyspecl tyspecr && intl == intr
constExprEq (KC (B vall) _) (KC (B valr) _) = vall == valr
constExprEq (KC (C charl) _) (KC (C charr) _) = charl == charr
constExprEq _ _ = False

-- Helper to detect invocations to 'self'
objIsSelf :: Object a -> Bool
objIsSelf (Variable ident _ann ) = ident == "self"
objIsSelf _ = False


----------------------------------------
-- Invokation Dependency in a block of code.
-- Capturing the pattern `self->f()` on objects.
-- As far as I understand it, all objects should have name, and thus, we cannot
-- (or shoudln't) concatenate invocations.

-- We do not have name shadowing, so technically there cannot be two things with
-- the same name. We do not need to insepect 'self->f()', but I am afraid of
-- breaking something else.

selfInv :: Expression a -> (Object a -> Bool) -> Maybe Identifier
selfInv (MemberFunctionAccess obj mident _constArgs _args _ann) isSelf =
  if isSelf obj then Just mident else Nothing
selfInv (DerefMemberFunctionAccess obj mident _constArgs _args _ann) isSelf =
  if isSelf obj then Just mident else Nothing
selfInv _ _isSelf = Nothing

selfInvStmt :: (Object a -> Bool) -> Statement a -> [Identifier]
selfInvStmt isSelf = selfInvStmt'
 where
    isSelfExpression e = maybeToList (selfInv e isSelf)
    -- selfInvStmt' :: Statement' (Expression' obj) obj a -> [Identifier]
    selfInvStmt' (Declaration _vident _accK _type e _ann) = isSelfExpression e
    selfInvStmt' (AssignmentStmt _obj e _ann) = isSelfExpression e
    selfInvStmt' (IfElseStmt eC bIf bEls bEl _ann) =
      isSelfExpression eC
        ++
        concatMap selfInvStmt' bIf
        ++
        concatMap (\ el ->
                     isSelfExpression (elseIfCond el)
                ++ selfInvBlock isSelf (elseIfBody el)
                ) bEls
        ++ maybe [] (concatMap selfInvStmt') bEl
    selfInvStmt' (ForLoopStmt _loopIdent _type _initV _endV cBreak body _ann) =
      -- isSelfExpression initV ++ isSelfExpression endV ++
      concat (maybeToList (isSelfExpression <$> cBreak))
      ++ concatMap selfInvStmt' body
    selfInvStmt' (MatchStmt e mcases _ann) =
      isSelfExpression e
      ++  concatMap (selfInvBlock isSelf . matchBody) mcases
    selfInvStmt' (SingleExpStmt e _ann) = isSelfExpression e

selfInvBlock :: (Object a -> Bool) -> Block a -> [Identifier]
selfInvBlock isSelf = concatMap (selfInvStmt isSelf)

selfInvRetStmt :: (Object a -> Bool) -> ReturnStmt a -> [Identifier]
selfInvRetStmt isSelf = maybe [] ( maybeToList . (`selfInv` isSelf)) . returnExpression

selfInvBlockRet :: (Object a -> Bool) -> BlockRet a -> [Identifier]
selfInvBlockRet isSelf bret
  = selfInvBlock isSelf (blockBody bret)
  ++ selfInvRetStmt isSelf (blockRet bret)


selfDepClass
  :: (Object a -> Bool)
  -> ClassMember a
  -> Maybe (Identifier, [Identifier])
selfDepClass isSelf = selfDepClass'
 where
   -- Fields do not have self dependencies
   selfDepClass' (ClassField {}) = Nothing
   -- Methods can
   selfDepClass' (ClassMethod mId _type bRet _ann) =
     Just (mId,selfInvBlockRet isSelf bRet)
   -- Procedures can
   selfDepClass' (ClassProcedure pId _constParams _params blk _ann) =
     Just (pId, selfInvBlock isSelf blk)
   -- Viewers can
   selfDepClass' (ClassViewer vId _constParams _params _type bRet _ann) =
     Just (vId , selfInvBlockRet isSelf bRet)
   -- Actions can
   selfDepClass' (ClassAction aId _param _type bRet _ann) =
      Just (aId , selfInvBlockRet isSelf bRet)