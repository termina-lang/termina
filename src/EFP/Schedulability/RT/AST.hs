module EFP.Schedulability.RT.AST 
    (module EFP.Schedulability.RT.AST
    , module EFP.Schedulability.Core.AST) where

import EFP.Schedulability.Core.AST
import Utils.Annotations

data ConstFieldValue a =
    ConstStructFieldValue (ConstStructInitializer a)
    | ConstStructSimpleValue (ConstExpression a)
    deriving Show

data ConstFieldAssignment a = 
    ConstFieldAssignment {
        cfaFieldName :: Identifier -- ^ Field name
        , cfaValue :: ConstFieldValue a -- ^ Value
        , cfaAnnotation :: a -- ^ Annotation
    }
    deriving Show

data ConstStructInitializer a = 
    ConstStructInitializer
        [ConstFieldAssignment a] -- ^ Initial value of each field identifier
        a
    deriving Show

data RTTransStep a = 
    RTTransStepAction
        Identifier -- ^ Step name
        Identifier -- ^ Task or handler name
        Identifier -- ^ Action name
        Identifier -- ^ Path name
        (Maybe (RTTransStep a)) -- ^ Next step (if any)
        a -- ^ Annotation
    | RTTransStepMuticast 
        [RTTransStep a] -- ^ Steps to multicast to
        a -- ^ Annotation
    | RTTransStepConditional
        [(ConstExpression a, RTTransStep a)] -- ^ Condition and corresponding step
        a -- ^ Annotation
    deriving Show

instance Annotated RTTransStep where

    getAnnotation (RTTransStepAction _ _ _ _ _ a) = a
    getAnnotation (RTTransStepMuticast _ a) = a
    getAnnotation (RTTransStepConditional _ a) = a

    updateAnnotation (RTTransStepAction name task action path next _) = RTTransStepAction name task action path next
    updateAnnotation (RTTransStepMuticast steps _) = RTTransStepMuticast steps
    updateAnnotation (RTTransStepConditional conds _) = RTTransStepConditional conds

data RTElement a =
    RTTransaction 
        Identifier -- ^ Transaction identifier
        (RTTransStep a) -- ^ First step
        a -- ^ Annotation
    | RTSituation 
        Identifier -- ^ Situation identifier
        (ConstStructInitializer a) -- ^ Initializer expression
        a -- ^ Annotation
    deriving Show

instance Annotated RTElement where
    getAnnotation (RTTransaction _ _ a) = a
    getAnnotation (RTSituation _ _ a) = a

    updateAnnotation (RTTransaction ident step _) = RTTransaction ident step
    updateAnnotation (RTSituation ident expr _) = RTSituation ident expr