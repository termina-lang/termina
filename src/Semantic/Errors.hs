{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Module Encapsulating Semantic Errors

module Semantic.Errors where

-- Termina AST
import Semantic.AST
import Semantic.Reserved (ReservedBy(..))
import Utils.Annotations
import Utils.Errors
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Text.Parsec
import Semantic.Types
import qualified Parser.AST as PAST
import Parser.Types
import Utils.Printer

----------------------------------------
-- Type checker error handling
----------------------------------------
data Error
  -- | Expected /similar/ types?
  = 
    EMismatch (TerminaType SemanticAnn) (TerminaType SemanticAnn) -- ^ Type mismatch (Internal)
  | ENoStructFound Identifier -- ^ Struct not found (Internal)
  | EUnboxingExpression -- ^ Error when trying to unbox an object from an expression (Internal)
  | EInvalidObjectTypeAnnotation -- ^ Error when the semantic annotation of an object does not contain the expected type information (Internal)
  | EInvalidExprTypeAnnotation -- ^ Error when the semantic annotation of an expression does not contain the expected type information (Internal)
  | EInvalidMemberFunctionTypeAnnotation -- ^ Error when the semantic annotation of a member function does not containg the expected type information (Internal)
  | EExpectedStructType -- ^ Error when obtaining a struct type definition from the environment (Internal)
  | EExpectedEnumType -- ^ Error when obtaining an enum type definition from the environment (Internal)
  | EExpectedClassType -- ^ Error when obtaining a class type definition from the environment (Internal)
  | EExpectedInterfaceType -- ^ Error when obtaining an interface type from the environment (Internal)
  | EExpectedIntConstant -- ^ Error when obtaining the integer value of an integer constant (Internal)
  | EExpectedArrayTy (TerminaType SemanticAnn) -- ^ Expected a valid type for the elements of an array (Internal)
  | EExpectedCopyType (TerminaType SemanticAnn) -- ^ Expected a copiable type (Internal)
  | EExpectedNumType (TerminaType SemanticAnn) -- ^ Expected a numeric type (Internal)
  | EInvalidEmitterClass -- ^ Error when obtaining the the class of an emitter (Internal)
  | EInvalidObjectDeclaration Identifier -- ^ Invalid object declaration (Internal)
  | EMalformedSlice -- ^ Malformed slice (Internal)
  | EMalformedClassTyping -- ^ Malformed class typing (Internal)
  | EContinueActionNotFound -- ^ Action not found in continue statement (Internal)
  | EMissingIdentifier -- ^ Missing identifier (Internal)
  | EMatchCaseInternalError -- ^ Internal error in match case (Internal)
  | EStructDefEmpty Identifier -- ^ Empty struct definition (Internal)
  | EEnumDefEmpty Identifier -- ^ Empty enum definition (Internal)
  | EInterfaceEmpty Identifier -- ^ Empty interface definition (Internal)
  | ESystemInterfaceDefinition Identifier -- ^ System interface definition (Internal)
  | EInvalidConstExprType (TerminaType SemanticAnn) -- ^ Invalid constant expression type (Internal)
  | EInvalidArrayIndexing (TerminaType SemanticAnn) -- ^ Invalid array indexing
  | ENotNamedObject Identifier -- ^ Object not found
  | EExpressionNotConstant -- ^ Expected constant expression
  | EAssignmentToImmutable -- ^ Assignment to immutable variable
  | EIfElseNoOtherwise -- ^ Missing else clause
  | ENotCasteable (TerminaType SemanticAnn) (TerminaType SemanticAnn) -- ^ Casting error
  | EInvalidParameterType (Parameter SemanticAnn) -- ^ Invalid parameter type
  | EInvalidReturnType (TerminaType SemanticAnn) -- ^ Invalid return type
  | EProcedureCallExtraArgs (Identifier, [Parameter SemanticAnn], Location) Integer -- ^ Extra parameters in procedure call
  | EProcedureCallMissingArgs (Identifier, [Parameter SemanticAnn], Location) Integer -- ^ Missing parameters in procedure call
  | EProcedureCallArgTypeMismatch (Identifier, Parameter SemanticAnn, Location) Integer (TerminaType SemanticAnn) -- ^ Parameter type mismatch in procedure call
  | EUnknownProcedure Identifier -- ^ Unknown procedure
  | EResourceClassNoProvides Identifier -- ^ Resource class does not provide any interface
  | EResourceClassAction (Identifier, Location) Identifier -- ^ Resource class defines an action
  | EResourceClassInPort (Identifier, Location) Identifier -- ^ Resource class defines an in port
  | EResourceClassOutPort (Identifier, Location) Identifier -- ^ Resource class defines an out port
  | EInterfaceNotFound Identifier -- ^ Interface not found
  | EGlobalNotInterface Identifier -- ^ The type is not an interface
  | EProcedureNotFromProvidedInterfaces (Identifier, Location) Identifier -- ^ Procedure not from provided interfaces
  | EMissingProcedure Identifier Identifier -- ^ Missing procedure
  | EProcedureExtraParams (Identifier, Identifier, [TerminaType SemanticAnn], Location) Integer -- ^ Extra parameters in procedure definition
  | EProcedureMissingParams (Identifier, Identifier, [TerminaType SemanticAnn], Location) Integer -- ^ Missing parameters in procedure definition
  | EProcedureParamTypeMismatch (Identifier, Identifier, TerminaType SemanticAnn, Location) (TerminaType SemanticAnn) -- ^ Parameter type mismatch in procedure definition
  | ETaskClassProvides Identifier -- ^ Task class provides an interface
  | ETaskClassProcedure (Identifier, Location) Identifier -- ^ Task class defines a procedure
  | ETaskClassNoActions Identifier -- ^ Task class does not define any actions
  | EHandlerClassProvides Identifier -- ^ Handler class provides an interface
  | EHandlerClassProcedure (Identifier, Location) Identifier -- ^ Handler class defines a procedure
  | EHandlerClassNoAction Identifier -- ^ Handler class does not define any actions
  | EHandlerClassMultipleActions Identifier Location -- ^ Handler class defines multiple actions
  | EHandlerClassNoSinkPort Identifier -- ^ Handler class does not define a sink port
  | EHandlerClassMultipleSinkPorts Identifier Location -- ^ Handler class defines multiple sink ports
  | EHandlerClassInPort (Identifier, Location) Identifier -- ^ Handler class defines an in port
  | EIfElseIfCondNotBool (TerminaType SemanticAnn) -- ^ If-else-if condition is not a boolean
  | EFunctionCallExtraArgs (Identifier, [Parameter SemanticAnn], Location) Integer -- ^ Extra parameters in function call
  | EFunctionCallMissingArgs (Identifier, [Parameter SemanticAnn], Location) Integer -- ^ Missing parameters in function call
  | EFunctionCallArgTypeMismatch (Identifier, Parameter SemanticAnn, Location) Integer (TerminaType SemanticAnn) -- ^ Parameter type mismatch in function call
  | EMemberAccessNotFunction Identifier -- ^ Access to a member that is not a function
  | EMutableReferenceToImmutable -- ^ Mutable reference to immutable object
  | EMutableReferenceToPrivate -- ^ Mutable reference to immutable object
  | EBinOpExpectedTypeLeft Op (TerminaType SemanticAnn) (TerminaType SemanticAnn) -- ^ Binary operation expected type on the left
  | EBinOpExpectedTypeRight Op (TerminaType SemanticAnn) (TerminaType SemanticAnn) -- ^ Binary operation expected type on the right
  | EBinOpTypeMismatch Op (TerminaType SemanticAnn) (TerminaType SemanticAnn) -- ^ Binary operation type mismatch
  | EBinOpExpectedTypeNotBool Op (TerminaType SemanticAnn) -- ^ Binary operation expected result type not boolean
  | EBinOpLeftTypeNotBool Op (TerminaType SemanticAnn) -- ^ Binary operation expected boolean type on the left
  | EBinOpRightTypeNotBool Op (TerminaType SemanticAnn) -- ^ Binary operation expected boolean type on the right
  | EBinOpExpectedTypeNotArith Op (TerminaType SemanticAnn) -- ^ Binary operation expected result type not arithmetic (integer or float)
  | EBinOpLeftTypeNotArith Op (TerminaType SemanticAnn) -- ^ Binary operation expected arithmetic type on the left
  | EBinOpRightTypeNotArith Op (TerminaType SemanticAnn) -- ^ Binary operation expected arithmetic type on the right
  | EBinOpExpectedTypeNotInt Op (TerminaType SemanticAnn) -- ^ Binary operation expected result type not integer
  | EBinOpLeftTypeNotInt Op (TerminaType SemanticAnn) -- ^ Binary operation expected integer type on the left
  | EBinOpRightTypeNotInt Op (TerminaType SemanticAnn) -- ^ Binary operation expected integer type on the right
  | EBinOpRightTypeNotPos Op (TerminaType SemanticAnn) -- ^ Binary operation expected positive numeric type on the right
  | EBinOpLeftTypeNotEq Op (TerminaType SemanticAnn) -- ^ Binary operation expected equatable type on the left
  | EBinOpRightTypeNotEq Op (TerminaType SemanticAnn) -- ^ Binary operation expected equatable type on the right
  | EAtomicAccessInvalidType (TerminaType SemanticAnn) -- ^ Invalid type for the atomic access interface
  | EAtomicArrayAccessInvalidType (TerminaType SemanticAnn) -- ^ Invalid type for the atomic array access interface
  | EAtomicInvalidType (TerminaType SemanticAnn) -- ^ Invalid atomic type
  | EAtomicArrayInvalidType (TerminaType SemanticAnn) -- ^ Invalid atomic array type
  | EAtomicConnectionTypeMismatch (TerminaType SemanticAnn) (TerminaType SemanticAnn) -- ^ Atomic connection type mismatch
  | EAtomicArrayConnectionTypeMismatch (TerminaType SemanticAnn) (TerminaType SemanticAnn) -- ^ Atomic array connection type mismatch
  | EInvalidDefaultCase -- ^ Invalid default case in match statement
  | EConstantWithoutKnownType (Const SemanticAnn) -- ^ Constant without known type
  | EStructInitializerInvalidUse -- ^ Invalid use of a struct initializer
  | EStructInitializerTypeMismatch (TerminaType SemanticAnn) (TerminaType SemanticAnn) -- ^ Struct initializer type mismatch
  | EEnumInitializerExpectedTypeMismatch (TerminaType SemanticAnn) (TerminaType SemanticAnn) -- ^ Enum initializer expected type mismatch
  | ESliceInvalidUse -- ^ Invalid use of a slice
  | EArrayInitializerInvalidUse -- ^ Invalid use of an array initializer
  | EArrayInitializerNotArray (TerminaType SemanticAnn) -- ^ Assignment of an array initializer to a non-array type
  | EArrayExprListInitializerInvalidUse -- ^ Invalid use of an expression list array initializer
  | EArrayExprListInitializerNotArray (TerminaType SemanticAnn) -- ^ Assignment of an expression list array initializer to a non-array type
  | EMonadicVariantInitializerInvalidUse -- ^ Invalid use of a builtin variant initializer
  | EForLoopLowerBoundTypeMismatch (TerminaType SemanticAnn) (TerminaType SemanticAnn) -- ^ For loop lower bound type mismatch
  | EForLoopUpperBoundTypeMismatch (TerminaType SemanticAnn) (TerminaType SemanticAnn) -- ^ For loop upper bound type mismatch
  | EArrayExprListInitializerExprTypeMismatch (TerminaType SemanticAnn) (TerminaType SemanticAnn) -- ^ List of initializing expressions type mismatch
  | EReturnValueExpected (TerminaType SemanticAnn) -- ^ Expected return value
  | EReturnValueNotUnit -- ^ Return value not expected
  | EInvalidArrayType (TerminaType SemanticAnn) -- ^ Invalid array type
  | EInvalidBoxType (TerminaType SemanticAnn) -- ^ Invalid box type
  | ENoTypeFound Identifier -- ^ Type not found
  | EGlobalNotType (Identifier, Location) -- ^ Global object but not a type
  | EInvalidAccessToGlobal Identifier -- ^ Invalid access to global object
  | EConstantIsReadOnly Identifier -- ^ Invalid write to a constant
  | ESymbolAlreadyDefined (Identifier, Location) -- ^ Symbol already defined
  | EContinueInvalidExpression -- ^ Invalid expression in continue statement
  | EContinueInvalidMethodOrViewerCall Identifier -- ^ Invalid method or viewer call in continue statement
  | EContinueInvalidMemberCall (TerminaType SemanticAnn) -- ^ Invalid member call in continue statement
  | EContinueActionExtraArgs (Identifier, [Parameter SemanticAnn], Location) Integer -- ^ Extra parameters in action call in continue statement
  | EContinueActionMissingArgs (Identifier, Location) -- ^ Missing parameters in action call in continue statement
  | EEnumVariantInitializerInvalidUse -- ^ Invalid use of an enum variant initializer
  | EEnumVariantNotFound Identifier Identifier -- ^ Enum variant not found
  | EEnumVariantExtraParams (Identifier, Location) (Identifier, [TerminaType SemanticAnn]) Integer -- ^ Extra parameters in enum variant
  | EEnumVariantMissingParams (Identifier, Location) (Identifier, [TerminaType SemanticAnn]) Integer -- ^ Missing parameters in enum variant
  | EEnumVariantParamTypeMismatch (Identifier, Location) (Identifier, Integer, TerminaType SemanticAnn) (TerminaType SemanticAnn) -- ^ Parameter type mismatch in enum variant
  | EFunctionNotFound Identifier -- ^ Function not found
  | EGlobalNotFunction (Identifier, Location) -- ^ Global object but not a function
  | EUnexpectedNumericConstant (TerminaType SemanticAnn) -- ^ Unexpected numeric constant
  | EInvalidAssignmentExprType (TerminaType SemanticAnn) -- ^ Invalid assignment expression type
  | EInvalidMessageType (TerminaType SemanticAnn) -- ^ Invalid message type
  | EInvalidOptionType (TerminaType SemanticAnn) -- ^ Invalid option type
  | EInvalidReferenceType (TerminaType SemanticAnn) -- ^ Invalid reference type
  | EInvalidFixedLocationType (TerminaType SemanticAnn) -- ^ Invalid fixed-location type
  | EInvalidAllocatorType (TerminaType SemanticAnn) -- ^ Invalid allocator type
  | EInvalidClassFieldType (TerminaType SemanticAnn) -- ^ Invalid class field type
  | EInvalidStructFieldType (TerminaType SemanticAnn) -- ^ Invalid struct field type
  | EInvalidEnumParameterType (TerminaType SemanticAnn) -- ^ Invalid enum parameter type
  | EInvalidAccessPortType (TerminaType SemanticAnn) -- ^ Invalid access port type
  | EInvalidDeclarationType (TerminaType SemanticAnn) -- ^ Invalid declaration type
  | EInvalidTypeSpecifier (PAST.TypeSpecifier ParserAnn) -- ^ Invalid type specifier
  | EInvalidNumericConstantType (TerminaType SemanticAnn) -- ^ Invalid numeric constant type
  | EInvalidActionParameterType (TerminaType SemanticAnn) -- ^ Invalid action parameter type
  | EInvalidProcedureParameterType (TerminaType SemanticAnn) -- ^ Invalid procedure parameter type
  | EMemberFunctionCallExtraArgs (Identifier, [Parameter SemanticAnn], Location) Integer -- ^ Extra arguments in member function call
  | EMemberFunctionCallMissingArgs (Identifier, [Parameter SemanticAnn], Location) Integer -- ^ Missing arguments in member function call
  | EMemberFunctionCallArgTypeMismatch (Identifier, Parameter SemanticAnn, Location) Integer (TerminaType SemanticAnn) -- ^ Parameter type mismatch in member function call
  | EArrayIndexNotUSize (TerminaType SemanticAnn) -- ^ Invalid array index type
  | EArraySliceLowerBoundNotUSize (TerminaType SemanticAnn) -- ^ Invalid array slice lower bound type
  | EArraySliceUpperBoundNotUSize (TerminaType SemanticAnn) -- ^ Invalid array slice upper bound type
  | EOutboundPortSendInvalidNumArgs Integer -- ^ Invalid number of arguments in outbound port send
  | EOutboundPortArgTypeMismatch (TerminaType SemanticAnn) (TerminaType SemanticAnn) -- ^ Parameter type mismatch in output port
  | EAssignmentExprMismatch (TerminaType SemanticAnn) (TerminaType SemanticAnn) -- ^ Assignment expression type mismatch
  | EFieldValueAssignmentMissingFields (TerminaType SemanticAnn, Location) [Identifier] -- ^ Missing field/s in field assignment expression
  | EFieldValueAssignmentUnknownFields (TerminaType SemanticAnn, Location) [Identifier] -- ^ Unknown field/s in field assignment expression
  | EFieldNotFixedLocation Identifier (TerminaType SemanticAnn) -- ^ Field is not a fixed-location
  | EFieldNotAccessPort Identifier (TerminaType SemanticAnn) -- ^ Field is not an access port
  | EFieldNotSinkOrInboundPort Identifier (TerminaType SemanticAnn) -- ^ Field is not a sink or in port
  | EFieldNotOutboundPort Identifier (TerminaType SemanticAnn) -- ^ Field is not an out port
  | EMemberAccessInvalidType (TerminaType SemanticAnn) -- ^ Invalid member access type
  | EMemberFunctionCallInvalidType (TerminaType SemanticAnn) -- ^ Invalid member function call type
  | EMemberAccessUnknownField (Identifier, Location) Identifier -- ^ Unknown field in member access
  | EInvalidProcedureCallInsideMemberFunction -- ^ Invalid procedure call inside member function
  | EConstantOutRange (Const SemanticAnn) -- ^ Numeric constant out of range
  | EForIteratorInvalidType (TerminaType SemanticAnn) -- ^ Invalid for iterator type
  | EUsedTypeName Identifier Location -- ^ Type name already used
  | EUsedGlobalName Identifier Location -- ^ Global object name already used
  | EUsedFunName Identifier Location -- ^ Function name already used
  | EAccessPortConnectionInvalidGlobal Identifier -- ^ Invalid access port connection
  | EAccessPortConnectionInterfaceNotProvided Identifier Identifier -- ^ Resource does not provide the interface
  | ESinkPortConnectionInvalidGlobal Identifier -- ^ Invalid sink port connection
  | EInboundPortConnectionInvalidObject Identifier -- ^ Invalid inbound port connection
  | EOutboundPortConnectionInvalidGlobal Identifier -- ^ Invalid outbound port connection
  | EAllocatorPortConnectionInvalidGlobal Identifier -- ^ Invalid allocator port connection
  | EAtomicAccessPortConnectionInvalidGlobal Identifier -- ^ Invalid atomic access port connection
  | EAtomicArrayAccessPortConnectionInvalidGlobal Identifier -- ^ Invalid atomic array access port connection
  | EStructDefNotUniqueField [Identifier] -- ^ Repeated field in struct definition
  | EEnumDefNotUniqueVariant [Identifier] -- ^ Repeated variant in enum definition
  | EInterfaceNotUniqueProcedure [Identifier] -- ^ Repeated procedure in interface definition
  | EClassLoop [(Identifier, Location)] -- ^ Loop between member function calls in class definition
  | EDereferenceInvalidType (TerminaType SemanticAnn) -- ^ Invalid dereference type
  | EMatchInvalidType (TerminaType SemanticAnn) -- ^ Invalid match type
  | EMatchCaseDuplicate Identifier Location -- ^ Duplicate case in match statement
  | EMatchCaseUnknownVariant Identifier -- ^ Unknown variant in match case
  | EMatchMissingCases [Identifier] -- ^ Missing case/s in match statement
  | EIsVariantInvalidType (TerminaType SemanticAnn) -- ^ Invalid type for is-variant expression
  | EIsOptionVariantInvalidType (TerminaType SemanticAnn) -- ^ Invalid type for is-option-variant expression
  | EIsVariantEnumTypeMismatch Identifier Identifier -- ^ Enum type mismatch in is variant expression
  | EOutboundPortInvalidProcedure Identifier -- ^ Invalid procedure in outbound port
  | EInvalidPoolInitialization -- ^ Invalid pool initialization
  | EInvalidMsgQueueInitialization -- ^ Invalid message queue initialization
  | EUnknownGlobal Identifier -- ^ Unknown global object
  | EInvalidInterruptEmitterType (TerminaType SemanticAnn) -- ^ Invalid interrupt emitter type
  | EInvalidPeriodicTimerEmitterType (TerminaType SemanticAnn) -- ^ Invalid periodic timer emitter type
  | EInvalidSystemInitEmitterType (TerminaType SemanticAnn) -- ^ Invalid system init emitter type
  | EInboundPortConnectionMsgQueueTypeMismatch Identifier (TerminaType SemanticAnn) (TerminaType SemanticAnn) -- ^ Message queue type mismatch in inbound port connection
  | EOutboundPortConnectionMsgQueueTypeMismatch Identifier (TerminaType SemanticAnn) (TerminaType SemanticAnn) -- ^ Message queue type mismatch in outbound port connection
  | EAllocatorPortConnectionPoolTypeMismatch Identifier (TerminaType SemanticAnn) (TerminaType SemanticAnn) -- ^ Pool type mismatch in allocator port connection
  | EInvalidTaskType (TerminaType SemanticAnn) -- ^ Invalid task type
  | EInvalidHandlerType (TerminaType SemanticAnn) -- ^ Invalid handler type
  | EInvalidResourceType (TerminaType SemanticAnn) -- ^ Invalid resource type
  | EInvalidEmitterType (TerminaType SemanticAnn) -- ^ Invalid emitter type
  | EInvalidChannelType (TerminaType SemanticAnn) -- ^ Invalid channel type
  | EEmitterClassNotInstantiable Identifier -- ^ Emitter class not instantiable
  | ESingleExpressionTypeNotUnit (TerminaType SemanticAnn) -- ^ Single expression type not unit
  | EInterfaceDuplicatedExtendedIface Identifier -- ^ Duplicated extended interface
  | EInterfaceDuplicatedExtendedProcedure Identifier Identifier Identifier -- ^ Duplicated procedure in extended interfaces
  | EInterfaceProcedurePreviouslyExtended Identifier Identifier -- ^ Procedure previously defined by an extended interface
  | EInterfacePreviouslyExtended Identifier Identifier -- ^ Interface previously extended by another interface
  | EResourceDuplicatedProvidedIface Identifier -- ^ Duplicated provided interface
  | EResourceDuplicatedProvidedProcedure Identifier Identifier Identifier -- ^ Duplicated procedure in provided interfaces
  | EResourceInterfacePreviouslyExtended Identifier Identifier -- ^ Interface previously extended by another interface
  | EStringInitializerInvalidUse -- ^ Invalid use of a string initializer
  | EStringInitializerNotArrayOfChars (TerminaType SemanticAnn) -- ^ Assignment of a string array initializer to an invalid type
  | EInvalidConstType (TerminaType SemanticAnn) -- ^ Invalid type for constant
  | EInvalidAccessToConstExpr Identifier -- ^ Invalid access to constant expression
  | EInvalidResultType (TerminaType SemanticAnn) -- ^ Invalid type for result
  | EInvalidStatusType (TerminaType SemanticAnn) -- ^ Invalid type for status
  | EInvalidVariantForOption Identifier -- ^ Invalid variant for option
  | EInvalidVariantForResult Identifier -- ^ Invalid variant for result
  | EInvalidVariantForStatus Identifier -- ^ Invalid variant for status
  | EInvalidResultTypeSpecifier (PAST.TypeSpecifier ParserAnn) -- ^ Invalid type specifier for result
  | EMonadicVariantParameterTypeMismatch (TerminaType SemanticAnn) (TerminaType SemanticAnn) -- ^ Parameter type mismatch in monadic variant
  | EObjectPreviouslyMoved Location -- ^ Object previously moved
  | EIsStatusVariantInvalidType (TerminaType SemanticAnn) -- ^ Invalid type for is-status-variant expression
  | EIsResultVariantInvalidType (TerminaType SemanticAnn) -- ^ Invalid type for is-result-variant expression
  | EInvalidSystemExceptEmitterType (TerminaType SemanticAnn) -- ^ Invalid system except emitter type
  | EInvalidInterruptActionReturnType Identifier (TerminaType SemanticAnn) -- ^ Invalid interrupt action return type
  | EInvalidPeriodicTimerActionReturnType Identifier (TerminaType SemanticAnn) -- ^ Invalid periodic timer action return type
  | EInvalidSystemInitActionReturnType Identifier (TerminaType SemanticAnn) -- ^ Invalid system init action return type
  | EInvalidSystemExceptActionReturnType Identifier (TerminaType SemanticAnn) -- ^ Invalid system except action return type
  | EInvalidMsgQueueActionReturnType Identifier (TerminaType SemanticAnn) -- ^ Invalid message queue action return type
  | ETypeNotInScope Identifier QualifiedName -- ^ Type not in scope
  | EFunctionNotInScope Identifier QualifiedName -- ^ Function not in scope
  | EUnknownAction Identifier -- ^ Unknown action
  | EInPortActionParamTypeMismatch (Identifier, Location) (TerminaType SemanticAnn) (TerminaType SemanticAnn) -- ^ In port action parameter type mismatch
  | ESinkPortActionParamTypeMismatch (Identifier, Location) (TerminaType SemanticAnn) (TerminaType SemanticAnn) -- ^ Sink port action parameter type mismatch
  | EInvalidViewerParameterType (TerminaType SemanticAnn) -- ^ Invalid viewer parameter type
  | EInvalidAccessToProcedureFromImmutableSelfReference -- ^ Invalid access to procedure from immutable self reference
  | EInvalidAccessToOutPortFromImmutableSelfReference -- ^ Invalid access to out port from immutable self reference
  | EProcedureSelfAccessKindMismatch (Identifier, Identifier, AccessKind, Location) AccessKind -- ^ Self reference access kind mismatch in procedure
  | ETaskClassMethod (Identifier, Location) Identifier -- ^ Task class defines a method
  | EHandlerClassMethod (Identifier, Location) Identifier -- ^ Handler class defines a method
  | EResourceClassViewer (Identifier, Location) Identifier -- ^ Resource class defines a viewer
  | EUnprotectedResourceWithRegularFields (Identifier, Location) -- ^ Unprotected resource with regular fields
  | EMemberFunctionWithMutableSelfInTaskClass Identifier -- ^ Member function with mutable self reference in task class
  | EMemberFunctionWithMutableSelfInHandlerClass Identifier -- ^ Member function with mutable self reference in handler class
  | ECharLiteralOutOfRange Char -- ^ Character literal whose code point is outside the 7-bit ASCII range (SE-217)
  | EReferenceToPackedMember Identifier -- ^ Reference to a member reached through a packed struct, on a strict-alignment target (SE-218)
  | EReservedIdentifier Identifier ReservedBy -- ^ Identifier that C keeps for itself (SE-219)
  deriving Show

type SemanticErrors = AnnotatedError Error Location

instance Diagnosable Error where

    describe (EInvalidArrayIndexing ty) =
        diagnostic "SE-001" "invalid array indexing"
            ("You are trying to index an object of type \x1b[31m" <> showText ty <> "\x1b[0m.")
    describe (ENotNamedObject ident) =
        diagnostic "SE-002" "object not found"
            ("The variable \x1b[31m" <> T.pack ident <> "\x1b[0m has not been declared")
    describe EExpressionNotConstant =
        diagnostic "SE-003" "expected constant expression"
            ("The expression is not constant.")
    describe EAssignmentToImmutable =
        diagnostic "SE-004" "assignment to immutable variable"
            ("You are trying to assign a value to an immutable object.")
    describe EIfElseNoOtherwise =
        diagnostic "SE-005" "missing else clause"
            ("You are missing the else clause in an if-else-if statement.\n" <> "You must provide an else clause if you are defining an else-if clause.")
    describe (ENotCasteable ty1 ty2) =
        diagnostic "SE-006" "invalid cast"
            ("You cannot cast a value of type \x1b[31m" <> showText ty1 <> "\x1b[0m to type \x1b[31m" <> showText ty2 <> "\x1b[0m.")
    describe (EInvalidParameterType (Parameter ident ts)) =
        diagnostic "SE-007" "invalid parameter type"
            ("Parameter \x1b[31m" <> T.pack ident <> "\x1b[0m has an invalid type \x1b[31m" <> showText ts <> "\x1b[0m.")
    describe (EInvalidReturnType ty) =
        diagnostic "SE-008" "invalid return type"
            ("Invalid return type \x1b[31m" <> showText ty <> "\x1b[0m.")
    describe (EProcedureCallExtraArgs (procId, params, procPos) numArgs) =
        relatedTo procPos "\nThe interface of the procedure is defined here" $
        diagnostic "SE-009" "extra arguments in procedure call"
            ("Procedure \x1b[31m" <> T.pack procId <> "\x1b[0m has only \x1b[31m" <> T.pack (show (length params)) <> "\x1b[0m parameters but you are providing \x1b[31m" <> T.pack (show numArgs) <> "\x1b[0m.")
    describe (EProcedureCallMissingArgs (ident, params, procPos) numArgs) =
        relatedTo procPos ("Procedure \x1b[31m" <> T.pack ident <> "\x1b[0m is defined here:") $
        diagnostic "SE-010" "missing arguments in procedure call"
            ("Procedure \x1b[31m" <> T.pack ident <> "\x1b[0m has \x1b[31m" <> T.pack (show (length params)) <> "\x1b[0m parameters but you are providing only \x1b[31m" <> T.pack (show numArgs) <> "\x1b[0m.")
    describe (EProcedureCallArgTypeMismatch (ident, Parameter _ expectedTy, procPos) numArgs actualTy) =
        relatedTo procPos ("Procedure \x1b[31m" <> T.pack ident <> "\x1b[0m is defined here:") $
        diagnostic "SE-011" "argument type mismatch in procedure call"
            ("Argument \x1b[31m#" <> T.pack (show numArgs) <> "\x1b[0m of procedure \x1b[31m" <> T.pack ident <> "\x1b[0m is expected to be of type \x1b[31m" <> showText expectedTy <> "\x1b[0m but you are providing it of type \x1b[31m" <> showText actualTy <> "\x1b[0m.")
    describe (EUnknownProcedure ident) =
        diagnostic "SE-012" "unknown procedure"
            ("Unknown procedure \x1b[31m" <> T.pack ident <> "\x1b[0m.")
    describe (EResourceClassNoProvides ident) =
        diagnostic "SE-013" "resource class does not provide any interface"
            ("Resource class \x1b[31m" <> T.pack ident <> "\x1b[0m does not provide any interface.\n" <> "A resource class must provide at least one interface.")
    describe (EResourceClassAction (classId, clsPos) ident) =
        relatedTo clsPos "the class is defined here" $
        diagnostic "SE-014" "resource class defines an action"
            ("Resource class \x1b[31m" <> T.pack classId <> "\x1b[0m defines the action \x1b[31m" <> T.pack ident <> "\x1b[0m.\n" <> "Resource classes cannot define actions.")
    describe (EResourceClassInPort (classId, clsPos) ident) =
        relatedTo clsPos "the class is defined here" $
        diagnostic "SE-015" "resource class defines an in port"
            ("Resource class \x1b[31m" <> T.pack classId <> "\x1b[0m defines the in port \x1b[31m" <> T.pack ident <> "\x1b[0m.\n" <> "Resource classes cannot define in ports.")
    describe (EResourceClassOutPort (classId, clsPos) ident) =
        relatedTo clsPos "the class is defined here" $
        diagnostic "SE-016" "resource class defines an out port"
            ("Resource class \x1b[31m" <> T.pack classId <> "\x1b[0m defines the out port \x1b[31m" <> T.pack ident <> "\x1b[0m.\n" <> "Resource classes cannot define out ports.")
    describe (EInterfaceNotFound ident) =
        diagnostic "SE-017" "interface not found"
            ("Interface \x1b[31m" <> T.pack ident <> "\x1b[0m not found.")
    describe (EGlobalNotInterface ident) =
        diagnostic "SE-018" "identifier not an interface"
            ("Identifier \x1b[31m" <> T.pack ident <> "\x1b[0m is not an interface.")
    describe (EProcedureNotFromProvidedInterfaces (classId, clsPos) ident) =
        relatedTo clsPos "the class is defined here" $
        diagnostic "SE-019" "procedure not from provided interfaces"
            ("The procedure \x1b[31m" <> T.pack ident <> "\x1b[0m does not belong to any of the provided interfaces of resource class \x1b[31m" <> T.pack classId <> "\x1b[0m.")
    describe (EMissingProcedure ifaceId procId) =
        diagnostic "SE-020" "missing procedure"
            ("Procedure \x1b[31m" <> T.pack procId <> "\x1b[0m of interface \x1b[31m" <> T.pack ifaceId <> "\x1b[0m is not being provided.")
    describe (EProcedureExtraParams (ifaceId, procId, params, procPos) paramNumber) =
        relatedTo procPos "the interface of the procedure is defined here" $
        diagnostic "SE-021" "extra parameters in procedure definition"
            ("Procedure \x1b[31m" <> T.pack procId <> "\x1b[0m of interface \x1b[31m" <> T.pack ifaceId <> "\x1b[0m has only \x1b[31m" <> T.pack (show (length params)) <> "\x1b[0m parameters but you are providing \x1b[31m" <> T.pack (show paramNumber) <> "\x1b[0m.\n")
    describe (EProcedureMissingParams (ifaceId, procId, params, procPos) paramNumber) =
        relatedTo procPos "the interface of the procedure is defined here" $
        diagnostic "SE-022" "missing parameters in procedure definition"
            ("Procedure \x1b[31m" <> T.pack procId <> "\x1b[0m of interface \x1b[31m" <> T.pack ifaceId <> "\x1b[0m has \x1b[31m" <> T.pack (show (length params)) <> "\x1b[0m parameters but you are providing only \x1b[31m" <> T.pack (show paramNumber) <> "\x1b[0m.\n")
    describe (EProcedureParamTypeMismatch (ifaceId, procId, expectedTy, procPos) actualTy) =
        relatedTo procPos ("The procedure \x1b[31m" <> T.pack procId <> "\x1b[0m of the interface \x1b[31m" <> T.pack ifaceId <> "\x1b[0m is defined here:") $
        diagnostic "SE-023" "parameter type mismatch in procedure definition"
            ("Parameter is expected to be of type \x1b[31m" <> showText expectedTy <> "\x1b[0m but you are defining it of type \x1b[31m" <> showText actualTy <> "\x1b[0m.\n")
    describe (ETaskClassProvides ident) =
        diagnostic "SE-024" "task class provides an interface"
            ("Task class \x1b[31m" <> T.pack ident <> "\x1b[0m provides an interface.\n" <> "Task classes must not provide any interface.")
    describe (ETaskClassProcedure (classId, clsPos) ident) =
        relatedTo clsPos "the class is defined here" $
        diagnostic "SE-025" "task class defines a procedure"
            ("Task class \x1b[31m" <> T.pack classId <> "\x1b[0m defines the procedure \x1b[31m" <> T.pack ident <> "\x1b[0m.\n" <> "Task classes cannot define procedures.")
    describe (ETaskClassNoActions ident) =
        diagnostic "SE-026" "task class does not define any actions"
            ("Task class \x1b[31m" <> T.pack ident <> "\x1b[0m does not define any actions.\n" <> "Task classes must define at least one action.")
    describe (EHandlerClassProvides ident) =
        diagnostic "SE-027" "handler class provides an interface"
            ("Handler class \x1b[31m" <> T.pack ident <> "\x1b[0m provides an interface.\n" <> "Handler classes must not provide any interface.")
    describe (EHandlerClassProcedure (classId, clsPos) ident) =
        relatedTo clsPos "the class is defined here" $
        diagnostic "SE-028" "handler class defines a procedure"
            ("Handler class \x1b[31m" <> T.pack classId <> "\x1b[0m defines the procedure \x1b[31m" <> T.pack ident <> "\x1b[0m.\n" <> "Handler classes cannot define procedures.")
    describe (EHandlerClassNoAction ident) =
        diagnostic "SE-029" "handler class does not define any actions"
            ("Handler class \x1b[31m" <> T.pack ident <> "\x1b[0m does not define any actions.\n" <> "Handler classes must define exactly one action.")
    describe (EHandlerClassMultipleActions classId prevActPos) =
        relatedTo prevActPos "another action is defined here" $
        diagnostic "SE-030" "handler class defines multiple actions"
            ("Handler class \x1b[31m" <> T.pack classId <> "\x1b[0m defines multiple actions.\n")
    describe (EHandlerClassNoSinkPort classId) =
        diagnostic "SE-031" "handler class does not define any sink port"
            ("Handler class \x1b[31m" <> T.pack classId <> "\x1b[0m does not define any sink port.\n" <> "Handler classes must define exactly one sink port.")
    describe (EHandlerClassMultipleSinkPorts classId prevPortPos) =
        relatedTo prevPortPos "another sink port is defined here" $
        diagnostic "SE-032" "handler class defines multiple sink ports"
            ("Handler class \x1b[31m" <> T.pack classId <> "\x1b[0m defines multiple sink ports.\n")
    describe (EHandlerClassInPort (classId, clsPos) ident) =
        relatedTo clsPos "the class is defined here" $
        diagnostic "SE-033" "handler class defines an in port"
            ("Handler class \x1b[31m" <> T.pack classId <> "\x1b[0m defines the in port \x1b[31m" <> T.pack ident <> "\x1b[0m.\n" <> "Handler classes cannot define in ports.")
    describe (EIfElseIfCondNotBool ts) =
        diagnostic "SE-034" "if-else-if condition not boolean"
            ("The condition in the statement is expected to be of type \x1b[31mbool\x1b[0m but it is of type \x1b[31m" <> showText ts <> "\x1b[0m.")
    describe (EFunctionCallExtraArgs (funcId, params, funcPos) argNumber) =
        relatedTo funcPos ("Function \x1b[31m" <> T.pack funcId <> "\x1b[0m is defined here:") $
        diagnostic "SE-035" "extra arguments in function call"
            ("Function \x1b[31m" <> T.pack funcId <> "\x1b[0m has only \x1b[31m" <> T.pack (show (length params)) <> "\x1b[0m parameters but you are providing \x1b[31m" <> T.pack (show argNumber) <> "\x1b[0m.\n")
    describe (EFunctionCallMissingArgs (funcId, params, funcPos) argNumber) =
        relatedTo funcPos ("Function \x1b[31m" <> T.pack funcId <> "\x1b[0m is defined here:") $
        diagnostic "SE-036" "missing arguments in function call"
            ("Function \x1b[31m" <> T.pack funcId <> "\x1b[0m has \x1b[31m" <> T.pack (show (length params)) <> "\x1b[0m parameters but you are providing only \x1b[31m" <> T.pack (show argNumber) <> "\x1b[0m.\n")
    describe (EFunctionCallArgTypeMismatch (funcId, Parameter _ expectedTy, funcPos) argNumber actualTy) =
        relatedTo funcPos ("Function \x1b[31m" <> T.pack funcId <> "\x1b[0m is defined here:") $
        diagnostic "SE-037" "argument type mismatch in function call"
            ("Argument \x1b[31m#" <> T.pack (show argNumber) <> "\x1b[0m of function \x1b[31m" <> T.pack funcId <> "\x1b[0m is expected to be of type \x1b[31m" <> showText expectedTy <> "\x1b[0m but you are providing it of type \x1b[31m" <> showText actualTy <> "\x1b[0m.\n")
    describe (EMemberAccessNotFunction ident) =
        diagnostic "SE-038" "access to a member that is not a function"
            ("The identifier \x1b[31m" <> T.pack ident <> "\x1b[0m is not a valid member function.")
    describe EMutableReferenceToImmutable =
        diagnostic "SE-039" "mutable reference to immutable object"
            ("You are trying to create a mutable reference to an immutable object.")
    describe EMutableReferenceToPrivate =
        diagnostic "SE-040" "mutable reference to private object"
            ("You are trying to create a mutable reference to a private object.")
    describe (EBinOpExpectedTypeLeft op expectedTy actualTy) =
        diagnostic "SE-041" "binary operation expected type on the left"
            ("The result of the binary operation \x1b[31m" <> showText op <> "\x1b[0m is expected to be of type \x1b[31m" <> showText expectedTy <> "\x1b[0m but the left operand you are providing is of type \x1b[31m" <> showText actualTy <> "\x1b[0m.")
    describe (EBinOpExpectedTypeRight op expectedTy actualTy) =
        diagnostic "SE-042" "binary operation expected type on the right"
            ("The result of the binary operation \x1b[31m" <> showText op <> "\x1b[0m is expected to be of type \x1b[31m" <> showText expectedTy <> "\x1b[0m but the right operand you are providing is of type \x1b[31m" <> showText actualTy <> "\x1b[0m.")
    describe (EBinOpTypeMismatch op ty_le ty_re) =
        diagnostic "SE-043" "binary operation type mismatch"
            ("Binary operation \x1b[31m" <> showText op <> "\x1b[0m expects operands of the same type but the left one is of type \x1b[31m" <> showText ty_le <> "\x1b[0m and the right one is of type \x1b[31m" <> showText ty_re <> "\x1b[0m.")
    describe (EBinOpExpectedTypeNotBool op ty) =
        diagnostic "SE-044" "binary operation expected result type not boolean"
            ("The binary operation \x1b[31m" <> showText op <> "\x1b[0m will result in a value of type \x1b[31m" <> showText (TBool :: TerminaType a) <> "\x1b[0m but it is expected to be of type \x1b[31m" <> showText ty <> "\x1b[0m.")
    describe (EBinOpLeftTypeNotBool op ty) =
        diagnostic "SE-045" "binary operation expected boolean type on the left"
            ("The left operand of the binary operation \x1b[31m" <> showText op <> "\x1b[0m is of type \x1b[31m" <> showText ty <> "\x1b[0m but it is expected to be of type \x1b[31m" <> showText (TBool :: TerminaType a) <> "\x1b[0m.")
    describe (EBinOpRightTypeNotBool op ty) =
        diagnostic "SE-046" "binary operation expected boolean type on the right"
            ("The right operand of the binary operation \x1b[31m" <> showText op <> "\x1b[0m is of type \x1b[31m" <> showText ty <> "\x1b[0m but it is expected to be of type \x1b[31m" <> showText (TBool :: TerminaType a) <> "\x1b[0m.")
    describe (EBinOpExpectedTypeNotArith op ty) =
        diagnostic "SE-047" "binary operation expected result type not arithmetic"
            ("The binary operation \x1b[31m" <> showText op <> "\x1b[0m will result in an arithmetic value but the expected type is \x1b[31m" <> showText ty <> "\x1b[0m.")
    describe (EBinOpLeftTypeNotArith op ty) =
        diagnostic "SE-048" "binary operation expected arithmetic type on the left"
            ("The left operand of the binary operation \x1b[31m" <> showText op <> "\x1b[0m is of type \x1b[31m" <> showText ty <> "\x1b[0m but it is expected to be of arithmetic type (integer or float).")
    describe (EBinOpRightTypeNotArith op ty) =
        diagnostic "SE-049" "binary operation expected arithmetic type on the right"
            ("The right operand of the binary operation \x1b[31m" <> showText op <> "\x1b[0m is of type \x1b[31m" <> showText ty <> "\x1b[0m but it is expected to be of arithmetic type (integer or float).")
    describe (EBinOpExpectedTypeNotInt op ty) =
        diagnostic "SE-214" "binary operation expected result type not integer"
            ("The binary operation \x1b[31m" <> showText op <> "\x1b[0m will result in an integer value but the expected type is \x1b[31m" <> showText ty <> "\x1b[0m.")
    describe (EBinOpLeftTypeNotInt op ty) =
        diagnostic "SE-215" "binary operation expected integer type on the left"
            ("The left operand of the binary operation \x1b[31m" <> showText op <> "\x1b[0m is of type \x1b[31m" <> showText ty <> "\x1b[0m but it is expected to be of integer type.")
    describe (EBinOpRightTypeNotInt op ty) =
        diagnostic "SE-216" "binary operation expected integer type on the right"
            ("The right operand of the binary operation \x1b[31m" <> showText op <> "\x1b[0m is of type \x1b[31m" <> showText ty <> "\x1b[0m but it is expected to be of integer type.")
    describe (EBinOpRightTypeNotPos op ty) =
        diagnostic "SE-050" "binary operation expected positive numeric type on the right"
            ("The right operand of the binary operation \x1b[31m" <> showText op <> "\x1b[0m is of type \x1b[31m" <> showText ty <> "\x1b[0m but it is expected to be of positive numeric type.")
    describe (EBinOpLeftTypeNotEq op ty) =
        diagnostic "SE-051" "binary operation expected equatable type on the left"
            ("The left operand of the binary operation \x1b[31m" <> showText op <> "\x1b[0m is of type \x1b[31m" <> showText ty <> "\x1b[0m but it is expected to be of equatable type.")
    describe (EBinOpRightTypeNotEq op ty) =
        diagnostic "SE-052" "binary operation expected equatable type on the right"
            ("The right operand of the binary operation \x1b[31m" <> showText op <> "\x1b[0m is of type \x1b[31m" <> showText ty <> "\x1b[0m but it is expected to be of equatable type.")
    describe (EAtomicAccessInvalidType ty) =
        diagnostic "SE-053" "invalid type for the atomic access interface"
            ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not valid for atomic access, only numeric types are allowed.")
    describe (EAtomicArrayAccessInvalidType ty) =
        diagnostic "SE-054" "invalid type for the atomic array access interface"
            ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not valid for atomic array access, only numeric types are allowed.")
    describe (EAtomicInvalidType ty) =
        diagnostic "SE-055" "invalid atomic type"
            ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not valid for atomic.")
    describe (EAtomicArrayInvalidType ty) =
        diagnostic "SE-056" "invalid atomic array type"
            ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not valid for atomic array.")
    describe (EAtomicConnectionTypeMismatch expectedTy actualTy) =
        diagnostic "SE-057" "atomic connection type mismatch"
            ("The type of the connected atomic resource is expected to be \x1b[31m" <> showText expectedTy <> "\x1b[0m but it is of type \x1b[31m" <> showText actualTy <> "\x1b[0m.")
    describe (EAtomicArrayConnectionTypeMismatch expectedTy actualTy) =
        diagnostic "SE-058" "atomic array connection type mismatch"
            ("The type of the elements of the connected atomic array is expected to be \x1b[31m" <> showText expectedTy <> "\x1b[0m but the array is of elements of type \x1b[31m" <> showText actualTy <> "\x1b[0m.")
    describe EInvalidDefaultCase =
        diagnostic "SE-059" "unnecessary default case"
            ("The cases are already exhaustive, the default case is not needed.")
    describe (EConstantWithoutKnownType c) =
        diagnostic "SE-060" "constant without known type"
            ("The type of the constant \x1b[31m" <> showText c <> "\x1b[0m cannot be inferred from the environment and must be explicitly defined.")
    describe EStructInitializerInvalidUse =
        diagnostic "SE-061" "invalid use of struct initializer"
            ("You are trying to use a struct initializer in an invalid context.\n" <> "Struct initializers can only be used to initialize struct objects.")
    describe (EStructInitializerTypeMismatch expectedTy actualTy) =
        diagnostic "SE-062" "struct initializer type mismatch"
            ("The struct initializer is expected to be of type \x1b[31m" <> showText expectedTy <> "\x1b[0m but it is of type \x1b[31m" <> showText actualTy <> "\x1b[0m.")
    describe (EEnumInitializerExpectedTypeMismatch expectedTy actualTy) =
        diagnostic "SE-063" "enum initializer expected type mismatch"
            ("The enum initializer is expected to be of type \x1b[31m" <> showText expectedTy <> "\x1b[0m but it is of type \x1b[31m" <> showText actualTy <> "\x1b[0m.")
    describe ESliceInvalidUse =
        diagnostic "SE-064" "invalid use of slice"
            ("You are trying to use a slice in an invalid context.\n" <> "Slices can only be used to create references to a part of an array.")
    describe EArrayInitializerInvalidUse =
        diagnostic "SE-065" "invalid use of an array initializer"
            ("You are trying to use an array initializer in an invalid context.\n" <> "Array initializers can only be used to initialize array objects.")
    describe (EArrayInitializerNotArray ty) =
        diagnostic "SE-066" "assignment of an array initializer to a non-array type"
            ("Invalid use of an array initializer.\n" <> "You are trying to assign an array initializer to an object of type \x1b[31m" <> showText ty <> "\x1b[0m.")
    describe EArrayExprListInitializerInvalidUse =
        diagnostic "SE-067" "invalid use of an expression list array initializer"
            ("You are trying to use an array expression list initializer in an invalid context.\n" <> "TArray expression list initializers can only be used to initialize array objects.")
    describe (EArrayExprListInitializerNotArray ty) =
        diagnostic "SE-068" "assignment of an array expression list initializer to a non-array type"
            ("Invalid use of an array expression list initializer.\n" <> "You are trying to assign an array expression list initializer to an object of type \x1b[31m" <> showText ty <> "\x1b[0m.")
    describe EMonadicVariantInitializerInvalidUse =
        diagnostic "SE-069" "invalid use of an builtin variant initializer"
            ("You are trying to use an variant initializer for a builtin type in an invalid context.\n" <> "Variant initializers can only be used to initialize objects.")
    describe (EForLoopLowerBoundTypeMismatch expectedTy actualTy) =
        diagnostic "SE-070" "for loop lower bound type mismatch"
            ("The lower bound of the for loop is expected to be of the type of the iterator \x1b[31m" <> showText expectedTy <> "\x1b[0m but it is of type \x1b[31m" <> showText actualTy <> "\x1b[0m.")
    describe (EForLoopUpperBoundTypeMismatch expectedTy actualTy) =
        diagnostic "SE-071" "for loop upper bound type mismatch"
            ("The upper bound of the for loop is expected to be of the type of the iterator \x1b[31m" <> showText expectedTy <> "\x1b[0m but it is of type \x1b[31m" <> showText actualTy <> "\x1b[0m.")
    describe (EArrayExprListInitializerExprTypeMismatch expectedTy actualTy) =
        diagnostic "SE-072" "list of initializing expressions type mismatch"
            ("The expression in the array expression list initializer is expected to be of type \x1b[31m" <> showText expectedTy <> "\x1b[0m but it is of type \x1b[31m" <> showText actualTy <> "\x1b[0m.")
    describe (EReturnValueExpected ty) =
        diagnostic "SE-073" "expected return value"
            ("The function is expected to return a value of type \x1b[31m" <> showText ty <> "\x1b[0m.")
    describe EReturnValueNotUnit =
        diagnostic "SE-074" "return value not expected"
            ("The function is not expected to return a value.")
    describe (EInvalidArrayType ty) =
        diagnostic "SE-075" "invalid array type"
            ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid array type.")
    describe (EInvalidBoxType ty) =
        diagnostic "SE-076" "invalid box type"
            ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid box type.")
    describe (ENoTypeFound ident) =
        diagnostic "SE-077" "no type found"
            ("The type \x1b[31m" <> T.pack ident <> "\x1b[0m is not found.")
    describe (EGlobalNotType (ident, globalPos)) =
        relatedTo globalPos "the global object is defined here" $
        diagnostic "SE-078" "global object but not a type"
            ("The global object \x1b[31m" <> T.pack ident <> "\x1b[0m is not a type.\n")
    describe (EInvalidAccessToGlobal ident) =
        diagnostic "SE-079" "invalid access to global object"
            ("The global object \x1b[31m" <> T.pack ident <> "\x1b[0m cannot be accessed from within this context.")
    describe (EConstantIsReadOnly ident) =
        diagnostic "SE-080" "invalid write to a constant"
            ("The constant \x1b[31m" <> T.pack ident <> "\x1b[0m is read-only and cannot be modified.")
    describe (ESymbolAlreadyDefined (ident, symbolPos)) =
        relatedTo symbolPos "the symbol was previoulsy defined here" $
        diagnostic "SE-081" "symbol already defined"
            ("The symbol \x1b[31m" <> T.pack ident <> "\x1b[0m is already defined.\n")
    describe EContinueInvalidExpression =
        diagnostic "SE-082" "invalid expression in continue statement"
            ("The expression in a continue statement must be a call to a member action.")
    describe (EContinueInvalidMethodOrViewerCall ident) =
        diagnostic "SE-083" "invalid method or viewer call in continue statement"
            ("This statement can only be used to call a continuation action.\n" <> "The member function call \x1b[31m" <> T.pack ident <> "\x1b[0m in a continue statement is invalid.")
    describe (EContinueInvalidMemberCall ts) =
        diagnostic "SE-084" "invalid member call in continue statement"
            ("This statement can only be used to call a continuation action.\n" <> "Calling a procedure of an object of type \x1b[31m" <> showText ts <> "\x1b[0m in a continue statement is invalid.")
    describe (EContinueActionExtraArgs (ident, params, actionPos) argNumber) =
        relatedTo actionPos "the action is defined here" $
        diagnostic "SE-085" "extra arguments in continuation action"
            ("Action \x1b[31m" <> T.pack ident <> "\x1b[0m has only \x1b[31m" <> T.pack (show (length params)) <> "\x1b[0m parameters but you are providing \x1b[31m" <> T.pack (show argNumber) <> "\x1b[0m.\n")
    describe (EContinueActionMissingArgs (ident, actionPos)) =
        relatedTo actionPos "the action is defined here" $
        diagnostic "SE-086" "missing arguments in continuation action"
            ("Action \x1b[31m" <> T.pack ident <> "\x1b[0m requires \x1b[31mone\x1b[0m parameter but you are providing \x1b[31mnone\x1b[0m.\n")
    describe EEnumVariantInitializerInvalidUse =
        diagnostic "SE-087" "invalid use of an enum variant initializer"
            ("You are trying to use an enum variant initializer in an invalid context.\n" <> "Enum variant initializers can only be used to initialize enum objects.")
    describe (EEnumVariantNotFound enumId variant) =
        diagnostic "SE-088" "enum variant not found"
            ("Enum \x1b[31m" <> T.pack enumId <> "\x1b[0m does not have a variant named \x1b[31m" <> T.pack variant <> "\x1b[0m.")
    describe (EEnumVariantExtraParams (enumId, enumPos) (variant, params) paramNumber) =
        relatedTo enumPos "the enum is defined here" $
        diagnostic "SE-089" "extra parameters in enum variant"
            ("Enum variant \x1b[31m" <> T.pack variant <> "\x1b[0m of enum \x1b[31m" <> T.pack enumId <> "\x1b[0m has only \x1b[31m" <> T.pack (show (length params)) <> "\x1b[0m parameters but you are providing \x1b[31m" <> T.pack (show paramNumber) <> "\x1b[0m.\n")
    describe (EEnumVariantMissingParams (enumId, enumPos) (variant, params) paramNumber) =
        relatedTo enumPos "the enum is defined here" $
        diagnostic "SE-090" "missing parameters in enum variant"
            ("Enum variant \x1b[31m" <> T.pack variant <> "\x1b[0m of enum \x1b[31m" <> T.pack enumId <> "\x1b[0m has \x1b[31m" <> T.pack (show (length params)) <> "\x1b[0m parameters but you are providing only \x1b[31m" <> T.pack (show paramNumber) <> "\x1b[0m.\n")
    describe (EEnumVariantParamTypeMismatch (enumId, enumPos) (variant, paramNumber, expectedTy) actualTy) =
        relatedTo enumPos "the enum is defined here" $
        diagnostic "SE-091" "enum variant parameter type mismatch"
            ("Parameter \x1b[31m" <> T.pack (show paramNumber) <> "\x1b[0m of enum variant \x1b[31m" <> T.pack variant <> "\x1b[0m of enum \x1b[31m" <> T.pack enumId <> "\x1b[0m is expected to be of type \x1b[31m" <> showText expectedTy <> "\x1b[0m but it is of type \x1b[31m" <> showText actualTy <> "\x1b[0m.\n")
    describe (EFunctionNotFound ident) =
        diagnostic "SE-092" "function not found"
            ("Function \x1b[31m" <> T.pack ident <> "\x1b[0m not found.")
    describe (EGlobalNotFunction (ident, globalPos)) =
        relatedTo globalPos "the global object is defined here" $
        diagnostic "SE-093" "global object but not a function"
            ("The global object \x1b[31m" <> T.pack ident <> "\x1b[0m is not a function.\n")
    describe (EUnexpectedNumericConstant ty) =
        diagnostic "SE-094" "unexpected numeric constant"
            ("Expected a value of type \x1b[31m" <> showText ty <> "\x1b[0m but found a numeric constant.")
    describe (EInvalidAssignmentExprType ty) =
        diagnostic "SE-095" "invalid assignment expression type"
            ("Objects of type \x1b[31m" <> showText ty <> "\x1b[0m cannot be copied.")
    describe (EInvalidMessageType ty) =
        diagnostic "SE-096" "invalid message type"
            ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid message type.")
    describe (EInvalidOptionType ty) =
        diagnostic "SE-097" "invalid option type"
            ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid option type.")
    describe (EInvalidReferenceType ty) =
        diagnostic "SE-098" "invalid reference type"
            ("References to objects of type \x1b[31m" <> showText ty <> "\x1b[0m cannot be created.")
    describe (EInvalidFixedLocationType ty) =
        diagnostic "SE-099" "invalid fixed-location type"
            ("Fixed-location fields of type \x1b[31m" <> showText ty <> "\x1b[0m cannot be defined.")
    describe (EInvalidAllocatorType ty) =
        diagnostic "SE-100" "invalid allocator type"
            ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid allocator type.")
    describe (EInvalidClassFieldType ty) =
        diagnostic "SE-101" "invalid class field type"
            ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid class field type.")
    describe (EInvalidStructFieldType ty) =
        diagnostic "SE-102" "invalid struct field type"
            ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid struct field type.")
    describe (EInvalidEnumParameterType ty) =
        diagnostic "SE-103" "invalid enum parameter type"
            ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid parameter type for an enum variant.")
    describe (EInvalidAccessPortType ty) =
        diagnostic "SE-104" "invalid access port type"
            ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid access port type.")
    describe (EInvalidDeclarationType ty) =
        diagnostic "SE-105" "invalid declaration type"
            ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid object declaration type.")
    describe (EInvalidTypeSpecifier ts) =
        diagnostic "SE-106" "invalid type specifier"
            ("The type specifier \x1b[31m" <> showText ts <> "\x1b[0m is not valid.")
    describe (EInvalidNumericConstantType ty) =
        diagnostic "SE-107" "invalid numeric constant type"
            ("The expected type of this expression is \x1b[31m" <> showText ty <> "\x1b[0m but it is a numeric constant.")
    describe (EInvalidActionParameterType ty) =
        diagnostic "SE-108" "invalid action parameter type"
            ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid parameter type for an action.")
    describe (EInvalidProcedureParameterType ty) =
        diagnostic "SE-109" "invalid procedure parameter type"
            ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid parameter type for a procedure.")
    describe (EMemberFunctionCallExtraArgs (funcId, params, funcPos) argNumber) =
        relatedTo funcPos ("Member function \x1b[31m" <> T.pack funcId <> "\x1b[0m is defined here:") $
        diagnostic "SE-110" "extra arguments in member function call"
            ("Member function \x1b[31m" <> T.pack funcId <> "\x1b[0m has only \x1b[31m" <> T.pack (show (length params)) <> "\x1b[0m parameters but you are providing \x1b[31m" <> T.pack (show argNumber) <> "\x1b[0m.\n")
    describe (EMemberFunctionCallMissingArgs (funcId, params, funcPos) argNumber) =
        relatedTo funcPos ("Member function \x1b[31m" <> T.pack funcId <> "\x1b[0m is defined here:") $
        diagnostic "SE-111" "missing arguments in member function call"
            ("Member function \x1b[31m" <> T.pack funcId <> "\x1b[0m has \x1b[31m" <> T.pack (show (length params)) <> "\x1b[0m parameters but you are providing only \x1b[31m" <> T.pack (show argNumber) <> "\x1b[0m.")
    describe (EMemberFunctionCallArgTypeMismatch (funcId, Parameter _ expectedTy, funcPos) argNumber actualTy) =
        relatedTo funcPos ("Member function \x1b[31m" <> T.pack funcId <> "\x1b[0m is defined here:") $
        diagnostic "SE-112" "member function call argument type mismatch"
            ("Argument \x1b[31m#" <> T.pack (show argNumber) <> "\x1b[0m of member function \x1b[31m" <> T.pack funcId <> "\x1b[0m is expected to be of type \x1b[31m" <> showText expectedTy <> "\x1b[0m but it is of type \x1b[31m" <> showText actualTy <> "\x1b[0m.\n")
    describe (EArrayIndexNotUSize ty) =
        diagnostic "SE-113" "invalid array index type"
            ("The type of the array index is \x1b[31m" <> showText ty <> "\x1b[0m but it is expected to be of type \x1b[31m" <> showText (TUSize :: TerminaType a) <> "\x1b[0m.")
    describe (EArraySliceLowerBoundNotUSize ty) =
        diagnostic "SE-114" "invalid array slice lower bound type"
            ("The type of the lower bound of the array slice is \x1b[31m" <> showText ty <> "\x1b[0m but it is expected to be of type \x1b[31m" <> showText (TUSize :: TerminaType a) <> "\x1b[0m.")
    describe (EArraySliceUpperBoundNotUSize ty) =
        diagnostic "SE-115" "invalid array slice upper bound type"
            ("The type of the upper bound of the array slice is \x1b[31m" <> showText ty <> "\x1b[0m but it is expected to be of type \x1b[31m" <> showText (TUSize :: TerminaType a) <> "\x1b[0m.")
    describe (EOutboundPortSendInvalidNumArgs argNumber) =
        diagnostic "SE-116" "invalid number of arguments in outbound port send"
            ("The send procedure of an outbound port expects \x1b[31mone\x1b[0m argument but you are providing \x1b[31m" <> T.pack (show argNumber) <> "\x1b[0m.")
    describe (EOutboundPortArgTypeMismatch expectedTy actualTy) =
        diagnostic "SE-117" "output port argument type mismatch"
            ("The output data is expected to be of type \x1b[31m" <> showText expectedTy <> "\x1b[0m but you are sending data of type \x1b[31m" <> showText actualTy <> "\x1b[0m.")
    describe (EAssignmentExprMismatch expectedTy actualTy) =
        diagnostic "SE-118" "assignment expression type mismatch"
            ("The expected type of the assignment is \x1b[31m" <> showText expectedTy <> "\x1b[0m but it is of type \x1b[31m" <> showText actualTy <> "\x1b[0m.")
    describe (EFieldValueAssignmentMissingFields (record, recordPos) [field]) =
        relatedTo recordPos ("\nThe type \x1b[31m" <> showText record <> "\x1b[0m is defined here:") $
        diagnostic "SE-119" "missing field/s in field assignment expression"
            ("Field \x1b[31m" <> T.pack field <> "\x1b[0m is not being assigned a value in the field assignment expression.")
    describe (EFieldValueAssignmentMissingFields (record, recordPos) fields) =
        relatedTo recordPos ("\nThe type \x1b[31m" <> showText record <> "\x1b[0m is defined here:") $
        diagnostic "SE-119" "missing field/s in field assignment expression"
            ("Fields \x1b[31m" <> T.intercalate ", " (map T.pack fields) <> "\x1b[0m are not being assigned a value in the field assignment expression.")
    describe (EFieldValueAssignmentUnknownFields (record, recordPos) [field]) =
        relatedTo recordPos ("\nThe type \x1b[31m" <> showText record <> "\x1b[0m is defined here:") $
        diagnostic "SE-120" "unknown field/s in field assignment expression"
            ("Field \x1b[31m" <> T.pack field <> "\x1b[0m is not a field of the type \x1b[31m" <> showText record <> "\x1b[0m.")
    describe (EFieldValueAssignmentUnknownFields (record, recordPos) fields) =
        relatedTo recordPos ("\nThe type \x1b[31m" <> showText record <> "\x1b[0m is defined here:") $
        diagnostic "SE-120" "unknown field/s in field assignment expression"
            ("Fields \x1b[31m" <> T.intercalate ", " (map T.pack fields) <> "\x1b[0m are not fields of the type \x1b[31m" <> showText record <> "\x1b[0m.")
    describe (EFieldNotFixedLocation fieldName ty) =
        diagnostic "SE-121" "field is not a fixed-location field"
            ("Field \x1b[31m" <> T.pack fieldName <> "\x1b[0m of type \x1b[31m" <> showText ty <> "\x1b[0m is not a fixed-location field.")
    describe (EFieldNotAccessPort fieldName ty) =
        diagnostic "SE-122" "field is not an access port field"
            ("Field \x1b[31m" <> T.pack fieldName <> "\x1b[0m of type \x1b[31m" <> showText ty <> "\x1b[0m is not an access port field.")
    describe (EFieldNotSinkOrInboundPort fieldName ty) =
        diagnostic "SE-123" "field is not a sink or inbound port field"
            ("Field \x1b[31m" <> T.pack fieldName <> "\x1b[0m of type \x1b[31m" <> showText ty <> "\x1b[0m is not a sink or inbound port field.")
    describe (EFieldNotOutboundPort fieldName ty) =
        diagnostic "SE-124" "field is not an outbound port field"
            ("Field \x1b[31m" <> T.pack fieldName <> "\x1b[0m of type \x1b[31m" <> showText ty <> "\x1b[0m is not an outbound port field.")
    describe (EMemberAccessInvalidType ty) =
        diagnostic "SE-125" "invalid member access type"
            ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid type for member access.")
    describe (EMemberFunctionCallInvalidType ty) =
        diagnostic "SE-126" "invalid member function call type"
            ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid type for member function call.")
    describe (EMemberAccessUnknownField (recordId, recordPos) field) =
        relatedTo recordPos ("The type \x1b[31m" <> T.pack recordId <> "\x1b[0m is defined here:") $
        diagnostic "SE-127" "unknown field in member access"
            ("Field \x1b[31m" <> T.pack field <> "\x1b[0m is not a field of the type \x1b[31m" <> T.pack recordId <> "\x1b[0m.\n")
    describe EInvalidProcedureCallInsideMemberFunction =
        diagnostic "SE-128" "invalid procedure call inside member function"
            ("Procedure calls are not allowed inside member functions.")
    describe (EConstantOutRange ty) =
        diagnostic "SE-129" "constant out of range"
            ("The constant value \x1b[31m" <> showText ty <> "\x1b[0m is out of range for its type.")
    describe (EForIteratorInvalidType ty) =
        diagnostic "SE-130" "invalid type for for-loop iterator"
            ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid type for a for-loop iterator.")
    describe (EUsedTypeName ident prevPos) =
        relatedTo prevPos "the symbol is previously used here" $
        diagnostic "SE-131" "type name already used"
            ("The type cannot be defined because the symbol \x1b[31m" <> T.pack ident <> "\x1b[0m is already in use.\n")
    describe (EUsedGlobalName ident prevPos) =
        relatedTo prevPos "the symbol is previously used here" $
        diagnostic "SE-132" "global name already used"
            ("The global object cannot be declared because the symbol \x1b[31m" <> T.pack ident <> "\x1b[0m is already in use.\n")
    describe (EUsedFunName ident prevPos) =
        relatedTo prevPos "the symbol is previously used here" $
        diagnostic "SE-133" "function name already used"
            ("The function cannot be declared because the symbol \x1b[31m" <> T.pack ident <> "\x1b[0m is already in use.\n")
    describe (EAccessPortConnectionInvalidGlobal ident) =
        diagnostic "SE-134" "invalid global object in access port connection"
            ("The global object \x1b[31m" <> T.pack ident <> "\x1b[0m cannot be used in an access port connection.")
    describe (EAccessPortConnectionInterfaceNotProvided ident iface) =
        diagnostic "SE-135" "resource does not provide the interface"
            ("Resource \x1b[31m" <> T.pack ident <> "\x1b[0m does not provide the interface \x1b[31m" <> T.pack iface <> "\x1b[0m.")
    describe (ESinkPortConnectionInvalidGlobal ident) =
        diagnostic "SE-136" "invalid sink port connection"
            ("The global object \x1b[31m" <> T.pack ident <> "\x1b[0m cannot be connected to a sink port.")
    describe (EInboundPortConnectionInvalidObject ident) =
        diagnostic "SE-137" "invalid inbound port connection"
            ("The object \x1b[31m" <> T.pack ident <> "\x1b[0m cannot be connected to an inbound port.")
    describe (EOutboundPortConnectionInvalidGlobal ident) =
        diagnostic "SE-138" "invalid outbound port connection"
            ("The global object \x1b[31m" <> T.pack ident <> "\x1b[0m cannot be connected to an outbound port.")
    describe (EAllocatorPortConnectionInvalidGlobal ident) =
        diagnostic "SE-139" "invalid allocator port connection"
            ("The global object \x1b[31m" <> T.pack ident <> "\x1b[0m cannot be connected to an allocator port.")
    describe (EAtomicAccessPortConnectionInvalidGlobal ident) =
        diagnostic "SE-140" "invalid atomic access port connection"
            ("The global object \x1b[31m" <> T.pack ident <> "\x1b[0m cannot be connected to an atomic access port.")
    describe (EAtomicArrayAccessPortConnectionInvalidGlobal ident) =
        diagnostic "SE-141" "invalid atomic array access port connection"
            ("The global object \x1b[31m" <> T.pack ident <> "\x1b[0m cannot be connected to an atomic array access port.")
    describe (EStructDefNotUniqueField [fieldName]) =
        diagnostic "SE-142" "duplicate field in struct definition"
            ("Field \x1b[31m" <> T.pack fieldName <> "\x1b[0m is duplicated in the struct definition.")
    describe (EStructDefNotUniqueField fieldNames) =
        diagnostic "SE-142" "duplicate field in struct definition"
            ("Fields \x1b[31m" <> T.intercalate ", " (map T.pack fieldNames) <> "\x1b[0m are duplicated in the struct definition.")
    describe (EEnumDefNotUniqueVariant [variantName]) =
        diagnostic "SE-143" "duplicate variant in enum definition"
            ("Variant \x1b[31m" <> T.pack variantName <> "\x1b[0m is duplicated in the enum definition.")
    describe (EEnumDefNotUniqueVariant variantNames) =
        diagnostic "SE-143" "duplicate variant in enum definition"
            ("Variants \x1b[31m" <> T.intercalate ", " (map T.pack variantNames) <> "\x1b[0m are duplicated in the enum definition.")
    describe (EInterfaceNotUniqueProcedure [procName]) =
        diagnostic "SE-144" "duplicate procedure in interface definition"
            ("Procedure \x1b[31m" <> T.pack procName <> "\x1b[0m is duplicated in the interface definition.")
    describe (EInterfaceNotUniqueProcedure procNames) =
        diagnostic "SE-144" "duplicate procedure in interface definition"
            ("Procedures \x1b[31m" <> T.intercalate ", " (map T.pack procNames) <> "\x1b[0m are duplicated in the interface definition.")
    describe (EClassLoop _) =
        diagnostic "SE-145" "loop between member function calls in class definition"
            ("A recursive calling loop has been detected in the class definition.")
    describe (EDereferenceInvalidType ty) =
        diagnostic "SE-146" "invalid type for dereference"
            ("The type \x1b[31m" <> showText ty <> "\x1b[0m cannot be dereferenced.")
    describe (EMatchInvalidType ty) =
        diagnostic "SE-147" "invalid type for match statement"
            ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid type for match statement.")
    describe (EMatchCaseDuplicate variantName prevCase) =
        relatedTo prevCase "the variant is previously used here" $
        diagnostic "SE-148" "duplicate case in match statement"
            ("Variant \x1b[31m" <> T.pack variantName <> "\x1b[0m is duplicated in the match statement.\n")
    describe (EMatchCaseUnknownVariant variantName) =
        diagnostic "SE-149" "unknown variant in match case"
            ("Variant \x1b[31m" <> T.pack variantName <> "\x1b[0m is not a valid variant of the enum or option.")
    describe (EMatchMissingCases [caseIdent]) =
        diagnostic "SE-150" "missing case/s in match statement"
            ("Case \x1b[31m" <> T.pack caseIdent <> "\x1b[0m is missing in the match statement.")
    describe (EMatchMissingCases caseIdents) =
        diagnostic "SE-150" "missing case/s in match statement"
            ("Cases \x1b[31m" <> T.intercalate ", " (map T.pack caseIdents) <> "\x1b[0m are missing in the match statement.")
    describe (EIsVariantInvalidType ty) =
        diagnostic "SE-151" "invalid type for is-variant expression"
            ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid type for is-variant expression.")
    describe (EIsOptionVariantInvalidType ty) =
        diagnostic "SE-152" "invalid type for is-option-variant expression"
            ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not an option type.")
    describe (EIsVariantEnumTypeMismatch expectedEnum actualEnum) =
        diagnostic "SE-153" "type mismatch in is-variant expression"
            ("The expected enum type is \x1b[31m" <> T.pack expectedEnum <> "\x1b[0m but the actual type is \x1b[31m" <> T.pack actualEnum <> "\x1b[0m.")
    describe (EOutboundPortInvalidProcedure ident) =
        diagnostic "SE-154" "invalid procedure in outbound port"
            ("The procedure \x1b[31m" <> T.pack ident <> "\x1b[0m is not a valid procedure for an outbound port.")
    describe EInvalidPoolInitialization =
        diagnostic "SE-155" "invalid pool initialization"
            ("A pool object cannot be initialized with a value.")
    describe EInvalidMsgQueueInitialization =
        diagnostic "SE-156" "invalid message queue initialization"
            ("A message queue object cannot be initialized with a value.")
    describe (EUnknownGlobal ident) =
        diagnostic "SE-157" "unknown global object"
            ("Global object \x1b[31m" <> T.pack ident <> "\x1b[0m is not defined.")
    describe (EInvalidInterruptEmitterType ty) =
        diagnostic "SE-158" "invalid interrupt emitter type"
            ("Interrupts emit data of type \x1b[31m" <> showText (TUInt32 :: TerminaType a) <> "\x1b[0m but you are expecting data of type \x1b[31m" <> showText ty <> "\x1b[0m.")
    describe (EInvalidPeriodicTimerEmitterType ty) =
        diagnostic "SE-159" "invalid periodic timer emitter type"
            ("Periodic timers emit data of type \x1b[31m" <> showText (TStruct "TimeVal" :: TerminaType a) <> "\x1b[0m but you are expecting data of type \x1b[31m" <> showText ty <> "\x1b[0m.")
    describe (EInvalidSystemInitEmitterType ty) =
        diagnostic "SE-160" "invalid system init emitter type"
            ("System init emitters emit data of type \x1b[31m" <> showText (TStruct "TimeVal" :: TerminaType a) <> "\x1b[0m but you are expecting data of type \x1b[31m" <> showText ty <> "\x1b[0m.")
    describe (EInboundPortConnectionMsgQueueTypeMismatch msgQueueId expectedTy actualTy) =
        diagnostic "SE-161" "message queue type mismatch"
            ("The message queue \x1b[31m" <> T.pack msgQueueId <> "\x1b[0m exchanges data messages of type \x1b[31m" <> showText expectedTy <> "\x1b[0m but you are expecting data of type \x1b[31m" <> showText actualTy <> "\x1b[0m.")
    describe (EOutboundPortConnectionMsgQueueTypeMismatch msgQueueId expectedTy actualTy) =
        diagnostic "SE-162" "message queue type mismatch"
            ("The message queue \x1b[31m" <> T.pack msgQueueId <> "\x1b[0m exchanges data messages of type \x1b[31m" <> showText expectedTy <> "\x1b[0m but you are sending data of type \x1b[31m" <> showText actualTy <> "\x1b[0m.")
    describe (EAllocatorPortConnectionPoolTypeMismatch poolId expectedTy actualTy) =
        diagnostic "SE-163" "pool type mismatch"
            ("The pool \x1b[31m" <> T.pack poolId <> "\x1b[0m serves data of type \x1b[31m" <> showText expectedTy <> "\x1b[0m but you are expecting data of type \x1b[31m" <> showText actualTy <> "\x1b[0m.")
    describe (EInvalidTaskType ty) =
        diagnostic "SE-164" "invalid task type"
            ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid task type.")
    describe (EInvalidHandlerType ty) =
        diagnostic "SE-165" "invalid handler type"
            ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid handler type.")
    describe (EInvalidResourceType ty) =
        diagnostic "SE-166" "invalid resource type"
            ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid resource type.")
    describe (EInvalidEmitterType ty) =
        diagnostic "SE-167" "invalid emitter type"
            ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid emitter type.")
    describe (EInvalidChannelType ty) =
        diagnostic "SE-168" "invalid channel type"
            ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid channel type.")
    describe (EEmitterClassNotInstantiable ident) =
        diagnostic "SE-169" "emitter class is not instantiable"
            ("Applications cannot instantiate event emitters of class \x1b[31m" <> T.pack ident <> "\x1b[0m.")
    describe (ESingleExpressionTypeNotUnit ty) =
        diagnostic "SE-170" "single expression type is not unit"
            ("Expressions used in single-expression statements must have type \x1b[31m" <> showText (TUnit :: TerminaType a) <> "\x1b[0m but the expression has type \x1b[31m" <> showText ty <> "\x1b[0m. Return values of functions cannot be ignored.")
    describe (EInterfaceDuplicatedExtendedIface ifaceName) =
        diagnostic "SE-171" "interface extends the same interface multiple times"
            ("Interface \x1b[31m" <> T.pack ifaceName <> "\x1b[0m is extended more than once.")
    describe (EInterfaceDuplicatedExtendedProcedure iface1 iface2 procName) =
        diagnostic "SE-172" "procedure duplicated in extended interfaces"
            ("Procedure \x1b[31m" <> T.pack procName <> "\x1b[0m is defined in extended interfaces \x1b[31m" <> T.pack iface1 <> "\x1b[0m and \x1b[31m" <> T.pack iface2 <> "\x1b[0m.")
    describe (EInterfaceProcedurePreviouslyExtended procName ifaceName) =
        diagnostic "SE-173" "interface procedure previously defined by an extended interface"
            ("Procedure \x1b[31m" <> T.pack procName <> "\x1b[0m is previously defined in interface \x1b[31m" <> T.pack ifaceName <> "\x1b[0m.")
    describe (EInterfacePreviouslyExtended iface1 iface2) =
        diagnostic "SE-174" "interface previously extended by another interface"
            ("Interface \x1b[31m" <> T.pack iface1 <> "\x1b[0m is already extended by interface \x1b[31m" <> T.pack iface2 <> "\x1b[0m.")
    describe (EResourceDuplicatedProvidedIface ifaceName) =
        diagnostic "SE-175" "resource provides the same interface multiple times"
            ("Resource provides interface \x1b[31m" <> T.pack ifaceName <> "\x1b[0m more than once.")
    describe (EResourceDuplicatedProvidedProcedure iface1 iface2 procName) =
        diagnostic "SE-176" "procedure duplicated in provided interfaces"
            ("Procedure \x1b[31m" <> T.pack procName <> "\x1b[0m is provided in interfaces \x1b[31m" <> T.pack iface1 <> "\x1b[0m and \x1b[31m" <> T.pack iface2 <> "\x1b[0m.")
    describe (EResourceInterfacePreviouslyExtended iface1 iface2) =
        diagnostic "SE-177" "interface previously extended by another interface"
            ("Interface \x1b[31m" <> T.pack iface1 <> "\x1b[0m is previously extended by interface \x1b[31m" <> T.pack iface2 <> "\x1b[0m.")
    describe EStringInitializerInvalidUse =
        diagnostic "SE-178" "invalid use of a string initializer"
            ("You are trying to use a string initializer in an invalid context.\n" <> "String initializers can only be used to initialize arrays of characters.")
    describe (EStringInitializerNotArrayOfChars ty) =
        diagnostic "SE-180" "assignment of a string array initializer to an invalid type"
            ("Invalid use of a string initializer.\n" <> "You are trying to assign a string initializer to an object of type \x1b[31m" <> showText ty <> "\x1b[0m.")
    describe (EInvalidConstType ty) =
        diagnostic "SE-181" "invalid type for constant"
            ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid type for a constant.\n" <> "Only numeric types, boolean and character types are valid for constants.")
    describe (EInvalidAccessToConstExpr ident) =
        diagnostic "SE-182" "invalid access to a constant expression"
            ("Constant expression \x1b[31m" <> T.pack ident <> "\x1b[0m cannot be accessed in this context.\n")
    describe (EInvalidResultType ty) =
        diagnostic "SE-183" "invalid type for result"
            ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid type for a result.")
    describe (EInvalidStatusType ty) =
        diagnostic "SE-184" "invalid type for status"
            ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid type for a status.")
    describe (EInvalidVariantForOption variantName) =
        diagnostic "SE-185" "invalid variant for option"
            ("The variant \x1b[31m" <> T.pack variantName <> "\x1b[0m is not a valid variant for an option.\n" <> "Only the variants \x1b[31mNone\x1b[0m and \x1b[31mSome\x1b[0m are valid.")
    describe (EInvalidVariantForResult variantName) =
        diagnostic "SE-186" "invalid variant for result"
            ("The variant \x1b[31m" <> T.pack variantName <> "\x1b[0m is not a valid variant for a result.\n" <> "Only the variants \x1b[31mOk\x1b[0m and \x1b[31mError\x1b[0m are valid.")
    describe (EInvalidVariantForStatus variantName) =
        diagnostic "SE-187" "invalid variant for status"
            ("The variant \x1b[31m" <> T.pack variantName <> "\x1b[0m is not a valid variant for a status.\n" <> "Only the variants \x1b[31mSuccess\x1b[0m, \x1b[31mFailure\x1b[0m are valid.")
    describe (EInvalidResultTypeSpecifier typeSpec) =
        diagnostic "SE-188" "invalid type specifier for result"
            ("The type specifier \x1b[31m" <> showText typeSpec <> "\x1b[0m is not a valid type specifier for a result.\n" <> "Result types must be of the form \x1b[31mResult<R; L>\x1b[0m, where \x1b[31mR\x1b[0m is the valid result type and \x1b[31mL\x1b[0m is the error type.")
    describe (EMonadicVariantParameterTypeMismatch expectedTy actualTy) =
        diagnostic "SE-189" "monadic variant parameter type mismatch"
            ("The parameter of the variant is expected to be of type \x1b[31m" <> showText expectedTy <> "\x1b[0m but you are providing it of type \x1b[31m" <> showText actualTy <> "\x1b[0m.")
    describe (EObjectPreviouslyMoved prevPos) =
        relatedTo prevPos "the object was previously moved here" $
        diagnostic "SE-190" "object previously moved"
            ("You are trying to access an object that has been moved.\n")
    describe (EIsStatusVariantInvalidType ty) =
        diagnostic "SE-191" "invalid type for is-status-variant expression"
            ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid type for is-status-variant expression.")
    describe (EIsResultVariantInvalidType ty) =
        diagnostic "SE-192" "invalid type for is-result-variant expression"
            ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid type for is-result-variant expression.")
    describe (EInvalidSystemExceptEmitterType ty) =
        diagnostic "SE-193" "invalid system exception emitter type"
            ("System exception emitters emit data of type \x1b[31m" <> showText (TEnum "Exception" :: TerminaType a) <> "\x1b[0m but you are expecting data of type \x1b[31m" <> showText ty <> "\x1b[0m.")
    describe (EInvalidInterruptActionReturnType ident ty) =
        diagnostic "SE-194" "invalid interrupt action return type"
            ("The return type of actions attached to the interrupt event is expected to be \x1b[31m" <> showText (TStatus TInt32 :: TerminaType a) <> "\x1b[0m but the return type of action \x1b[31m" <> T.pack ident <> "\x1b[0m is \x1b[31m" <> showText ty <> "\x1b[0m.")
    describe (EInvalidPeriodicTimerActionReturnType ident ty) =
        diagnostic "SE-195" "invalid periodic timer action return type"
            ("The return type of actions attached to the periodic timer event is expected to be \x1b[31m" <> showText (TStatus TInt32 :: TerminaType a) <> "\x1b[0m but the return type of action \x1b[31m" <> T.pack ident <> "\x1b[0m is \x1b[31m" <> showText ty <> "\x1b[0m.")
    describe (EInvalidSystemInitActionReturnType ident ty) =
        diagnostic "SE-196" "invalid system init action return type"
            ("The return type of actions attached to the system init event is expected to be \x1b[31m" <> showText (TStatus TInt32 :: TerminaType a) <> "\x1b[0m but the return type of action \x1b[31m" <> T.pack ident <> "\x1b[0m is \x1b[31m" <> showText ty <> "\x1b[0m.")
    describe (EInvalidSystemExceptActionReturnType ident ty) =
        diagnostic "SE-197" "invalid system exception action return type"
            ("Actions that handle system exceptions shall not return a value.\n" <> "However, the return type of action \x1b[31m" <> T.pack ident <> "\x1b[0m is \x1b[31m" <> showText ty <> "\x1b[0m.")
    describe (EInvalidMsgQueueActionReturnType ident ty) =
        diagnostic "SE-198" "invalid message queue action return type"
            ("The return type of the actions attached to the reception of messages from a message queue is expected to be \x1b[31m" <> showText (TStatus TInt32 :: TerminaType a) <> "\x1b[0m but the return type of action \x1b[31m" <> T.pack ident <> "\x1b[0m is \x1b[31m" <> showText ty <> "\x1b[0m.")
    describe (ETypeNotInScope ident qualifiedName) =
        let importString = T.replace "\\" "." $ T.pack qualifiedName
            importString' = T.replace "/" "." importString
        in
            diagnostic "SE-199" "type not in scope"
                ("The type \x1b[31m" <> T.pack ident <> "\x1b[0m is not in scope.\n" <> "The type is defined in the module \x1b[31m" <> importString' <> "\x1b[0m. You need to import it.")
    describe (EFunctionNotInScope ident qualifiedName) =
        let importString = T.replace "\\" "." $ T.pack qualifiedName
            importString' = T.replace "/" "." importString
        in
            diagnostic "SE-200" "function not in scope"
                ("The function \x1b[31m" <> T.pack ident <> "\x1b[0m is not in scope.\n" <> "The function is defined in the module \x1b[31m" <> importString' <> "\x1b[0m. You need to import it.")
    describe (EUnknownAction ident) =
        diagnostic "SE-201" "unknown action"
            ("The action \x1b[31m" <> T.pack ident <> "\x1b[0m is not defined.")
    describe (EInvalidViewerParameterType ty) =
        diagnostic "SE-204" "invalid viewer parameter type"
            ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid type for a viewer parameter.")
    describe EInvalidAccessToProcedureFromImmutableSelfReference =
        diagnostic "SE-205" "invalid access to procedure from immutable self reference"
            ("You are trying to access a non-immutable procedure from an immutable self reference. " <> "Immutable self references can only access immutable procedures.")
    describe EInvalidAccessToOutPortFromImmutableSelfReference =
        diagnostic "SE-206" "invalid access to out port from immutable self reference"
            ("You are trying to access an outbound port from an immutable self reference. " <> "Immutable self references cannot access outbound ports.")
    describe (EProcedureSelfAccessKindMismatch (ifaceId, procId, expectedAccessKind, prevPos) accessKind) =
        relatedTo prevPos "the interface procedure is defined here" $
        diagnostic "SE-207" "self reference access kind mismatch in procedure"
            ("Procedure \x1b[31m" <> T.pack procId <> "\x1b[0m of interface \x1b[31m" <> T.pack ifaceId <> "\x1b[0m is expected to have a self reference of access kind \x1b[31m" <> showText expectedAccessKind <> "\x1b[0m but the access kind of the self reference of the implementated procedure is \x1b[31m" <> showText accessKind <> "\x1b[0m.\n")
    describe (ETaskClassMethod (classId, clsPos) ident) =
        relatedTo clsPos "the class is defined here" $
        diagnostic "SE-208" "task class defines a method"
            ("Task class \x1b[31m" <> T.pack classId <> "\x1b[0m defines the method \x1b[31m" <> T.pack ident <> "\x1b[0m.\n" <> "Task classes cannot define methods.")
    describe (EHandlerClassMethod (classId, clsPos) ident) =
        relatedTo clsPos "the class is defined here" $
        diagnostic "SE-209" "handler class defines a method"
            ("Handler class \x1b[31m" <> T.pack classId <> "\x1b[0m defines the method \x1b[31m" <> T.pack ident <> "\x1b[0m.\n" <> "Handler classes cannot define methods.")
    describe (EResourceClassViewer (classId, clsPos) ident) =
        relatedTo clsPos "the class is defined here" $
        diagnostic "SE-210" "resource class defines a viewer"
            ("Resource class \x1b[31m" <> T.pack classId <> "\x1b[0m defines the viewer \x1b[31m" <> T.pack ident <> "\x1b[0m.\n" <> "Resource classes cannot define viewers.")
    describe (EUnprotectedResourceWithRegularFields (clsId, prevPos)) =
        relatedTo prevPos "the resource class is defined here" $
        diagnostic "SE-211" "unprotected resource with regular fields"
            ("Resource class \x1b[31m" <> T.pack clsId <> "\x1b[0m defines regular fields but the resource is defined as unprotected.\n" <> "Unprotected resources cannot define regular fields.\n")
    describe (EMemberFunctionWithMutableSelfInTaskClass ident) =
        diagnostic "SE-212" "mutable member function in task class"
            ("Member function \x1b[31m" <> T.pack ident <> "\x1b[0m defines a mutable self reference. " <> "Member functions in task classes cannot define mutable self references\n" <> "Only immutable or private self references are allowed.")
    describe (EMemberFunctionWithMutableSelfInHandlerClass ident) =
        diagnostic "SE-213" "mutable member function in handler class"
            ("Member function \x1b[31m" <> T.pack ident <> "\x1b[0m defines a mutable self reference. " <> "Member functions in handler classes cannot define mutable self references\n" <> "Only immutable or private self references are allowed.")
    describe (ECharLiteralOutOfRange cp) =
        diagnostic "SE-217" "character literal out of range"
            ("The character literal has code point \x1b[31m" <> T.pack (show (fromEnum cp)) <> "\x1b[0m, which is outside the 7-bit ASCII range (0 to 127).")
    describe (EReservedIdentifier ident reservedBy) =
        diagnostic "SE-219" "reserved identifier"
            ("The name \x1b[31m" <> T.pack ident <> "\x1b[0m is reserved: " <> heldBy <> ".\n" <>
             "The transpiler does not rename, so a Termina name reaches the generated code as it is\n" <>
             "written and lands in the same name space as the names of C.")
      where
        heldBy = case reservedBy of
            CKeyword -> "it is a keyword of C"
            CStandardLibrary -> "it names something in the standard library of C"
            CImplementation -> "an underscore and an uppercase letter are kept for the implementation (ISO/IEC 9899 7.1.3)"
            CPlatform -> "the target platform declares it in the headers that a generated module includes"
    describe (EReferenceToPackedMember ident) =
        diagnostic "SE-218" "reference to a packed struct member"
            ("This reference reaches into the packed struct \x1b[31m" <> T.pack ident <> "\x1b[0m.\n" <> "Taking a reference to a member of a packed struct yields an under-aligned pointer, whose\n" <> "packed provenance is lost at the call boundary; on a strict-alignment target the callee then\n" <> "performs a misaligned access (undefined behavior). Read or write the member by value instead.")

    -- | The two clauses below pick their detail with a case, which the script
    -- that moved the rest of this table does not read, so they were moved by
    -- hand.
    describe (EInPortActionParamTypeMismatch (ident, prevPos) expectedTy actualTy) =
        relatedTo prevPos "the action is defined here" $
        diagnostic "SE-202" "in port action parameter type mismatch"
            (case expectedTy of
                TUnit ->
                    "The action \x1b[31m" <> T.pack ident <> "\x1b[0m is expected to have no parameters but it defines a parameter of type \x1b[31m" <> showText actualTy <> "\x1b[0m.\n"
                _ ->
                    "The action \x1b[31m" <> T.pack ident <> "\x1b[0m is expected to have a parameter of type \x1b[31m" <> showText expectedTy <>
                        "\x1b[0m but the actual type is \x1b[31m" <> showText actualTy <> "\x1b[0m.\n")
    describe (ESinkPortActionParamTypeMismatch (ident, prevPos) expectedTy actualTy) =
        relatedTo prevPos "the action is defined here" $
        diagnostic "SE-203" "sink port action parameter type mismatch"
            (case expectedTy of
                TUnit ->
                    "The action \x1b[31m" <> T.pack ident <> "\x1b[0m is expected to have no parameters but it defines a parameter of type \x1b[31m" <> showText actualTy <> "\x1b[0m.\n"
                _ ->
                    "The action \x1b[31m" <> T.pack ident <> "\x1b[0m is expected to have a parameter of type \x1b[31m" <> showText expectedTy <>
                        "\x1b[0m but the actual type is \x1b[31m" <> showText actualTy <> "\x1b[0m.\n")

    -- | Everything else is an error of the compiler, not of the program.
    describe _ = diagnosticWithoutDetail "Internal" "internal error"

instance ErrorMessage SemanticErrors where

    errorIdent = diagCode . describe . getError
    errorTitle = diagTitle . describe . getError

    -- | A calling loop names one position per member function it goes through,
    -- each with a header of its own, which is a message that walks several
    -- files and not an error with related positions. It therefore keeps the
    -- printer it had.
    toText e@(AnnotatedError (EClassLoop ((currentCall, _) : xs)) _pos) files =
        errorToText e files <> printCallTrace currentCall xs

        where

            -- | Prints a trace of member function calls
            printCallTrace :: Identifier -> [(Identifier, Location)] -> T.Text
            printCallTrace _ [] = ""
            printCallTrace currentCall' [(finalCall, tracePos@(Position _ traceStartPos _))] =
                let title = "\nFinally, member function \x1b[31m" <> T.pack currentCall' <>
                        "\x1b[0m calls \x1b[31m" <> T.pack finalCall <> "\x1b[0m again here:"
                    traceFileName = sourceName traceStartPos
                    traceSourceLines = files M.! traceFileName
                in
                    pprintSimpleError
                        traceSourceLines title traceFileName tracePos Nothing
            printCallTrace currentCall' ((nextCall, tracePos@(Position _ traceStartPos _)) : xr) =
                let title = "\nMember function \x1b[31m" <> T.pack currentCall' <>
                        "\x1b[0m calls \x1b[31m" <> T.pack nextCall <> "\x1b[0m here:"
                    traceFileName = sourceName traceStartPos
                    traceSourceLines = files M.! traceFileName
                in
                    pprintSimpleError
                        traceSourceLines title traceFileName tracePos Nothing <> printCallTrace nextCall xr
            printCallTrace _ _ = error "Internal error: invalid error position"

    toText e files = errorToText e files

    toDiagnostics = errorToDiagnostics
