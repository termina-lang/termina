module EFP.Schedulability.RT.Parsing
  (terminaRTParser) where

-- Importing parser combinators
import Text.Parsec hiding (Error, Ok)

import EFP.Schedulability.RT.Parser.AST

import Utils.Annotations
import EFP.Schedulability.Core.Parsing
import EFP.Schedulability.Core.Types

transStepActionParser :: SchedParser (RTTransStep ParserAnn)
transStepActionParser = do
    current <- getState
    startPos <- getPosition
    lbl <- identifierParser
    _ <- reservedOp "#"
    componentIdentifier <- identifierParser
    _ <- reservedOp "."
    actionIdentifier <- identifierParser
    _ <- reservedOp "::"
    pathIdentifier <- identifierParser
    nextStep <-  reservedOp "->" >> nextStepParser
    RTTransStepAction lbl componentIdentifier actionIdentifier pathIdentifier nextStep . Position current startPos <$> getPosition

transEndStepParser :: SchedParser (RTTransStep ParserAnn)
transEndStepParser = do
    current <- getState
    startPos <- getPosition
    lbl <- identifierParser
    _ <- reservedOp "#"
    _ <- reserved "end"
    RTTransStepEnd lbl . Position current startPos <$> getPosition

rtTransStepMulticastParser :: SchedParser (RTTransStep ParserAnn)
rtTransStepMulticastParser = do
    current <- getState
    startPos <- getPosition
    _ <- reservedOp "{|"
    steps <- many1 nextStepParser
    _ <- reservedOp "|}"
    RTTransStepMuticast steps . Position current startPos <$> getPosition

rtTransStepConditionalParser :: SchedParser (RTTransStep ParserAnn)
rtTransStepConditionalParser = do
    current <- getState
    startPos <- getPosition
    branches <- angles (sepBy conditionalBranchParser comma)
    RTTransStepConditional branches . Position current startPos <$> getPosition

  where

    conditionalBranchParser :: SchedParser (ConstExpression ParserAnn, RTTransStep ParserAnn)
    conditionalBranchParser = do
        times <- constExpressionParser
        _ <- reservedOp "!"
        step <- nextStepParser
        return (times, step)

nextStepParser :: SchedParser (RTTransStep ParserAnn)
nextStepParser =
    try transStepActionParser
    <|> try transEndStepParser
    <|> try rtTransStepMulticastParser
    <|> rtTransStepConditionalParser

rtTransactionParser :: SchedParser (RTElement ParserAnn)
rtTransactionParser = do
    current <- getState
    startPos <- getPosition
    _ <- reserved "transaction"
    transName <- identifierParser
    _ <- reservedOp "="
    firstStep <- nextStepParser
    _ <- semi
    RTTransaction transName firstStep . Position current startPos <$> getPosition

structInitializerParser :: SchedParser (ConstStructInitializer ParserAnn)
structInitializerParser = do
    current <- getState
    startPos <- getPosition
    fieldAssignments <- braces (sepBy fieldAssignmentParser comma)
    ConstStructInitializer fieldAssignments . Position current startPos <$> getPosition

  where

    fieldAssignmentParser :: SchedParser (ConstFieldAssignment ParserAnn)
    fieldAssignmentParser = do
        current <- getState
        startPos <- getPosition
        fieldName <- identifierParser
        _ <- reservedOp "="
        fva <- try (ConstStructFieldValue <$> structInitializerParser)
               <|> (ConstStructSimpleValue <$> constExpressionParser)
        ConstFieldAssignment fieldName fva . Position current startPos <$> getPosition

rtEventBurstyParser :: SchedParser (RTEvent ParserAnn)
rtEventBurstyParser = do
    current <- getState
    startPos <- getPosition
    _ <- reserved "bursty"
    eventName <- identifierParser
    _ <- reservedOp "="
    initializerExpr <- structInitializerParser
    RTEventBursty eventName initializerExpr . Position current startPos <$> getPosition

rtEventPeriodicParser :: SchedParser (RTEvent ParserAnn)
rtEventPeriodicParser = do
    current <- getState
    startPos <- getPosition
    _ <- reserved "periodic"
    eventName <- identifierParser
    _ <- reservedOp "="
    initializerExpr <- structInitializerParser
    RTEventPeriodic eventName initializerExpr . Position current startPos <$> getPosition

rtEventParser :: SchedParser (RTEvent ParserAnn)
rtEventParser = try rtEventBurstyParser <|> rtEventPeriodicParser

rtSituationParser :: SchedParser (RTElement ParserAnn)
rtSituationParser = do
    current <- getState
    startPos <- getPosition
    _ <- reserved "rts"
    rtsName <- identifierParser
    _ <- reservedOp "="
    events <- braces (sepBy rtEventParser comma)
    _ <- semi
    RTSituation rtsName events . Position current startPos <$> getPosition

-- | Top Level parser
topLevel :: SchedParser [RTElement ParserAnn]
topLevel = many $
  try rtTransactionParser
  <|> rtSituationParser

terminaRTParser :: SchedParser [RTElement ParserAnn]
terminaRTParser = contents topLevel