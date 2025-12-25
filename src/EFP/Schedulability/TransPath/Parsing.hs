module EFP.Schedulability.TransPath.Parsing 
  (terminaTransPathsParser) where

-- Importing parser combinators
import Text.Parsec hiding (Error, Ok)

import EFP.Schedulability.TransPath.AST
import Utils.Annotations
import EFP.Schedulability.Core.Parsing
import EFP.Schedulability.Core.Types


blockPositionParser :: SchedParser BlockPosition
blockPositionParser = do
    _ <- reservedOp "@("
    startLine <- decimal
    _ <- reservedOp ":"
    startCol <- decimal
    _ <- reservedOp ","
    endLine <- decimal
    _ <- reservedOp ":"
    endCol <- decimal
    _ <- reservedOp ")"
    return $ BlockPosition startLine startCol endLine endCol

condIfParser :: SchedParser (WCEPathBlock ParserAnn)
condIfParser = do
    current <- getState
    startPos <- getPosition
    _ <- reserved "if"
    loc <- blockPositionParser
    blocks <- brackets (sepBy wcepPathBlockParser comma)
    WCEPathCondIf blocks loc . Position current startPos <$> getPosition

condElseIfParser :: SchedParser (WCEPathBlock ParserAnn)
condElseIfParser = do
    current <- getState
    startPos <- getPosition
    _ <- reserved "elif"
    loc <- blockPositionParser
    blocks <- brackets (sepBy wcepPathBlockParser comma)
    WCEPathCondElseIf blocks loc . Position current startPos <$> getPosition

condElseParser :: SchedParser (WCEPathBlock ParserAnn)
condElseParser = do
    current <- getState
    startPos <- getPosition
    _ <- reserved "else"
    loc <- blockPositionParser
    blocks <- brackets (sepBy wcepPathBlockParser comma)
    WCEPathCondElse blocks loc . Position current startPos <$> getPosition

forLoopParser :: SchedParser (WCEPathBlock ParserAnn)
forLoopParser = do
    current <- getState
    startPos <- getPosition
    _ <- reserved "for"
    (initExpr, finalExpr) <- parens $ do
        initE <- constExpressionParser
        _ <- reservedOp ".."
        finalE <- constExpressionParser
        return (initE, finalE)
    loc <- blockPositionParser
    blocks <- brackets (sepBy wcepPathBlockParser comma)
    WCEPathForLoop initExpr finalExpr blocks loc . Position current startPos <$> getPosition

matchCaseParser :: SchedParser (WCEPathBlock ParserAnn)
matchCaseParser = do
    current <- getState
    startPos <- getPosition
    _ <- reserved "case"
    loc <- blockPositionParser
    blocks <- brackets (sepBy wcepPathBlockParser comma)
    WCEPathMatchCase blocks loc . Position current startPos <$> getPosition

sendMessageParser :: SchedParser (WCEPathBlock ParserAnn)
sendMessageParser = do
    current <- getState
    startPos <- getPosition
    _ <- reserved "send"
    portName <- parens identifierParser
    loc <- blockPositionParser
    WCEPSendMessage portName loc . Position current startPos <$> getPosition

memberFunctionCallParser :: SchedParser (WCEPathBlock ParserAnn)
memberFunctionCallParser = do
    current <- getState
    startPos <- getPosition
    _ <- reserved "call"
    funcName <- parens identifierParser
    argExprs <- option [] (parens (sepBy constExpressionParser comma))
    loc <- blockPositionParser
    WCEPathMemberFunctionCall funcName argExprs loc . Position current startPos <$> getPosition

procedureInvokeParser :: SchedParser (WCEPathBlock ParserAnn)
procedureInvokeParser = do
    current <- getState
    startPos <- getPosition
    _ <- reserved "invoke"
    (portName, procName) <- parens $ do
        pName <- identifierParser
        _ <- reservedOp "."
        prName <- identifierParser
        return (pName, prName)
    argExprs <- option [] (parens (sepBy constExpressionParser comma))
    loc <- blockPositionParser
    WCEPProcedureInvoke portName procName argExprs loc . Position current startPos <$> getPosition

allocBoxParser :: SchedParser (WCEPathBlock ParserAnn)
allocBoxParser = do
    current <- getState
    startPos <- getPosition
    _ <- reserved "alloc"
    portName <- parens identifierParser
    loc <- blockPositionParser
    WCEPAllocBox portName loc . Position current startPos <$> getPosition

freeBoxParser :: SchedParser (WCEPathBlock ParserAnn)
freeBoxParser = do
    current <- getState
    startPos <- getPosition
    _ <- reserved "free"
    portName <- parens identifierParser
    loc <- blockPositionParser
    WCEPFreeBox portName loc . Position current startPos <$> getPosition

regularBlockParser :: SchedParser (WCEPathBlock ParserAnn)
regularBlockParser = do
    current <- getState
    startPos <- getPosition
    _ <- reserved "block"
    loc <- blockPositionParser
    WCEPRegularBlock loc . Position current startPos <$> getPosition

returnParser :: SchedParser (WCEPathBlock ParserAnn)
returnParser = do
    current <- getState
    startPos <- getPosition
    _ <- reserved "return"
    loc <- blockPositionParser
    WCEPReturn loc . Position current startPos <$> getPosition

continueParser :: SchedParser (WCEPathBlock ParserAnn)
continueParser = do
    current <- getState
    startPos <- getPosition
    _ <- reserved "continue"
    actionName <- parens identifierParser
    loc <- blockPositionParser
    WCEPContinue actionName loc . Position current startPos <$> getPosition

rebootParser :: SchedParser (WCEPathBlock ParserAnn)
rebootParser = do
    current <- getState
    startPos <- getPosition
    _ <- reserved "reboot"
    loc <- blockPositionParser
    WCEPReboot loc . Position current startPos <$> getPosition

systemCallParser :: SchedParser (WCEPathBlock ParserAnn)
systemCallParser = do
    current <- getState
    startPos <- getPosition
    _ <- reserved "syscall"
    sysCallName <- parens identifierParser
    argExprs <- option [] (parens (sepBy constExpressionParser comma))
    loc <- blockPositionParser
    WCEPSystemCall sysCallName argExprs loc . Position current startPos <$> getPosition

wcepPathBlockParser :: SchedParser (WCEPathBlock ParserAnn)
wcepPathBlockParser =
    try regularBlockParser
    <|> try returnParser
    <|> try continueParser
    <|> try rebootParser
    <|> try systemCallParser
    <|> try condIfParser
    <|> try condElseIfParser
    <|> try condElseParser
    <|> try forLoopParser
    <|> try matchCaseParser
    <|> try sendMessageParser
    <|> try memberFunctionCallParser
    <|> try procedureInvokeParser
    <|> try allocBoxParser
    <|> freeBoxParser

transactionalWCEPParser :: SchedParser (TransactionalWCEPath ParserAnn)
transactionalWCEPParser = do
    current <- getState
    startPos <- getPosition
    _ <- reserved "twcep"
    clsName <- identifierParser
    _ <- reservedOp "::"
    elementName <- identifierParser
    _ <- reservedOp "::"
    memberFunction <- identifierParser
    constParams <- parens (sepBy identifierParser comma)
    _ <- reservedOp "="
    blocks <- brackets (sepBy wcepPathBlockParser comma)
    TransactionalWCEPath clsName elementName memberFunction constParams blocks . Position current startPos <$> getPosition

-- | Top Level parser
topLevel :: SchedParser [TransactionalWCEPath ParserAnn]
topLevel = many $
  try transactionalWCEPParser

terminaTransPathsParser :: SchedParser [TransactionalWCEPath ParserAnn]
terminaTransPathsParser = contents topLevel