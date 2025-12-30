module EFP.Schedulability.TransPath.Generator where

import EFP.Schedulability.RT.AST
import EFP.Schedulability.TransPath.AST
import EFP.Schedulability.TransPath.Monad
import EFP.Schedulability.WCEPath.AST
import Control.Monad
import EFP.Schedulability.TransPath.Errors
import Control.Monad.Except
import Utils.Annotations


genTPActivitiesFromWCEP :: WCEPath a -> TRPGenMonad [TransPathActivity a]
genTPActivitiesFromWCEP (WCEPath cmpName actionName pathName constParams blocks _ann) = 
    foldM genTPActivitiesFromBlock [] blocks

    where

    genTPActivitiesFromBlock :: [TransPathActivity a] -> WCEPathBlock a -> TRPGenMonad [TransPathActivity a]
    genTPActivitiesFromBlock acc (WCEPathForLoop initExpr finalExpr blocks _pos _ann) = undefined
    genTPActivitiesFromBlock acc (WCEPathMemberFunctionCall funcName constArgs _pos _ann) = undefined
    genTPActivitiesFromBlock acc (WCEPProcedureInvoke portName procName constArgs _pos _ann) = undefined
    genTPActivitiesFromBlock acc (WCEPathCondIf blocks _pos _ann) = undefined
    genTPActivitiesFromBlock acc (WCEPathCondElseIf blocks _pos _ann) = undefined
    genTPActivitiesFromBlock acc (WCEPathCondElse blocks _pos _ann) = undefined
    genTPActivitiesFromBlock acc (WCEPathMatchCase blocks _pos _ann) = undefined
    genTPActivitiesFromBlock acc (WCEPSendMessage portName _pos _ann) = undefined
    genTPActivitiesFromBlock acc (WCEPAllocBox poolName _pos _ann) = undefined
    genTPActivitiesFromBlock acc (WCEPFreeBox poolName _pos _ann) = undefined
    genTPActivitiesFromBlock acc (WCEPRegularBlock _pos _ann) = undefined
    genTPActivitiesFromBlock acc (WCEPSystemCall sysCallName constArgs _pos _ann) = undefined
    genTPActivitiesFromBlock acc (WCEPReturn _pos _ann) = undefined
    genTPActivitiesFromBlock acc (WCEPContinue actionName _pos _ann) = undefined
    genTPActivitiesFromBlock acc (WCEPReboot _pos _ann) = undefined


genTPActivitiesFromStep :: RTTransStep a -> TRPGenMonad [TransPathActivity a]
genTPActivitiesFromStep (RTTransStepAction name task action path Nothing a) = undefined
genTPActivitiesFromStep (RTTransStepAction name task action path (Just next) a) = undefined
genTPActivitiesFromStep (RTTransStepMuticast steps a) = undefined
genTPActivitiesFromStep (RTTransStepConditional _conds _ann) = throwError . annotateError Internal $ EInvalidTransStepType