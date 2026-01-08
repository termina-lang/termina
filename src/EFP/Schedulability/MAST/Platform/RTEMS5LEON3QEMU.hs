module EFP.Schedulability.MAST.Platform.RTEMS5LEON3QEMU where

import EFP.Schedulability.MAST.AST
import EFP.Schedulability.MAST.Monad
import Control.Monad.Except
import Utils.Annotations
import EFP.Schedulability.MAST.Errors


getRTEMS5LEON3QEMUIrqPriority :: Identifier -> MASTGenMonad Priority 
getRTEMS5LEON3QEMUIrqPriority emitterId = case emitterId of
    "irq_1" -> return $ 256 + 16 - 1 
    "irq_2" -> return $ 256 + 16 - 2
    "irq_3" -> return $ 256 + 16 - 3
    "irq_4" -> return $ 256 + 16 - 4
    "irq_5" -> return $ 256 + 16 - 5
    "irq_6" -> return $ 256 + 16 - 6
    "irq_7" -> return $ 256 + 16 - 7
    "irq_8" -> return $ 256 + 16 - 8
    "irq_9" -> return $ 256 + 16 - 9
    "irq_10" -> return $ 256 + 16 - 10
    "irq_11" -> return $ 256 + 16 - 11
    "irq_12" -> return $ 256 + 16 - 12
    "irq_13" -> return $ 256 + 16 - 13
    "irq_14" -> return $ 256 + 16 - 14
    "irq_15" -> return $ 256 + 16 - 15
    _ -> throwError . annotateError Internal $ EUnknownEmitter emitterId

getRTEMS5LEON3QEMUTimerIrqPriority :: MASTGenMonad Priority 
getRTEMS5LEON3QEMUTimerIrqPriority = return $ 256 + 16 - 8
