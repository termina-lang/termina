{-# LANGUAGE OverloadedStrings #-}

module Generator.Platform where

import qualified Data.Text as T
import AST.Core
import Semantic.Monad
import Semantic.Types
import DataFlow.Program.Types
import qualified Data.Map as M
import DataFlow.Program (emptyTerminaProgram)
import Parser.Parsing

data SupportedPlatform = 
    RTEMS5NoelSpike

instance Show SupportedPlatform where
    show RTEMS5NoelSpike = "rtems5-noel-spike"

checkPlatform :: T.Text -> Maybe SupportedPlatform
checkPlatform "rtems5-noel-spike" = Just RTEMS5NoelSpike
checkPlatform _ = Nothing

getPlatformInitialGlobalEnv :: SupportedPlatform -> [(Identifier, SAnns (GEntry SemanticAnns))]
getPlatformInitialGlobalEnv RTEMS5NoelSpike = 
    [
       ("irq_1", internalErrorSeman `SemAnn` GGlob (SEmitter (DefinedType "Interrupt"))),
       ("irq_2", internalErrorSeman `SemAnn` GGlob (SEmitter (DefinedType "Interrupt"))),
       ("irq_3", internalErrorSeman `SemAnn` GGlob (SEmitter (DefinedType "Interrupt"))),
       ("irq_4", internalErrorSeman `SemAnn` GGlob (SEmitter (DefinedType "Interrupt")))
    ]

getPlatformInitialProgram :: SupportedPlatform -> TerminaProgram SemanticAnns
getPlatformInitialProgram RTEMS5NoelSpike = 
    emptyTerminaProgram {
        emitters = M.union (emitters emptyTerminaProgram) $ M.fromList [
            ("irq_1", TPInterruptEmittter "irq_1" (SemAnn Internal (GTy (GGlob (SEmitter (DefinedType "Interrupt")))))),
            ("irq_2", TPInterruptEmittter "irq_2" (SemAnn Internal (GTy (GGlob (SEmitter (DefinedType "Interrupt")))))),
            ("irq_3", TPInterruptEmittter "irq_3" (SemAnn Internal (GTy (GGlob (SEmitter (DefinedType "Interrupt")))))),
            ("irq_4", TPInterruptEmittter "irq_4" (SemAnn Internal (GTy (GGlob (SEmitter (DefinedType "Interrupt"))))))
        ]
    }