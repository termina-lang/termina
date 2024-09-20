{-# LANGUAGE OverloadedStrings #-}

module Generator.Platform where

import qualified Data.Text as T
import Core.AST
import Semantic.Types
import DataFlow.Architecture.Types
import qualified Data.Map as M
import DataFlow.Architecture (emptyTerminaProgArch)
import Utils.Annotations

data SupportedPlatform = 
    RTEMS5NoelSpike

instance Show SupportedPlatform where
    show RTEMS5NoelSpike = "rtems5-noel-spike"

checkPlatform :: T.Text -> Maybe SupportedPlatform
checkPlatform "rtems5-noel-spike" = Just RTEMS5NoelSpike
checkPlatform _ = Nothing

getPlatformInitialGlobalEnv :: SupportedPlatform -> [(Identifier, Located (GEntry SemanticAnn))]
getPlatformInitialGlobalEnv RTEMS5NoelSpike = 
    [
       ("irq_1", Located (GGlob (SEmitter (DefinedType "Interrupt"))) Internal),
       ("irq_2", Located (GGlob (SEmitter (DefinedType "Interrupt"))) Internal),
       ("irq_3", Located (GGlob (SEmitter (DefinedType "Interrupt"))) Internal),
       ("irq_4", Located (GGlob (SEmitter (DefinedType "Interrupt"))) Internal)
    ]

getPlatformInitialProgram :: SupportedPlatform -> TerminaProgArch SemanticAnn
getPlatformInitialProgram RTEMS5NoelSpike = 
    emptyTerminaProgArch {
        emitters = M.union (emitters emptyTerminaProgArch) $ M.fromList [
            ("irq_1", TPInterruptEmittter "irq_1" (Located (GTy (GGlob (SEmitter (DefinedType "Interrupt")))) Internal)),
            ("irq_2", TPInterruptEmittter "irq_2" (Located (GTy (GGlob (SEmitter (DefinedType "Interrupt")))) Internal)),
            ("irq_3", TPInterruptEmittter "irq_3" (Located (GTy (GGlob (SEmitter (DefinedType "Interrupt")))) Internal)),
            ("irq_4", TPInterruptEmittter "irq_4" (Located (GTy (GGlob (SEmitter (DefinedType "Interrupt")))) Internal))
        ]
    }