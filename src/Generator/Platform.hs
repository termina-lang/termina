{-# LANGUAGE OverloadedStrings #-}

module Generator.Platform where

import qualified Data.Text as T
import AST.Core
import Semantic.Monad
import Semantic.Types

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