module Generator.CodeGen.SystemCall where

import qualified Data.Map as M
import Core.AST
import Generator.CodeGen.Common
import Generator.LanguageC.AST
import Generator.LanguageC.Embedded
import Generator.CodeGen.Types

-- | Map of syscall functions
-- | The key is the name of the function as defined in the system interface.
syscallFunctionsMap :: M.Map Identifier CExpression
syscallFunctionsMap =  M.fromList [
        ("clock_get_uptime", 
            -- | void __termina_systime__clock_get_uptime(TimeVal * const current_time)
            namefy "termina_systime" <::> "clock_get_uptime" @: 
                CTFunction void [
                    -- | TimeVal * const current_time
                    _const . ptr $ _TimeVal
            ]),
        ("delay_in",
            -- | void __termina_systime__delay_in(const TimeVal * const delay)
            namefy "termina_systime" <::> "delay_in" @:
                CTFunction void [
                    -- | const TimeVal * const delay
                    _const . ptr $ _const _TimeVal
            ])
    ]