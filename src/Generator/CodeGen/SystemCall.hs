module Generator.CodeGen.SystemCall where

import qualified Data.Map as M
import Core.AST
import Generator.CodeGen.Common

procedureToFunction :: M.Map Identifier (M.Map Identifier Identifier)
procedureToFunction = M.fromList [
        ("SysTime", M.fromList [
            ("clock_get_uptime", namefy "termina_systime" <::> "clock_get_uptime")
        ])
    ]