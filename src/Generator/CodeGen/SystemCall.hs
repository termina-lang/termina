module Generator.CodeGen.SystemCall where

import qualified Data.Map as M
import Core.AST
import Generator.LanguageC.AST
import Generator.LanguageC.Embedded
import Generator.CodeGen.Types

-- | Map of syscall functions
-- | The key is the name of the function as defined in the system interface.
syscallFunctionsMap :: M.Map Identifier CExpression
syscallFunctionsMap =  M.fromList [
        ("clock_get_uptime", 
            "__termina_sys_time" @: __termina_sys_time_t @. "clock_get_uptime" @: 
                CTFunction void [
                    -- | TimeVal * const current_time
                    _const . ptr $ _TimeVal
            ]),
        ("delay_in",
            "__termina_sys_time" @: __termina_sys_time_t @. "delay_in" @: 
                CTFunction void [
                    -- | const TimeVal * const delay
                    _const . ptr $ _const _TimeVal
            ]),
        ("print",
            "__termina_sys_print" @: __termina_sys_print_t @. "print" @:
                CTFunction void [
                    _const $ CTArray char ("sys_print_buffer_size" @: size_t)
                ]
        ),
        ("println",
            "__termina_sys_print" @: __termina_sys_print_t @. "println" @:
                CTFunction void [
                    _const $ CTArray char ("sys_print_buffer_size" @: size_t)
                ]
        ),
        ("print_char",
            "__termina_sys_print"  @: __termina_sys_print_t @. "print_char" @:
                CTFunction void [
                    _const char
                ]
        ),
        ("println_char",
            "__termina_sys_print"  @: __termina_sys_print_t @. "println_char" @:
                CTFunction void [
                    _const char
                ]
        ),
        ("print_u8",
            "__termina_sys_print"  @: __termina_sys_print_t @. "print_u8" @:
                CTFunction void [
                    _const uint8_t
                ]
        ),
        ("println_u8",
            "__termina_sys_print"  @: __termina_sys_print_t @. "println_u8" @:
                CTFunction void [
                    _const uint8_t
                ]
        ),
        ("print_u16",
            "__termina_sys_print"  @: __termina_sys_print_t @. "print_u16" @:
                CTFunction void [
                    _const uint16_t
                ]
        ),
        ("println_u16",
            "__termina_sys_print"  @: __termina_sys_print_t @. "println_u16" @:
                CTFunction void [
                    _const uint16_t
                ]
        ),
        ("print_u32",
            "__termina_sys_print"  @: __termina_sys_print_t @. "print_u32" @:
                CTFunction void [
                    _const uint32_t
                ]
        ),
        ("println_u32",
            "__termina_sys_print"  @: __termina_sys_print_t @. "println_u32" @:
                CTFunction void [
                    _const uint32_t
                ]
        ),
        ("print_u64",
            "__termina_sys_print"  @: __termina_sys_print_t @. "print_u64" @:
                CTFunction void [
                    _const uint64_t
                ]
        ),
        ("println_u64",
            "__termina_sys_print"  @: __termina_sys_print_t @. "println_u64" @:
                CTFunction void [
                    _const uint64_t
                ]
        ),
        ("print_i8",
            "__termina_sys_print"  @: __termina_sys_print_t @. "print_i8" @:
                CTFunction void [
                    _const int8_t
                ]
        ),
        ("println_i8",
            "__termina_sys_print"  @: __termina_sys_print_t @. "println_i8" @:
                CTFunction void [
                    _const int8_t
                ]
        ),
        ("print_i16",
            "__termina_sys_print"  @: __termina_sys_print_t @. "print_i16" @:
                CTFunction void [
                    _const int16_t
                ]
        ),
        ("println_i16",
            "__termina_sys_print"  @: __termina_sys_print_t @. "println_i16" @:
                CTFunction void [
                    _const int16_t
                ]
        ),
        ("print_i32",
            "__termina_sys_print"  @: __termina_sys_print_t @. "print_i32" @:
                CTFunction void [
                    _const int32_t
                ]
        ),
        ("println_i32",
            "__termina_sys_print"  @: __termina_sys_print_t @. "println_i32" @:
                CTFunction void [
                    _const int32_t
                ]
        ),
        ("print_i64",
            "__termina_sys_print"  @: __termina_sys_print_t @. "print_i64" @:
                CTFunction void [
                    _const int64_t
                ]
        ),
        ("println_i64",
            "__termina_sys_print"  @: __termina_sys_print_t @. "println_i64" @:
                CTFunction void [
                    _const int64_t
                ]
        ),
        ("print_usize",
            "__termina_sys_print"  @: __termina_sys_print_t @. "print_usize" @:
                CTFunction void [
                    _const size_t
                ]
        ),
        ("println_usize",
            "__termina_sys_print"  @: __termina_sys_print_t @. "println_usize" @:
                CTFunction void [
                    _const size_t
                ]
        )
    ]