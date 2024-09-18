{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Generator.CodeGen.Application.Types where
import Generator.LanguageC.AST
import Generator.CodeGen.Utils
import Generator.CodeGen.Common

-- | Generic types
_TimeVal, _Result,
    _Interrupt, _PeriodicTimer :: CType
_TimeVal = typeDef "TimeVal"
_Result = typeDef "Result"
_Interrupt = typeDef "Interrupt"
_PeriodicTimer = typeDef "PeriodicTimer"

__termina_resource_t, __termina_pool_t,
    __termina_msg_queue_t, __termina_task_t,
    __termina_timer_t :: CType
__termina_resource_t = typeDef resourceID
__termina_pool_t = typeDef pool
__termina_msg_queue_t = typeDef msgQueue
__termina_task_t = typeDef taskID
__termina_timer_t = typeDef "__termina_timer_t"

-- void __termina__add_timeval(TimeVal * const lhs, const TimeVal * const rhs);
__termina__add_timeval :: CExpression
__termina__add_timeval = "__termina__add_timeval" @: 
    CTFunction void
        [
            -- | TimeVal * const lhs
            _const . ptr $ _TimeVal,
            -- | const TimeVal * const rhs
            _const . ptr $ _const _TimeVal
        ]

-- Result classId__handle(classId * const self,
--                                  uint32_t _vector);
irq_handler :: Ident -> Ident -> CExpression
irq_handler classId handler = (classId <::> handler) @:
    CTFunction (typeDef "Result")
        [
            -- | CRISCVUARTHandler * const self
            _const . ptr $ classId,
            -- | uint32_t _vector
            uint32_t
        ]

timer_handler :: Ident -> Ident -> CExpression
timer_handler classId handler = (classId <::> handler) @:
    CTFunction void
        [
            -- | CRISCVUARTHandler * const self
            _const . ptr $ classId,
            -- | TimeVal current
            _TimeVal
        ]

system_init_handler :: Ident -> Ident -> CExpression
system_init_handler classId handler = (classId <::> handler) @:
    CTFunction (typeDef "Result")
        [
            -- | CRISCVUARTHandler * const self
            _const . ptr $ classId,
            -- | TimeVal current
            _TimeVal
        ]

-- Result __termina_resource__init(__termina_resource_t * const resource);
__termina_resource__init :: CExpression
__termina_resource__init = "__termina_resource__init" @:
    CTFunction (typeDef "Result")
        [
            -- | __termina_resource_t * const resource
            _const . ptr $ __termina_resource_t
            
        ]

-- | Result __termina_pool__init(__termina_pool_t * const pool, void * p_memory_area,
--                            size_t memory_area_size, size_t block_size);
__termina_pool__init :: CExpression
__termina_pool__init = "__termina_pool__init" @:
    CTFunction (typeDef "Result")
        [
            -- | __termina_pool_t * const pool
            _const . ptr $ __termina_pool_t,
            -- | void * p_memory_area
            CTPointer (CTVoid noqual) noqual,
            -- | size_t memory_area_size
            CTSizeT noqual,
            -- | size_t block_size
            CTSizeT noqual
        ]

__termina__clock_get_uptime :: CExpression
__termina__clock_get_uptime = "__termina__clock_get_uptime" @:
    CTFunction void [
        -- | TimeVal * const current
        _const . ptr $ _TimeVal
    ]

__termina_app__init_globals :: CExpression
__termina_app__init_globals = "__termina_app__init_globals" @:
    CTFunction void []

__termina_pool__size :: CExpression
__termina_pool__size = "__termina_pool__size" @:
    CTFunction size_t [
        -- | size of type
        size_t,
        -- | number of elements
        size_t
    ]