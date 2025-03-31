{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Generator.CodeGen.Types where

import Generator.LanguageC.AST
import Generator.LanguageC.Embedded
import Generator.CodeGen.Common

-- | Generic types
_TimeVal, _Result, _Status :: CType
_TimeVal = typeDef "TimeVal"
_Result = typeDef "Result"
_Status = typeDef "Status"

__termina_id_t, __termina_pool_t, 
    __termina_allocator_t,
    __termina_msg_queue_t, __termina_periodic_timer_t :: CType
__termina_id_t = typeDef terminaID
__termina_pool_t = typeDef pool
__termina_allocator_t = typeDef allocator
__termina_msg_queue_t = typeDef msgQueue
__termina_periodic_timer_t = typeDef periodicTimer

__termina_box_t, __option_box_t :: CType
__termina_box_t = typeDef boxStruct
__option_box_t = typeDef optionBox

__termina_emitter_task_connection_t :: CType
__termina_emitter_task_connection_t = typeDef "__termina_emitter_task_connection_t"

__termina_periodic_timer_connection_t,
    __termina_periodic_timer_handler_connection_t,
    __termina_periodic_timer_action_t :: CType
__termina_periodic_timer_connection_t = typeDef "__termina_periodic_timer_connection_t"
__termina_periodic_timer_handler_connection_t = typeDef "__termina_periodic_timer_handler_connection_t"
__termina_periodic_timer_action_t = typeDef "__termina_periodic_timer_action_t"

__termina_interrupt_connection_t,
    __termina_interrupt_handler_connection_t,
    __termina_interrupt_action_t :: CType
__termina_interrupt_connection_t = typeDef "__termina_interrupt_connection_t"
__termina_interrupt_handler_connection_t = typeDef "__termina_interrupt_handler_connection_t"
__termina_interrupt_action_t = typeDef "__termina_interrupt_action_t"


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
--                                  uint32_t _irq_vector);
irq_handler :: Ident -> Ident -> CExpression
irq_handler classId handler = (classId <::> handler) @:
    CTFunction (typeDef "Result")
        [
            -- | CRISCVUARTHandler * const self
            _const . ptr $ classId,
            -- | uint32_t _irq_vector
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

__termina_sys_time__clock_get_uptime :: CExpression
__termina_sys_time__clock_get_uptime = "__termina_sys_time__clock_get_uptime" @:
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

__termina_periodic_timer__init :: CExpression
__termina_periodic_timer__init = "__termina_periodic_timer__init" @:
    CTFunction void
        [
            -- | const __termina_id_t timer_id
            _const __termina_id_t,
            -- | const __termina_periodic_timer_connection_t * const connection,
            _const . ptr $ _const __termina_periodic_timer_connection_t,
            -- | const TimeVal * const period,
            _const . ptr $ _const _TimeVal,
            -- | Status * const status
            _const . ptr $ _Status
        ]

__termina_interrupt__init :: CExpression
__termina_interrupt__init = "__termina_interrupt__init" @:
    CTFunction void
        [
            -- | const __termina_id_t interrupt_id
            _const __termina_id_t,
            -- | const __termina_interrupt_connection_t * const connection,
            _const . ptr $ _const __termina_interrupt_connection_t,
            -- | Status * const status
            _const . ptr $ _Status
        ]

__termina_mutex_policy_t, __termina_task_prio_t :: CType
__termina_mutex_policy_t = typeDef "__termina_mutex_policy_t"
__termina_task_prio_t = typeDef "__termina_task_prio_t"

__termina_task_entry_t :: CType
__termina_task_entry_t = typeDef "__termina_task_entry_t"

__termina_task__init :: CExpression
__termina_task__init = "__termina_task__init" @:
    CTFunction void
        [
            -- | const __termina_id_t task_id
            _const __termina_id_t,
            -- | const __termina_task_prio_t entry,
            _const __termina_task_prio_t,
            -- | const size_t stack_size,
            _const size_t,
            -- | const __termina_task_entry_t entry,
            __termina_task_entry_t,
            -- | void * arg,
            ptr void,
            -- | Status * const status
            _const . ptr $ _Status
        ]

__termina_mutex__init :: CExpression
__termina_mutex__init = "__termina_mutex__init" @:
    CTFunction void
        [
            -- | const __termina_id_t mutex_id
            _const __termina_id_t,
            -- | const __termina_mutex_policy_t policy,
            _const __termina_mutex_policy_t,
            -- | const __termina_task_prio_t prio_ceiling,
            _const __termina_task_prio_t,
            -- | Status * const status
            _const . ptr $ _Status
        ]

__termina_msg_queue__init :: CExpression
__termina_msg_queue__init = "__termina_msg_queue__init" @:
    CTFunction void
        [
            -- | const __termina_id_t msg_queue_id
            _const __termina_id_t,
            -- | const size_t message_size,
            _const size_t,
            -- | const size_t message_queue_size,
            _const size_t,
            -- | Status * const status
            _const . ptr $ _Status
        ]

__termina_msg_queue__recv :: CExpression
__termina_msg_queue__recv = "__termina_msg_queue__recv" @:
    CTFunction void
        [
            -- | const __termina_id_t msg_queue_id
            _const __termina_id_t,
            -- | void * const element,
            ptr void,
            -- | Status * const status
            _const . ptr $ _Status
        ]

__termina_exec__shutdown :: CExpression
__termina_exec__shutdown = "__termina_exec__shutdown" @:
    CTFunction void []

__termina_pool__alloc :: CExpression
__termina_pool__alloc = "__termina_pool__alloc" @:
    CTFunction void
        [
            _const __termina_id_t,
            -- | __option_box_t * const opt
            _const . ptr $ __option_box_t
        ]

__termina_pool__free :: CExpression
__termina_pool__free = "__termina_pool__free" @:
    CTFunction void
        [
            _const __termina_id_t,
            -- | void * const element
            __termina_box_t
        ]

__termina_app__init_tasks :: CExpression
__termina_app__init_tasks = "__termina_app__init_tasks" @:
    CTFunction void
        [
            -- | Status * const status
            _const . ptr $ _Status
        ]

__termina_app__install_emitters :: CExpression
__termina_app__install_emitters = "__termina_app__install_emitters" @:
    CTFunction void
        [
            -- | Status * const status
            _const . ptr $ _Status
        ]

__termina_app__enable_protection :: CExpression
__termina_app__enable_protection = "__termina_app__enable_protection" @:
    CTFunction void []

__termina_app__init_msg_queues :: CExpression
__termina_app__init_msg_queues = "__termina_app__init_msg_queues" @:
    CTFunction void
        [
            -- | Status * const status
            _const . ptr $ _Status
        ]

__termina_app__initial_event :: CExpression
__termina_app__initial_event = "__termina_app__initial_event" @:
    CTFunction void
        [
            -- | Status * const status
            _const . ptr $ _Status
        ]

__termina_app__init_emitters :: CExpression
__termina_app__init_emitters = "__termina_app__init_emitters" @:
    CTFunction void
        [
            -- | Status * const status
            _const . ptr $ _Status
        ]

__termina_app__init_mutexes :: CExpression
__termina_app__init_mutexes = "__termina_app__init_mutexes" @:
    CTFunction void
        [
            -- | Status * const status
            _const . ptr $ _Status
        ]

__termina_mutex__lock :: CExpression
__termina_mutex__lock = "__termina_mutex__lock" @:
    CTFunction void
        [
            -- | const __termina_id_t mutex_id
            _const __termina_id_t,
            -- | Status * const status
            _const . ptr $ _Status
        ]

__termina_mutex__unlock :: CExpression
__termina_mutex__unlock = "__termina_mutex__unlock" @:
    CTFunction void
        [
            -- | const __termina_id_t mutex_id
            _const __termina_id_t,
            -- | Status * const status
            _const . ptr $ _Status
        ]

__termina_task_lock_t :: CType
__termina_task_lock_t = typeDef "__termina_task_lock_t"

__termina_task__lock :: CExpression
__termina_task__lock = "__termina_task__lock" @:
    CTFunction __termina_task_lock_t
        []

__termina_task__unlock :: CExpression
__termina_task__unlock = "__termina_task__unlock" @:
    CTFunction void
        [
            -- | const __termina_task_lock_t lock
            _const __termina_task_lock_t
        ]

__termina_event_lock_t :: CType
__termina_event_lock_t = typeDef "__termina_event_lock_t"

__termina_event__lock :: CExpression
__termina_event__lock = "__termina_event__lock" @:
    CTFunction __termina_event_lock_t
        []

__termina_event__unlock :: CExpression
__termina_event__unlock = "__termina_event__unlock" @:
    CTFunction void
        [
            -- | const __termina_event_lock_t lock
            _const __termina_event_lock_t
        ]

__termina_sys_time_t, __termina_sys_print_t :: CType
__termina_sys_time_t = typeDef "__termina_sys_time_t"
__termina_sys_print_t = typeDef "__termina_sys_print_t"

