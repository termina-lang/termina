{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Generator.CodeGen.Types where

import Generator.LanguageC.AST
import Generator.LanguageC.Embedded
import Generator.CodeGen.Common

-- | Generic types
_TimeVal, __status_int32_t :: CType
_TimeVal = typeDef "TimeVal"
__status_int32_t = typeDef "__status_int32_t"

__termina_id_t, __termina_pool_t, 
    __termina_allocator_t,
    __termina_msg_queue_t, __termina_periodic_timer_t :: CType
__termina_id_t = typeDef terminaID
__termina_pool_t = typeDef pool
__termina_allocator_t = typeDef allocator
__termina_msg_queue_t = typeDef msgQueue
__termina_periodic_timer_t = typeDef periodicTimer

__termina_event_t, __termina_active_entity_t,
    __enum_termina_active_entity__handler_params_t,
    __enum_termina_active_entity__task_params_t :: CType
__termina_event_t = typeDef "__termina_event_t"
__termina_active_entity_t = typeDef "__termina_active_entity_t"
__enum_termina_active_entity__handler_params_t = typeDef "__enum_termina_active_entity__handler_params_t"
__enum_termina_active_entity__task_params_t = typeDef "__enum_termina_active_entity__task_params_t"

__termina_lock_t, __termina_resource_lock_type_t,
    __enum_termina_resource_lock_type__mutex_params_t :: CType
__termina_lock_t = typeDef "__termina_lock_t"
__termina_resource_lock_type_t = typeDef "__termina_resource_lock_type_t"
__enum_termina_resource_lock_type__mutex_params_t = typeDef "__enum_termina_resource_lock_type__mutex_params_t"

__termina_resource__lock :: CExpression
__termina_resource__lock = "__termina_resource__lock" @:
    CTFunction __termina_lock_t 
        [
            -- | const __termina_id_t resource_id
            _const . ptr $ _const __termina_active_entity_t,
            -- | const __termina_resource_lock_type_t lock_type,
            _const . ptr $ _const __termina_resource_lock_type_t
        ]

__termina_resource__unlock :: CExpression
__termina_resource__unlock = "__termina_resource__unlock" @:
    CTFunction __termina_lock_t
        [
            -- | const __termina_id_t resource_id
            _const . ptr $ _const __termina_active_entity_t,
            -- | const __termina_resource_lock_type_t lock_type,
            _const . ptr $ _const __termina_resource_lock_type_t,
            -- | __termina_lock_t lock
            __termina_lock_t
        ]

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
    CTFunction __status_int32_t
        [
            _const . ptr $ classId,
            -- | TimeVal current
            _TimeVal
        ]

__termina_pool__init :: CExpression
__termina_pool__init = "__termina_pool__init" @:
    CTFunction int32_t
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

_SystemEntry__clock_get_uptime :: CExpression
_SystemEntry__clock_get_uptime = "SystemEntry__clock_get_uptime" @:
    CTFunction void [
        _const . ptr $ _const __termina_event_t,
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
            -- | const __termina_id_t emitter_id,
            _const __termina_id_t,
            -- | const __termina_periodic_timer_connection_t * const connection,
            _const . ptr $ _const __termina_periodic_timer_connection_t,
            -- | const TimeVal * const period,
            _const . ptr $ _const _TimeVal,
            -- | int32_t * const status
            _const . ptr $ int32_t
        ]

__termina_interrupt__init :: CExpression
__termina_interrupt__init = "__termina_interrupt__init" @:
    CTFunction void
        [
            -- | const __termina_id_t interrupt_id
            _const __termina_id_t,
            -- | const __termina_interrupt_connection_t * const connection,
            _const . ptr $ _const __termina_interrupt_connection_t,
            -- | int32_t * const status
            _const . ptr $ int32_t
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
            -- | int32_t * const status
            _const . ptr $ int32_t
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
            -- | int32_t * const status
            _const . ptr $ int32_t
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
            -- | int32_t * const status
            _const . ptr $ int32_t
        ]

__termina_msg_queue__recv :: CExpression
__termina_msg_queue__recv = "__termina_msg_queue__recv" @:
    CTFunction void
        [
            -- | const __termina_id_t msg_queue_id
            _const __termina_id_t,
            -- | void * const element,
            ptr void,
            -- | int32_t * const status
            _const . ptr $ int32_t
        ]

__termina_except__msg_queue_recv_error :: CExpression
__termina_except__msg_queue_recv_error = "__termina_except__msg_queue_recv_error" @:
    CTFunction void
        [
            -- | const size_t msg_queue_id
            _const size_t,
            -- | const int32_t error_code
            _const int32_t
        ]

__termina_except__action_failure :: CExpression
__termina_except__action_failure = "__termina_except__action_failure" @:
    CTFunction void
        [
            -- | const Exception source
            _const (typeDef "Exception"),
            -- | const size_t sink_port_id,
            _const size_t,
            -- | const int32_t error_code
            _const int32_t
        ]

__termina_exec__reboot :: CExpression
__termina_exec__reboot = "__termina_exec__reboot" @:
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
            -- | int32_t * const status
            _const . ptr $ int32_t
        ]

__termina_app__init_handlers :: CExpression
__termina_app__init_handlers = "__termina_app__init_handlers" @:
    CTFunction void
        [
            -- | int32_t * const status
            _const . ptr $ int32_t
        ]

__termina_app__install_emitters :: CExpression
__termina_app__install_emitters = "__termina_app__install_emitters" @:
    CTFunction void
        [
            -- | int32_t * const status
            _const . ptr $ int32_t
        ]

__termina_app__enable_protection :: CExpression
__termina_app__enable_protection = "__termina_app__enable_protection" @:
    CTFunction void []

__termina_app__init_channel_connections :: CExpression
__termina_app__init_channel_connections = "__termina_app__init_channel_connections" @:
    CTFunction void
        [
            -- | int32_t * const status
            _const . ptr $ int32_t
        ]

__termina_app__init_msg_queues :: CExpression
__termina_app__init_msg_queues = "__termina_app__init_msg_queues" @:
    CTFunction void
        [
            -- | int32_t * const status
            _const . ptr $ int32_t
        ]

__termina_app__initial_event :: CExpression
__termina_app__initial_event = "__termina_app__initial_event" @:
    CTFunction void
        [
            -- | int32_t * const status
            _const . ptr $ int32_t
        ]

__termina_app__init_emitters :: CExpression
__termina_app__init_emitters = "__termina_app__init_emitters" @:
    CTFunction void
        [
            -- | int32_t * const status
            _const . ptr $ int32_t
        ]

__termina_app__init_pools :: CExpression
__termina_app__init_pools = "__termina_app__init_pools" @:
    CTFunction void
        [
            -- | int32_t * const status
            _const . ptr $ int32_t
        ]

__termina_app__init_mutexes :: CExpression
__termina_app__init_mutexes = "__termina_app__init_mutexes" @:
    CTFunction void
        [
            -- | int32_t * const status
            _const . ptr $ int32_t
        ]

__termina_mutex__lock :: CExpression
__termina_mutex__lock = "__termina_mutex__lock" @:
    CTFunction void
        [
            -- | const __termina_id_t mutex_id
            _const __termina_id_t,
            -- | int32_t * const status
            _const . ptr $ int32_t
        ]

__termina_mutex__unlock :: CExpression
__termina_mutex__unlock = "__termina_mutex__unlock" @:
    CTFunction void
        [
            -- | const __termina_id_t mutex_id
            _const __termina_id_t,
            -- | int32_t * const status
            _const . ptr $ int32_t
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

