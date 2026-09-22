{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Generator.CodeGen.Types where

import Generator.LanguageC.AST
import Generator.LanguageC.Embedded
import Generator.CodeGen.Common

-- | Generic types
_TimeVal, _Status__i32 :: CType
_TimeVal = typeDef "TimeVal"
_Status__i32 = typeDef "Status__i32"

termina__id_t, termina__pool_t, 
    termina__allocator_t,
    termina__msg_queue_t, termina__periodic_timer_t :: CType
termina__id_t = typeDef terminaID
termina__pool_t = typeDef pool
termina__allocator_t = typeDef allocator
termina__msg_queue_t = typeDef msgQueue
termina__periodic_timer_t = typeDef periodicTimer

termina__event_t, termina__active_entity_t,
    termina__enum__active_entity__handler_params_t,
    termina__enum__active_entity__task_params_t :: CType
termina__event_t = typeDef "termina__event_t"
termina__active_entity_t = typeDef "termina__active_entity_t"
termina__enum__active_entity__handler_params_t = typeDef "termina__enum__active_entity__handler_params_t"
termina__enum__active_entity__task_params_t = typeDef "termina__enum__active_entity__task_params_t"

termina__lock_t, termina__resource_lock_type_t,
    termina__enum__resource_lock_type__mutex_params_t :: CType
termina__lock_t = typeDef "termina__lock_t"
termina__resource_lock_type_t = typeDef "termina__resource_lock_type_t"
termina__enum__resource_lock_type__mutex_params_t = typeDef "termina__enum__resource_lock_type__mutex_params_t"

termina__resource__lock :: CExpression
termina__resource__lock = "termina__resource__lock" @:
    CTFunction termina__lock_t 
        [
            -- | const termina__id_t resource_id
            _const . ptr $ _const termina__active_entity_t,
            -- | const termina__resource_lock_type_t lock_type,
            _const . ptr $ _const termina__resource_lock_type_t
        ]

termina__resource__unlock :: CExpression
termina__resource__unlock = "termina__resource__unlock" @:
    CTFunction termina__lock_t
        [
            -- | const termina__id_t resource_id
            _const . ptr $ _const termina__active_entity_t,
            -- | const termina__resource_lock_type_t lock_type,
            _const . ptr $ _const termina__resource_lock_type_t,
            -- | termina__lock_t lock
            termina__lock_t
        ]

termina__box_t, _Option__box :: CType
termina__box_t = typeDef boxStruct
_Option__box = typeDef optionBox

termina__emitter_task_connection_t :: CType
termina__emitter_task_connection_t = typeDef "termina__emitter_task_connection_t"

termina__periodic_timer_connection_t,
    termina__periodic_timer_handler_connection_t,
    termina__periodic_timer_action_t :: CType
termina__periodic_timer_connection_t = typeDef "termina__periodic_timer_connection_t"
termina__periodic_timer_handler_connection_t = typeDef "termina__periodic_timer_handler_connection_t"
termina__periodic_timer_action_t = typeDef "termina__periodic_timer_action_t"

termina__interrupt_connection_t,
    termina__interrupt_handler_connection_t,
    termina__interrupt_action_t :: CType
termina__interrupt_connection_t = typeDef "termina__interrupt_connection_t"
termina__interrupt_handler_connection_t = typeDef "termina__interrupt_handler_connection_t"
termina__interrupt_action_t = typeDef "termina__interrupt_action_t"

termina__system_init_connection_t,
    termina__system_init_action_t :: CType
termina__system_init_connection_t = typeDef "termina__system_init_connection_t"
termina__system_init_action_t = typeDef "termina__system_init_action_t"

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
    CTFunction _Status__i32
        [
            _const . ptr $ classId,
            -- | TimeVal current
            _TimeVal
        ]

termina__pool__init :: CExpression
termina__pool__init = "termina__pool__init" @:
    CTFunction int32_t
        [
            -- | termina__pool_t * const pool
            _const . ptr $ termina__pool_t,
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
        _const . ptr $ _const termina__event_t,
        -- | TimeVal * const current
        _const . ptr $ _TimeVal
    ]

termina__app__init_globals :: CExpression
termina__app__init_globals = "termina__app__init_globals" @:
    CTFunction void []

termina__pool__size :: CExpression
termina__pool__size = "termina__pool__size" @:
    CTFunction size_t [
        -- | size of type
        size_t,
        -- | number of elements
        size_t
    ]

termina__periodic_timer__init :: CExpression
termina__periodic_timer__init = "termina__periodic_timer__init" @:
    CTFunction void
        [
            -- | const termina__id_t timer_id
            _const termina__id_t,
            -- | const termina__id_t emitter_id,
            _const termina__id_t,
            -- | const termina__periodic_timer_connection_t * const connection,
            _const . ptr $ _const termina__periodic_timer_connection_t,
            -- | const TimeVal * const period,
            _const . ptr $ _const _TimeVal,
            -- | int32_t * const status
            _const . ptr $ int32_t
        ]

termina__system_init__dispatch :: CExpression
termina__system_init__dispatch = "termina__system_init__dispatch" @:
    CTFunction void
        [
            -- | const termina__id_t emitter_id
            _const termina__id_t,
            -- | const termina__system_init_connection_t * const connection
            _const . ptr $ _const termina__system_init_connection_t
        ]

termina__interrupt__init :: CExpression
termina__interrupt__init = "termina__interrupt__init" @:
    CTFunction void
        [
            -- | const termina__id_t interrupt_id
            _const termina__id_t,
            -- | const termina__interrupt_connection_t * const connection,
            _const . ptr $ _const termina__interrupt_connection_t,
            -- | int32_t * const status
            _const . ptr $ int32_t
        ]

_MutexProtocol, termina__task_prio_t :: CType
_MutexProtocol = typeDef "MutexProtocol"
termina__task_prio_t = typeDef "termina__task_prio_t"

termina__task_entry_t :: CType
termina__task_entry_t = typeDef "termina__task_entry_t"

termina__task__init :: CExpression
termina__task__init = "termina__task__init" @:
    CTFunction void
        [
            -- | const termina__id_t task_id
            _const termina__id_t,
            -- | const termina__task_prio_t entry,
            _const termina__task_prio_t,
            -- | const size_t stack_size,
            _const size_t,
            -- | const termina__task_entry_t entry,
            termina__task_entry_t,
            -- | void * arg,
            ptr void,
            -- | int32_t * const status
            _const . ptr $ int32_t
        ]

termina__mutex__init :: CExpression
termina__mutex__init = "termina__mutex__init" @:
    CTFunction void
        [
            -- | const termina__id_t mutex_id
            _const termina__id_t,
            -- | const MutexProtocol protocol,
            _const _MutexProtocol,
            -- | int32_t * const status
            _const . ptr $ int32_t
        ]

termina__msg_queue__init :: CExpression
termina__msg_queue__init = "termina__msg_queue__init" @:
    CTFunction void
        [
            -- | const termina__id_t msg_queue_id
            _const termina__id_t,
            -- | const size_t message_size,
            _const size_t,
            -- | const size_t message_queue_size,
            _const size_t,
            -- | int32_t * const status
            _const . ptr $ int32_t
        ]

termina__msg_queue__recv :: CExpression
termina__msg_queue__recv = "termina__msg_queue__recv" @:
    CTFunction void
        [
            -- | const termina__id_t msg_queue_id
            _const termina__id_t,
            -- | void * const element,
            ptr void,
            -- | int32_t * const status
            _const . ptr $ int32_t
        ]

termina__except__msg_queue_recv_error :: CExpression
termina__except__msg_queue_recv_error = "termina__except__msg_queue_recv_error" @:
    CTFunction void
        [
            -- | const size_t msg_queue_id
            _const size_t,
            -- | const int32_t error_code
            _const int32_t
        ]

termina__except__action_failure :: CExpression
termina__except__action_failure = "termina__except__action_failure" @:
    CTFunction void
        [
            -- | const Exception source
            _const (typeDef "Exception"),
            -- | const size_t sink_port_id,
            _const size_t,
            -- | const int32_t error_code
            _const int32_t
        ]

termina__exec__reboot :: CExpression
termina__exec__reboot = "termina__exec__reboot" @:
    CTFunction void []

termina__pool__alloc :: CExpression
termina__pool__alloc = "termina__pool__alloc" @:
    CTFunction void
        [
            _const termina__id_t,
            -- | _Option__box * const opt
            _const . ptr $ _Option__box
        ]

termina__pool__free :: CExpression
termina__pool__free = "termina__pool__free" @:
    CTFunction void
        [
            _const termina__id_t,
            -- | void * const element
            termina__box_t
        ]

termina__app__init_tasks :: CExpression
termina__app__init_tasks = "termina__app__init_tasks" @:
    CTFunction void
        [
            -- | int32_t * const status
            _const . ptr $ int32_t
        ]

termina__app__init_handlers :: CExpression
termina__app__init_handlers = "termina__app__init_handlers" @:
    CTFunction void []

termina__app__install_emitters :: CExpression
termina__app__install_emitters = "termina__app__install_emitters" @:
    CTFunction void
        [
            -- | int32_t * const status
            _const . ptr $ int32_t
        ]

termina__app__enable_protection :: CExpression
termina__app__enable_protection = "termina__app__enable_protection" @:
    CTFunction void []

termina__app__init_channel_connections :: CExpression
termina__app__init_channel_connections = "termina__app__init_channel_connections" @:
    CTFunction void []

termina__app__init_msg_queues :: CExpression
termina__app__init_msg_queues = "termina__app__init_msg_queues" @:
    CTFunction void
        [
            -- | int32_t * const status
            _const . ptr $ int32_t
        ]

termina__app__initial_event :: CExpression
termina__app__initial_event = "termina__app__initial_event" @:
    CTFunction void []

termina__app__init_emitters :: CExpression
termina__app__init_emitters = "termina__app__init_emitters" @:
    CTFunction void
        [
            -- | int32_t * const status
            _const . ptr $ int32_t
        ]

termina__app__init_pools :: CExpression
termina__app__init_pools = "termina__app__init_pools" @:
    CTFunction void
        [
            -- | int32_t * const status
            _const . ptr $ int32_t
        ]

termina__app__init_mutexes :: CExpression
termina__app__init_mutexes = "termina__app__init_mutexes" @:
    CTFunction void
        [
            -- | int32_t * const status
            _const . ptr $ int32_t
        ]

termina__mutex__lock :: CExpression
termina__mutex__lock = "termina__mutex__lock" @:
    CTFunction void
        [
            -- | const termina__id_t mutex_id
            _const termina__id_t,
            -- | int32_t * const status
            _const . ptr $ int32_t
        ]

termina__mutex__unlock :: CExpression
termina__mutex__unlock = "termina__mutex__unlock" @:
    CTFunction void
        [
            -- | const termina__id_t mutex_id
            _const termina__id_t,
            -- | int32_t * const status
            _const . ptr $ int32_t
        ]

termina__task_lock_t :: CType
termina__task_lock_t = typeDef "termina__task_lock_t"

termina__task__lock :: CExpression
termina__task__lock = "termina__task__lock" @:
    CTFunction termina__task_lock_t
        []

termina__task__unlock :: CExpression
termina__task__unlock = "termina__task__unlock" @:
    CTFunction void
        [
            -- | const termina__task_lock_t lock
            _const termina__task_lock_t
        ]

termina__event_lock_t :: CType
termina__event_lock_t = typeDef "termina__event_lock_t"

termina__event__lock :: CExpression
termina__event__lock = "termina__event__lock" @:
    CTFunction termina__event_lock_t
        []

termina__event__unlock :: CExpression
termina__event__unlock = "termina__event__unlock" @:
    CTFunction void
        [
            -- | const termina__event_lock_t lock
            _const termina__event_lock_t
        ]

termina__sys_time_t, termina__sys_print_t :: CType
termina__sys_time_t = typeDef "termina__sys_time_t"
termina__sys_print_t = typeDef "termina__sys_print_t"

