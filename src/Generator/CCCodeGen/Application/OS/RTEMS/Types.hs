{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Generator.CCCodeGen.Application.OS.RTEMS.Types where
import Generator.LanguageC.CompCertC
import Generator.CCCodeGen.Utils
import Generator.CCCodeGen.Application.Types

-- | Generic RTEMS types

-- | rtems_status_code type
rtems_status_code :: CType
rtems_status_code = typeDef "rtems_status_code"

-- | rtems_task_argument type
rtems_task_argument :: CType
rtems_task_argument = typeDef "rtems_task_argument"

-- | rtems_task_priority type
rtems_task_priority :: CType
rtems_task_priority = typeDef "rtems_task_priority"

-- | rtems_id
rtems_id :: CType
rtems_id = typeDef "rtems_id"

-- | rtems_option 
rtems_option :: CType
rtems_option = typeDef "rtems_option"

rtems_interval :: CType
rtems_interval = typeDef "rtems_interval"

rtems_timer_service_routine_entry :: CType
rtems_timer_service_routine_entry = typeDef "rtems_timer_service_routine_entry"

__rtems_interrupt_emitter_t :: CType
__rtems_interrupt_emitter_t = typeDef "__rtems_interrupt_emitter_t"

__rtems_runtime_resource_lock_t :: CType
__rtems_runtime_resource_lock_t = typeDef "__rtems_runtime_resource_lock_t"

__rtems_runtime_resource_lock_mutex_params_t :: CType
__rtems_runtime_resource_lock_mutex_params_t = typeDef "__rtems_runtime_resource_lock_mutex_params_t"

__rtems_runtime_mutex_policy :: CType
__rtems_runtime_mutex_policy = typeDef "__rtems_runtime_mutex_policy"

rtems_shutdown_executive :: CExpression
rtems_shutdown_executive = "rtems_shutdown_executive" @: CTFunction void [uint32_t]

rtems_message_queue_receive :: CExpression
rtems_message_queue_receive = "rtems_message_queue_receive" @: 
    CTFunction rtems_status_code 
        [
            -- | rtems_id id
            rtems_id, 
            -- | void * buffer
            ptr void, 
            -- | size_t * size
            ptr size_t,
            -- | rtems_option option_set
            rtems_option,
            -- | rtems_interval timeout
            rtems_interval
        ]

rtems_message_queue_send :: CExpression
rtems_message_queue_send = "rtems_message_queue_send" @:
    CTFunction rtems_status_code 
        [
            -- | rtems_id id
            rtems_id, 
            -- | void * buffer
            ptr void, 
            -- | size_t size
            size_t,
            -- | rtems_option option_set
            rtems_option
        ]

__rtems__create_msg_queue :: CExpression
__rtems__create_msg_queue = "__rtems__create_msg_queue" @: 
    CTFunction rtems_status_code 
        [
            -- | uint32_t count
            uint32_t,
            -- | size_t max_messages
            size_t,
            -- | rtems_id * id
            ptr rtems_id
        ]

-- | rtems_status_code __rtems__create_timer(rtems_id * id);
__rtems__create_timer :: CExpression
__rtems__create_timer = "__rtems__create_timer" @: 
    CTFunction rtems_status_code 
        [
            -- | rtems_id * id
            ptr rtems_id
        ]

-- rtems_status_code __rtems__timer_delay_at(rtems_id id,
--                                          const TimeVal * next_time,
--                                          rtems_timer_service_routine_entry routine);
__rtems__timer_delay_at :: CExpression
__rtems__timer_delay_at = "__rtems__timer_delay_at" @: 
    CTFunction rtems_status_code 
        [
            -- | rtems_id id
            rtems_id,
            -- | const TimeVal * next_time
            _const . ptr $ _const  _TimeVal,
            -- | rtems_timer_service_routine_entry routine
            rtems_timer_service_routine_entry
        ]

-- | rtems_vector_number type
rtems_vector_number :: CType
rtems_vector_number = typeDef "rtems_vector_number"

-- | rtems_interrupt_handler type
rtems_interrupt_handler :: CType
rtems_interrupt_handler = typeDef "rtems_interrupt_handler"

-- | rtems_status_code __rtems__install_isr(rtems_vector_number vector,
--                                       rtems_interrupt_handler handler);
__rtems__install_isr :: CExpression
__rtems__install_isr = "__rtems__install_isr" @:
    CTFunction rtems_status_code 
        [
            -- | rtems_vector_number vector
            rtems_vector_number,
            -- | rtems_interrupt_handler handler
            rtems_interrupt_handler
        ]