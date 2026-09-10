
#include <termina.h>

#include "test.h"

void __termina_app__init_globals(void);

static void __termina_app__init_tasks(int32_t * const status) {
    
    *status = 0L;

    if (0L == *status) {
        
        t.__task_id = __t__task_id;

        t.__task_msg_queue_id = __t__task_msg_queue_id;

        __termina_task__init(__t__task_id, 10, 4096U, &__UserTask__termina_task,
                             &t, status);

    }

}

static void __termina_app__init_handlers(int32_t * const status) {
    
    *status = 0L;

}

static void __termina_app__init_emitters(int32_t * const status) {
    
    *status = 0L;

    if (0L == *status) {
        
        __termina_periodic_timer_connection_t connection;
        connection.type = __termina_emitter_connection_type__task;
        connection.task.task_msg_queue_id = __t__task_msg_queue_id;
        connection.task.sink_msgq_id = __t__timer_port__sink_msg_queue_id;
        connection.task.sink_port_id = __UserTask__timer_port;

        t.timer_port = __t__timer_port__sink_msg_queue_id;

        __termina_periodic_timer__init(__timer__timer_id, __timer__emitter_id,
                                       &connection, &timer.period, status);

    }

}

static void __termina_app__init_mutexes(int32_t * const status) {
    
    *status = 0L;

}

static void __termina_app__init_pools(int32_t * const status) {
    
    *status = 0L;

}

static void __termina_app__init_msg_queues(int32_t * const status) {
    
    *status = 0L;

    if (0L == *status) {
        
        __termina_msg_queue__init(__t__task_msg_queue_id,
                                  sizeof(__termina_event_t), 1U, status);

    }

    if (0L == *status) {
        
        __termina_msg_queue__init(__t__timer_port__sink_msg_queue_id,
                                  sizeof(TimeVal), 1U, status);

    }

}

static void __termina_app__enable_protection(void) {
    

}

static void __termina_app__init_channel_connections(int32_t * const status) {
    
    *status = 0L;

}

void __termina_app__init(int32_t * const status) {
    
    *status = 0L;

    __termina_app__init_globals();

    __termina_app__init_msg_queues(status);

    if (0L == *status) {
        
        __termina_app__init_channel_connections(status);

    }

    if (0L == *status) {
        
        __termina_app__init_pools(status);

    }

    if (0L == *status) {
        
        __termina_app__init_mutexes(status);

    }

    if (0L == *status) {
        
        __termina_app__enable_protection();

    }

    if (0L == *status) {
        
        __termina_app__init_emitters(status);

    }

    if (0L == *status) {
        
        __termina_app__init_handlers(status);

    }

    if (0L == *status) {
        
        __termina_app__init_tasks(status);

    }

}
