
#include <termina.h>

#include "test.h"

static void termina__app__init_tasks(int32_t * const status) {
    
    *status = 0L;

    t._task_id = t__task_id;

    t._task_msg_queue_id = t__task_msg_queue_id;

    termina__task__init(t__task_id, 10, 4096U, &termina__task_entry__UserTask,
                        &t, status);

}

static void termina__app__init_emitters(int32_t * const status) {
    
    termina__periodic_timer_connection_t timer_connection;

    *status = 0L;

    timer_connection.type = termina__emitter_connection_type__task;
    timer_connection.task.task_msg_queue_id = t__task_msg_queue_id;
    timer_connection.task.sink_msgq_id = t__timer_port__sink_msg_queue_id;
    timer_connection.task.sink_port_id = UserTask__timer_port;

    t.timer_port = t__timer_port__sink_msg_queue_id;

    termina__periodic_timer__init(timer__timer_id, timer__emitter_id,
                                  &timer_connection, &timer.period, status);

}

static void termina__app__init_msg_queues(int32_t * const status) {
    
    *status = 0L;

    termina__msg_queue__init(t__task_msg_queue_id, sizeof(termina__event_t), 1U,
                             status);

    if (0L == *status) {
        
        termina__msg_queue__init(t__timer_port__sink_msg_queue_id,
                                 sizeof(TimeVal), 1U, status);

    }

}

void termina__app__init(int32_t * const status) {
    
    *status = 0L;

    termina__app__init_globals();

    termina__app__init_msg_queues(status);

    if (0L == *status) {
        
        termina__app__init_emitters(status);

    }

    if (0L == *status) {
        
        termina__app__init_tasks(status);

    }

}
