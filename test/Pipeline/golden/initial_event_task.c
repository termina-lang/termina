
#include <termina.h>

#include "test.h"

static void termina__app__init_tasks(int32_t * const status) {
    
    *status = 0L;

    boot_task._task_id = boot_task__task_id;

    boot_task._task_msg_queue_id = boot_task__task_msg_queue_id;

    termina__task__init(boot_task__task_id, 10, 4096U,
                        &termina__task_entry__BootTask, &boot_task, status);

}

static void termina__app__init_msg_queues(int32_t * const status) {
    
    *status = 0L;

    termina__msg_queue__init(boot_task__task_msg_queue_id,
                             sizeof(termina__event_t), 1U, status);

    if (0L == *status) {
        
        termina__msg_queue__init(boot_task__boot_ev__sink_msg_queue_id,
                                 sizeof(TimeVal), 1U, status);

    }

}

static void termina__app__initial_event(void) {
    
    termina__event_t event;
    event.emitter_id = system_init__emitter_id;
    event.owner.type = termina__active_entity__task;
    event.owner.task.task_id = boot_task__task_id;
    event.port_id = BootTask__boot_ev;

    TimeVal current;
    SystemEntry__clock_get_uptime(&event, &current);

    BootTask * self = &boot_task;

    Status__i32 result;

    result = BootTask__boot(&event, self, current);

    if (result._variant != Status__Success) {
        
        ExceptSource source;
        source._variant = ExceptSource__Task;
        source.Task._0 = boot_task__task_id;

        termina__except__action_failure(source, BootTask__boot_ev,
                                        result.Failure._0);

    }

    return;

}

void termina__app__init(int32_t * const status) {
    
    *status = 0L;

    termina__app__init_globals();

    termina__app__init_msg_queues(status);

    if (0L == *status) {
        
        termina__app__initial_event();

        termina__app__init_tasks(status);

    }

}
