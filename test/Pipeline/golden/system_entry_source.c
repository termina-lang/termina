
#include "test.h"

void UserTask__tick(const termina__event_t * const termina__ev,
                    void * const termina__this,
                    const TimeVal termina__ignored__t) {
    
    (void)termina__ignored__t;

    UserTask * self = (UserTask *)termina__this;

    TimeVal now = { .tv_sec = 0U, .tv_usec = 0U };

    self->sys.clock_get_uptime(termina__ev, self->sys._that, &now);

    self->sys.println_u32(termina__ev, self->sys._that, now.tv_sec,
                          (SysPrintBase){ ._variant = SysPrintBase__Decimal });

    return;

}

void termina__task_entry__UserTask(void * const arg) {
    
    UserTask * self = (UserTask *)arg;

    termina__error_code_t status = termina__error__none;

    termina__event_t event;

    TimeVal tick__msg_data;

    for (;;) {
        
        termina__msg_queue__recv(self->_task_msg_queue_id, &event, &status);

        if (status != termina__error__none) {
            termina__except__msg_queue_recv_error(self->_task_msg_queue_id,
                                                  status);
        }

        switch (event.port_id) {
            
            case UserTask__timer_port:

                termina__msg_queue__recv(self->timer_port,
                                         (void *)&tick__msg_data, &status);

                if (status != termina__error__none) {
                    termina__except__msg_queue_recv_error(self->timer_port,
                                                          status);
                }

                UserTask__tick(&event, self, tick__msg_data);

                break;

            default:

                termina__except__runtime_failure(0U,
                                                 termina__runtime_operation__task_dispatch,
                                                 termina__error__invalid_id);

                break;

        }

    }

    return;

}

PeriodicTimer timer;

UserTask t;
