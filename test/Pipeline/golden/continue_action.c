
#include "test.h"

void T__second_step(const termina__event_t * const termina__ev,
                    void * const termina__this) {
    
    (void)termina__ev;

    T * self = (T *)termina__this;

    self->count = 0U;

    return;

}

void T__first_step(const termina__event_t * const termina__ev,
                   void * const termina__this,
                   const TimeVal termina__ignored__t) {
    
    (void)termina__ignored__t;

    T * self = (T *)termina__this;

    self->count = self->count + 1U;

    if (self->count > 3U) {
        
        T__second_step(termina__ev, self);
        return;

    } else {
        
        return;

    }

}

void termina__task_entry__T(void * const arg) {
    
    T * self = (T *)arg;

    termina__error_code_t status = termina__error__none;

    termina__event_t event;

    TimeVal first_step__msg_data;

    for (;;) {
        
        termina__msg_queue__recv(self->_task_msg_queue_id, &event, &status);

        if (status != termina__error__none) {
            termina__except__msg_queue_recv_error(self->_task_msg_queue_id,
                                                  status);
        }

        switch (event.port_id) {
            
            case T__timer_port:

                termina__msg_queue__recv(self->timer_port,
                                         (void *)&first_step__msg_data,
                                         &status);

                if (status != termina__error__none) {
                    termina__except__msg_queue_recv_error(self->timer_port,
                                                          status);
                }

                T__first_step(&event, self, first_step__msg_data);

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

T t;
