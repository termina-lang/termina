
#include "test.h"

void CSender__on_pin(const termina__event_t * const termina__ev,
                     void * const termina__this, const Pin p) {
    
    CSender * self = (CSender *)termina__this;

    termina__out_port__send(termina__ev, self->pins, (const void *)&p);

    return;

}

void CSender__run(const termina__event_t * const termina__ev,
                  void * const termina__this, const Level termina__ignored__l) {
    
    (void)termina__ignored__l;

    CSender * self = (CSender *)termina__this;
    {
        Pin msg = { .pin = 2U, .port = 1U };
        termina__out_port__send(termina__ev, self->pins, (const void *)&msg);
    }
    {
        Level msg = { ._variant = Level__Value, .Value = { ._0 = 3U } };
        termina__out_port__send(termina__ev, self->levels, (const void *)&msg);
    }
    {
        Level msg = { ._variant = Level__Low };
        termina__out_port__send(termina__ev, self->levels, (const void *)&msg);
    }

    return;

}

void termina__task_entry__CSender(void * const arg) {
    
    CSender * self = (CSender *)arg;

    termina__error_code_t status = termina__error__none;

    termina__event_t event;

    Level run__msg_data;
    Pin on_pin__msg_data;

    for (;;) {
        
        termina__msg_queue__recv(self->_task_msg_queue_id, &event, &status);

        if (status != termina__error__none) {
            termina__except__msg_queue_recv_error(self->_task_msg_queue_id,
                                                  status);
        }

        switch (event.port_id) {
            
            case CSender__levels_in:

                termina__msg_queue__recv(self->levels_in,
                                         (void *)&run__msg_data, &status);

                if (status != termina__error__none) {
                    termina__except__msg_queue_recv_error(self->levels_in,
                                                          status);
                }

                CSender__run(&event, self, run__msg_data);

                break;

            case CSender__pins_in:

                termina__msg_queue__recv(self->pins_in,
                                         (void *)&on_pin__msg_data, &status);

                if (status != termina__error__none) {
                    termina__except__msg_queue_recv_error(self->pins_in,
                                                          status);
                }

                CSender__on_pin(&event, self, on_pin__msg_data);

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

void CSenderTests__sends(const termina__event_t * const termina__ev,
                         void * const termina__this) {
    
    CSenderTests * self = (CSenderTests *)termina__this;

    size_t n = 0U;

    {
        const Level value = { ._variant = Level__Value, .Value = { ._0 = 4U } };
        self->level.fire(termina__ev, self->level._that, &value);
    }

    {
        const Pin value = { .pin = 4U, .port = 3U };
        self->pin.fire(termina__ev, self->pin._that, &value);
    }

    self->pins.pending(termina__ev, self->pins._that, &n);

    self->levels.pending(termina__ev, self->levels._that, &n);

    return;

}

termina__test__emitter_t level;

termina__test__emitter_t pin;

termina__msg_queue_t pins;

termina__msg_queue_t levels;

CSender sender;

CSenderTests sender_tests;
