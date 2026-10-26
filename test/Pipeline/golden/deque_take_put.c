
#include "test.h"

void T__offer(const termina__event_t * const termina__ev,
              void * const termina__this, const TimeVal termina__ignored__t) {
    
    (void)termina__ignored__t;

    T * self = (T *)termina__this;

    Option__box b = { ._variant = Option__None };

    self->pool.alloc(termina__ev, self->pool._that, &b);

    if (b._variant == Option__Some) {
        
        const termina__box_t m = b.Some._0;

        Option__box rejected = { ._variant = Option__None };

        termina__deque__try_put(self->pending, m, &rejected);

        if (rejected._variant == Option__Some) {
            
            const termina__box_t r = rejected.Some._0;

            self->pool.free(termina__ev, self->pool._that, r);

        } else {
            

        }

    } else {
        

    }

    Option__box c = { ._variant = Option__None };

    self->pool.alloc(termina__ev, self->pool._that, &c);

    if (c._variant == Option__Some) {
        
        const termina__box_t m = c.Some._0;

        Option__box evicted = { ._variant = Option__None };

        termina__deque__replace(self->pending, m, &evicted);

        if (evicted._variant == Option__Some) {
            
            const termina__box_t e = evicted.Some._0;

            self->pool.free(termina__ev, self->pool._that, e);

        } else {
            

        }

    } else {
        

    }

    return;

}

void T__tick(const termina__event_t * const termina__ev,
             void * const termina__this, const TimeVal termina__ignored__t) {
    
    (void)termina__ignored__t;

    T * self = (T *)termina__this;

    Option__box taken = { ._variant = Option__None };

    termina__deque__take(self->pending, &taken);

    if (taken._variant == Option__Some) {
        
        const termina__box_t m = taken.Some._0;

        if ((*(Msg *)m.data).n > 3U) {
            
            self->pool.free(termina__ev, self->pool._that, m);

        } else {
            
            (*(Msg *)m.data).n = (*(Msg *)m.data).n + 1U;

            termina__deque__put(self->pending, m);

        }

    } else {
        
        Option__box fresh = { ._variant = Option__None };

        self->pool.alloc(termina__ev, self->pool._that, &fresh);

        if (fresh._variant == Option__Some) {
            
            const termina__box_t f = fresh.Some._0;

            (*(Msg *)f.data).n = 0U;

            termina__deque__put(self->pending, f);

        } else {
            

        }

    }

    return;

}

void termina__task_entry__T(void * const arg) {
    
    T * self = (T *)arg;

    termina__error_code_t status = termina__error__none;

    termina__event_t event;

    TimeVal tick__msg_data;
    TimeVal offer__msg_data;

    for (;;) {
        
        termina__msg_queue__recv(self->_task_msg_queue_id, &event, &status);

        if (status != termina__error__none) {
            termina__except__msg_queue_recv_error(self->_task_msg_queue_id,
                                                  status);
        }

        switch (event.port_id) {
            
            case T__timer_port:

                termina__msg_queue__recv(self->timer_port,
                                         (void *)&tick__msg_data, &status);

                if (status != termina__error__none) {
                    termina__except__msg_queue_recv_error(self->timer_port,
                                                          status);
                }

                T__tick(&event, self, tick__msg_data);

                break;

            case T__offer_port:

                termina__msg_queue__recv(self->offer_port,
                                         (void *)&offer__msg_data, &status);

                if (status != termina__error__none) {
                    termina__except__msg_queue_recv_error(self->offer_port,
                                                          status);
                }

                T__offer(&event, self, offer__msg_data);

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

termina__pool_t pool_res;

termina__box_t termina__deque_items__pending_deque[2U];
termina__deque_t pending_deque;

PeriodicTimer timer;

PeriodicTimer timer2;

T t;
