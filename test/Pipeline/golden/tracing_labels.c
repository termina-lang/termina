
#include "test.h"

static uint32_t CCounter__double(const termina__event_t * const termina__ev,
                                 const CCounter * const self);

static uint32_t CCounter__double(const termina__event_t * const termina__ev,
                                 const CCounter * const self) {
    
    (void)termina__ev;

    __asm__ __volatile__("termina__CCounter__double__entry:\n");

    __asm__ __volatile__("termina__CCounter__double__exit__0:\n");

    return self->count * 2U;

}

void CCounter__increment(const termina__event_t * const termina__ev,
                         void * const termina__this,
                         Status__i32 * const status) {
    
    __asm__ __volatile__("termina__CCounter__increment__entry:\n");

    CCounter * self = (CCounter *)termina__this;

    termina__lock_t termina__lock = termina__resource__lock(&termina__ev->owner,
                                                            &self->_lock_type);

    if (CCounter__double(termina__ev, self) < 100U) {
        
        self->count = self->count + 1U;

    }

    (*status)._variant = Status__Success;

    termina__resource__unlock(&termina__ev->owner, &self->_lock_type,
                              termina__lock);

    __asm__ __volatile__("termina__CCounter__increment__exit__0:\n");

    return;

}

Status__i32 CWorker__on_retry(const termina__event_t * const termina__ev,
                              void * const termina__this) {
    
    (void)termina__ev;

    __asm__ __volatile__("termina__CWorker__on_retry__entry:\n");

    CWorker * self = (CWorker *)termina__this;

    self->threshold = 1U;

    Status__i32 result = { ._variant = Status__Success };

    __asm__ __volatile__("termina__CWorker__on_retry__exit__0:\n");

    return result;

}

Status__i32 CWorker__on_tick(const termina__event_t * const termina__ev,
                             void * const termina__this,
                             const TimeVal termina__ignored__current) {
    
    (void)termina__ignored__current;

    __asm__ __volatile__("termina__CWorker__on_tick__entry:\n");

    CWorker * self = (CWorker *)termina__this;

    if (self->threshold == 0U) {
        
        Status__i32 failed = { ._variant = Status__Failure,
                               .Failure = { ._0 = 1L } };

        __asm__ __volatile__("termina__CWorker__on_tick__exit__0:\n");

        return failed;

    } else {
        
        __asm__ __volatile__("termina__CWorker__on_tick__exit__1:\n");

        return CWorker__on_retry(termina__ev, self);

    }

}

void termina__task_entry__CWorker(void * const arg) {
    
    CWorker * self = (CWorker *)arg;

    termina__error_code_t status = termina__error__none;

    termina__event_t event;

    Status__i32 result;

    TimeVal on_tick__msg_data;

    for (;;) {
        
        termina__msg_queue__recv(self->_task_msg_queue_id, &event, &status);

        if (status != termina__error__none) {
            break;
        }

        switch (event.port_id) {
            
            case CWorker__tick:

                termina__msg_queue__recv(self->tick, (void *)&on_tick__msg_data,
                                         &status);

                if (status != termina__error__none) {
                    termina__except__msg_queue_recv_error(self->tick, status);
                }

                result = CWorker__on_tick(&event, self, on_tick__msg_data);

                if (result._variant != Status__Success) {
                    
                    ExceptSource source;
                    source._variant = ExceptSource__Task;
                    source.Task._0 = self->_task_id;

                    termina__except__action_failure(source, CWorker__tick,
                                                    result.Failure._0);

                }

                break;

            default:

                termina__exec__reboot();

                break;

        }

    }

    return;

}

uint32_t scale(const uint32_t value) {
    
    __asm__ __volatile__("termina__scale__entry:\n");

    __asm__ __volatile__("termina__scale__exit__0:\n");

    return value * 2U;

}
