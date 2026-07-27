
#include "test.h"

uint32_t CCounter__double(const __termina_event_t * const __ev,
                          const CCounter * const self) {
    
    __asm__ __volatile__("termina__CCounter__double__entry:\n");

    __asm__ __volatile__("termina__CCounter__double__exit__0:\n");

    return self->count * 2U;

}

void CCounter__increment(const __termina_event_t * const __ev,
                         void * const __this, __status_int32_t * const status) {
    
    __asm__ __volatile__("termina__CCounter__increment__entry:\n");

    CCounter * self = (CCounter *)__this;

    __termina_lock_t __lock = __termina_resource__lock(&__ev->owner,
                                                       &self->__lock_type);

    self->count = self->count + 1U;

    (*status).__variant = Success;

    __termina_resource__unlock(&__ev->owner, &self->__lock_type, __lock);

    __asm__ __volatile__("termina__CCounter__increment__exit__0:\n");

    return;

}

__status_int32_t CWorker__on_retry(const __termina_event_t * const __ev,
                                   void * const __this) {
    
    __asm__ __volatile__("termina__CWorker__on_retry__entry:\n");

    CWorker * self = (CWorker *)__this;

    __status_int32_t result = { .__variant = Success };

    __asm__ __volatile__("termina__CWorker__on_retry__exit__0:\n");

    return result;

}

__status_int32_t CWorker__on_tick(const __termina_event_t * const __ev,
                                  void * const __this, TimeVal _current) {
    
    __asm__ __volatile__("termina__CWorker__on_tick__entry:\n");

    CWorker * self = (CWorker *)__this;

    if (self->threshold == 0U) {
        
        __status_int32_t failed = { .__variant = Failure,
                                    .Failure = { .__0 = 1L } };

        __asm__ __volatile__("termina__CWorker__on_tick__exit__0:\n");

        return failed;

    } else {
        
        __asm__ __volatile__("termina__CWorker__on_tick__exit__1:\n");

        return CWorker__on_retry(__ev, self);

    }

}

void __CWorker__termina_task(void * arg) {
    
    CWorker * self = (CWorker *)arg;

    int32_t status = 0L;

    __termina_event_t event;

    __status_int32_t result;
    result.__variant = Success;

    TimeVal on_tick__msg_data;

    for (;;) {
        
        __termina_msg_queue__recv(self->__task_msg_queue_id, &event, &status);

        if (status != 0L) {
            break;
        }

        switch (event.port_id) {
            
            case __CWorker__tick:

                __termina_msg_queue__recv(self->tick,
                                          (void *)&on_tick__msg_data, &status);

                if (status != 0L) {
                    __termina_except__msg_queue_recv_error(self->tick, status);
                }

                result = CWorker__on_tick(&event, self, on_tick__msg_data);

                if (result.__variant != Success) {
                    
                    ExceptSource source;
                    source.__variant = ExceptSource__Handler;
                    source.Task.__0 = self->__task_id;

                    __termina_except__action_failure(source, __CWorker__tick,
                                                     result.Failure.__0);

                }

                break;

            default:

                __termina_exec__reboot();

                break;

        }

    }

    return;

}

uint32_t scale(uint32_t value) {
    
    __asm__ __volatile__("termina__scale__entry:\n");

    __asm__ __volatile__("termina__scale__exit__0:\n");

    return value * 2U;

}
