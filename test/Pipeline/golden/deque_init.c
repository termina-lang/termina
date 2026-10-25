
#include <termina.h>
#include "config.h"

#include "test.h"

void termina__app__init_globals(void) {
    
    pool_res._lock_type.type = termina__resource_lock_type__none;
    termina__deque__init(&pending_deque, termina__deque_items__pending_deque,
                         2U);
    timer.period.tv_sec = 1U;
    timer.period.tv_usec = 0U;
    timer2.period.tv_sec = 2U;
    timer2.period.tv_usec = 0U;
    t.pending = &pending_deque;
    t.pool._that = &pool_res;
    t.pool.alloc = &termina__pool__alloc;
    t.pool.free = &termina__pool__free;

    return;

}
