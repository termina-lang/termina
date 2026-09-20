
#include <termina.h>

#include "test.h"

void termina__app__init_globals(void) {
    
    timer.period.tv_sec = 1U;
    timer.period.tv_usec = 0U;
    foo_res._lock_type.type = termina__resource_lock_type__none;
    foo_res.value = 0U;
    t.foo._that = &foo_res;
    t.foo.get = &FooRes__get;

    return;

}
