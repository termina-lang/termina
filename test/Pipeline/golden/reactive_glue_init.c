
#include <termina.h>

#include "test.h"

void __termina_app__init_globals() {
    
    timer.period.tv_sec = 1U;
    timer.period.tv_usec = 0U;
    foo_res.__lock_type.type = __termina_resource_lock_type__none;
    foo_res.value = 0U;
    t.foo.__that = &foo_res;
    t.foo.get = &FooRes__get;

    return;

}
