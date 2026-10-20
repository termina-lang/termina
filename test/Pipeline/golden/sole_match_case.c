
#include "test.h"

uint32_t fun0(const Status__Error0 s) {
    
    uint32_t r;

    if (s._variant == Status__Success) {
        
        r = 1U;

    } else {
        
        const Error0 error = s.Failure._0;

        (void)error;

        r = 2U;

    }

    return r;

}
