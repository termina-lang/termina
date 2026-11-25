
#include "test.h"

uint8_t from_option(const Option__u8 o) {
    
    uint8_t r;

    if (o._variant == Option__Some) {
        
        const uint8_t v = o.Some._0;

        r = v;

    } else {
        
        r = 0U;

    }

    return r;

}

uint8_t from_status(const Status__i32 s) {
    
    uint8_t r = 0U;

    if (s._variant == Status__Success) {
        
        r = 1U;

    }

    return r;

}

uint8_t from_result(const Result__u8__i32 s) {
    
    uint8_t r;

    if (s._variant == Result__Ok) {
        
        const uint8_t v = s.Ok._0;

        r = v;

    } else {
        
        r = 2U;

    }

    return r;

}

uint8_t sum(const uint8_t x) {
    
    return (uint8_t)(((uint32_t)(uint8_t)(((uint32_t)(uint8_t)(((uint32_t)from_option((Option__u8){ ._variant = Option__Some,
                                                                                                    .Some = { ._0 = x } }) + from_option((Option__u8){ ._variant = Option__None })) & 0xFFU) + from_status((Status__i32){ ._variant = Status__Success })) & 0xFFU) + from_status((Status__i32){ ._variant = Status__Failure,
                                                                                                                                                                                                                                                                                                .Failure = { ._0 = -(3L) } })) & 0xFFU);

}

void C__set(const termina__event_t * const termina__ev,
            void * const termina__this, const uint8_t x) {
    
    C * self = (C *)termina__this;

    termina__resource__lock(&termina__ev->owner, &self->_lock_type);

    self->v = (uint8_t)(((uint32_t)(uint8_t)(((uint32_t)(uint8_t)(((uint32_t)self->v + from_result((Result__u8__i32){ ._variant = Result__Ok,
                                                                                                                      .Ok = { ._0 = (uint8_t)(((uint32_t)x + 1U) & 0xFFU) } })) & 0xFFU) + from_result((Result__u8__i32){ ._variant = Result__Error,
                                                                                                                                                                                                                          .Error = { ._0 = 7L } })) & 0xFFU) + sum(x)) & 0xFFU);

    termina__resource__unlock(&termina__ev->owner, &self->_lock_type);

    return;

}
