
#include "test.h"

size_t code(const Pin p, const Level l) {
    
    size_t r = p.port + p.pin;

    if (l._variant == Level__Low) {
        

    } else if (l._variant == Level__High) {
        
        r = r + 1U;

    } else {
        
        const uint8_t v = l.Value._0;

        r = r + (size_t)v;

    }

    return r;

}

size_t high_code(const Pin p) {
    
    return code(p, (Level){ ._variant = Level__High });

}

void C__set(const termina__event_t * const termina__ev,
            void * const termina__this, const Level l) {
    
    C * self = (C *)termina__this;

    termina__lock_t termina__lock = termina__resource__lock(&termina__ev->owner,
                                                            &self->_lock_type);

    self->v = (size_t)(self->v + code((Pin){ .pin = 2U, .port = 1U },
                                      (Level){ ._variant = Level__Value,
                                               .Value = { ._0 = 3U } })) + code((Pin){ .pin = 0U,
                                                                                       .port = 0U },
                                                                                l);

    termina__resource__unlock(&termina__ev->owner, &self->_lock_type,
                              termina__lock);

    return;

}
