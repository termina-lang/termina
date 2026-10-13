
#include <termina.h>

#include "test.h"

static void termina__app__init_handlers(void) {
    
    fault_hdlr._handler_id = fault_hdlr__handler_id;

}

static void termina__app__init_emitters(termina__error_code_t * const status) {
    
    termina__system_except_connection_t except_connection;

    *status = termina__error__none;

    except_connection.handler_object = (void *)&fault_hdlr;
    except_connection.handler_id = fault_hdlr__handler_id;
    except_connection.handler_action = &FaultHandler__on_fault;

    termina__except__init_emitter(system_except__emitter_id,
                                  &except_connection);

}

void termina__app__init(termina__error_code_t * const status) {
    
    *status = termina__error__none;

    termina__app__init_globals();

    termina__app__init_emitters(status);

    if (termina__error__none == *status) {
        
        termina__app__init_handlers();

    }

}
