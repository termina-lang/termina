
#include <termina.h>

#include "test.h"

static void termina__app__init_handlers(void) {
    
    boot_hdlr._handler_id = boot_hdlr__handler_id;

}

static void termina__app__initial_event(void) {
    
    termina__system_init_connection_t connection;

    connection.handler_object = (void *)&boot_hdlr;
    connection.handler_id = boot_hdlr__handler_id;
    connection.handler_action = &BootHandler__boot;

    termina__system_init__dispatch(system_init__emitter_id, &connection);

    return;

}

void termina__app__init(int32_t * const status) {
    
    *status = 0L;

    termina__app__init_globals();

    termina__app__initial_event();

    termina__app__init_handlers();

}
