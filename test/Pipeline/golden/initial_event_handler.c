
#include <termina.h>

#include "test.h"

static void termina__app__init_handlers(void) {
    
    boot_hdlr._handler_id = boot_hdlr__handler_id;

}

static void termina__app__initial_event(void) {
    
    termina__event_t event;
    event.emitter_id = system_init__emitter_id;
    event.owner.type = termina__active_entity__handler;
    event.owner.handler.handler_id = boot_hdlr__handler_id;
    event.port_id = 0;

    TimeVal current;
    SystemEntry__clock_get_uptime(&event, &current);

    BootHandler * self = &boot_hdlr;

    Status__i32 result;

    result = BootHandler__boot(&event, self, current);

    if (result._variant != Status__Success) {
        
        ExceptSource source;
        source._variant = ExceptSource__Handler;
        source.Handler._0 = boot_hdlr__handler_id;

        termina__except__action_failure(source, 0U, result.Failure._0);

    }

    return;

}

void termina__app__init(int32_t * const status) {
    
    *status = 0L;

    termina__app__init_globals();

    termina__app__initial_event();

    termina__app__init_handlers();

}
