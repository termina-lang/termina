
#include <termina.h>
#include "config.h"

#include "test.h"

void termina__app__init_globals(void) {
    
    timer.period.tv_sec = 1U;
    timer.period.tv_usec = 0U;
    t.sys._that = &system_entry;
    t.sys.clock_get_uptime = &SystemEntry__clock_get_uptime;
    t.sys.println_u32 = &SystemEntry__println_u32;

    return;

}
