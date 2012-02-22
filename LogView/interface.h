#include "Util/fltk_interface.h"
#include "logview_ui.h"

extern "C" {

LogViewWindow *create_logview(int max_bytes, int x, int y, int w, int h,
    const char *label, MsgCallback cb);
void append_log(LogViewWindow *view, const char *msg, const char *style);
void clear_logs(LogViewWindow *view);

void set_status(LogViewWindow *view, const char *status, const char *style);
void set_filter(LogViewWindow *view, const char *filter);

};
