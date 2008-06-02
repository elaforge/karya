#include "logview_ui.h"

extern "C" {

void initialize();
void ui_wait();
void ui_awake();
int has_windows();

LogViewWindow *create_logview(int x, int y, int w, int h, MsgCallback cb);
void append_log(LogViewWindow *view, const char *msg, const char *style);
void clear_logs(LogViewWindow *view);

void set_status(LogViewWindow *view, const char *status);

};
