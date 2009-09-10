#include <FL/Fl.H>
#include <FL/x.H>
#include "interface.h"

extern "C" {

void initialize() { Fl::lock(); }
void ui_wait() { Fl::wait(100); }
void ui_awake() { Fl::awake((void*) 0); }
int has_windows() { return Fl_X::first != NULL; }

LogViewWindow *
create_logview(int x, int y, int w, int h, MsgCallback cb, int max_bytes)
{
    LogViewWindow *view = new LogViewWindow(x, y, w, h, cb, max_bytes);
    view->show();
    return view;
}

void
append_log(LogViewWindow *view, const char *msg, const char *style)
{
    view->view.append_log(msg, style);
}

void
clear_logs(LogViewWindow *view)
{
    view->view.clear_logs();
}

void
set_status(LogViewWindow *view, const char *status)
{
    view->view.set_status(status);
}

void
set_filter(LogViewWindow *view, const char *filter)
{
    view->view.set_filter(filter);
}

};
