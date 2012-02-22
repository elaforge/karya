#include "interface.h"


extern "C" {

LogViewWindow *
create_logview(int max_bytes, int x, int y, int w, int h, const char *label,
    MsgCallback cb)
{
    LogViewWindow *view = new LogViewWindow(x, y, w, h, label, cb, max_bytes);
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
set_status(LogViewWindow *view, const char *status, const char *style)
{
    view->view.set_status(status, style);
}

void
set_filter(LogViewWindow *view, const char *filter)
{
    view->view.set_filter(filter);
}

};
