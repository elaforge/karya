// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

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

void
bring_to_front(LogViewWindow *view)
{
    view->show();
}

};
