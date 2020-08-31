// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include "Util/fltk_interface.h"
#include "logview_ui.h"

extern "C" {

LogViewWindow *create_logview(int max_bytes, int x, int y, int w, int h,
    const char *label, MsgCallback cb);
void append_log(LogViewWindow *view, const char *msg, const char *style);
void clear_logs(LogViewWindow *view);

void set_status(LogViewWindow *view, const char *status, const char *style);
void set_filter(LogViewWindow *view, const char *filter);
void bring_to_front(LogViewWindow *view);

};
