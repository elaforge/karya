// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include "interface.h"


extern "C" {

BrowserWindow *
create_browser(int x, int y, int w, int h, const char *title, MsgCallback cb)
{
    BrowserWindow *win = new BrowserWindow(x, y, w, h, title, cb);
    win->show();
    return win;
}

void
insert_line(BrowserWindow *w, int n, const char *line)
{
    w->browser.insert_line(n, line);
}

void
remove_line(BrowserWindow *w, int n)
{
    w->browser.remove_line(n);
}

void
set_info(BrowserWindow *w, const char *info)
{
    w->browser.set_info(info);
}

}
