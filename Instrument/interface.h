// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include "Util/fltk_interface.h"
#include "browser_ui.h"

extern "C" {

BrowserWindow *create_browser(int x, int y, int w, int h, const char *title,
    MsgCallback cb);

void insert_line(BrowserWindow *w, int n, const char *line);
void remove_line(BrowserWindow *w, int n);
void set_info(BrowserWindow *w, const char *info);

};

