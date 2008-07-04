#include "Util/fltk_interface.h"
#include "browser_ui.h"

extern "C" {

BrowserWindow *create_browser(int x, int y, int w, int h, MsgCallback cb);

void insert_line(BrowserWindow *w, int n, const char *line);
void remove_line(BrowserWindow *w, int n);
void set_info(BrowserWindow *w, const char *info);

};

