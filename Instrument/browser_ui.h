#ifndef __BROWSER_UI_H
#define __BROWSER_UI_H

/*
    A simple browser with a search box, a list to select from, and an info pane.

       body (Tile) --\
       /            info_pane (Fl_Text_Display)
    select_pane (Group) ____
        |                   \
    query (Input)   matches (Browser)
*/

#include <FL/Fl_Double_Window.H>
#include <FL/Fl_Group.H>
#include <FL/Fl_Tile.H>
#include <FL/Fl_Input.H>
#include <FL/Fl_Text_Display.H>
#include <FL/Fl_Hold_Browser.H>

#include "Util/fltk_interface.h"

enum MsgType { msg_select, msg_choose, msg_query };

class BrowserInput : public Fl_Input {
public:
    BrowserInput(int X, int Y, int W, int H, Fl_Hold_Browser *matches,
            MsgCallback cb) :
        Fl_Input(X, Y, W, H), matches(matches), msg_callback(cb)
    {}
    int handle(int evt);

private:
    Fl_Hold_Browser *matches;
    MsgCallback msg_callback;
};


class Browser : public Fl_Tile {
public:
    Browser(int X, int Y, int W, int H, MsgCallback cb);

    void insert_line(int n, const char *line) {
        matches.insert(n, line);
        matches.redraw();
    }
    void remove_line(int n) { matches.remove(n); matches.redraw(); }
    void set_info(const char *info);

private:
    Fl_Text_Buffer info_buffer;
    Fl_Text_Display info_pane;
    Fl_Group select_pane;
        BrowserInput query;
        Fl_Hold_Browser matches;

    MsgCallback msg_callback;
    static void query_cb(Fl_Widget *w, void *vp);
    static void matches_cb(Fl_Widget *w, void *vp);
};

class BrowserWindow : public Fl_Double_Window {
public:
    BrowserWindow(int X, int Y, int W, int H, const char *title,
        MsgCallback cb);
    Browser browser;
};

#endif
