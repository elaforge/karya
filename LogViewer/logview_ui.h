#ifndef __LOGVIEW_UI_H
#define __LOGVIEW_UI_H

#include <FL/Fl_Double_Window.H>
#include <FL/Fl_Group.H>
#include <FL/Fl_Output.H>
#include <FL/Fl_Text_Display.H>
#include <FL/Fl_Text_Buffer.H>


typedef void (*MsgCallback)(int callback_type, const char *);
enum CallbackType {
    cb_click = 1,
    cb_command = 2
};

class TextDisplay : public Fl_Text_Display {
public:
    TextDisplay(int X, int Y, int W, int H) : Fl_Text_Display(X, Y, W, H) {}
    int xy_to_position(int x, int y) {
        Fl_Text_Display::xy_to_position(x, y);
    }
};


class LogView : public Fl_Group {
public:
    LogView(int X, int Y, int W, int H, MsgCallback cb);
    void append_log(const char *msg, const char *style);
    void clear_logs();
    void set_status(const char *s) { status.value(s); }

protected:
    virtual int handle(int evt);
private:
    MsgCallback msg_callback;
    Fl_Text_Buffer buffer;
    Fl_Text_Buffer style_buffer;
    Fl_Output status;
    Fl_Input command;
    TextDisplay display;

    static void command_cb(Fl_Widget *w, void *vp);
};

class LogViewWindow : public Fl_Double_Window {
public:
    LogViewWindow(int X, int Y, int W, int H, MsgCallback cb);
    LogView view;
private:
};

#endif
