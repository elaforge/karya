#ifndef __SEQ_INPUT_H
#define __SEQ_INPUT_H

#include <FL/Fl_Input.H>

#include "util.h"

// A customized Fl_Input that can expand on demand for entered text, and
// collapse back down to size when it loses focus.
class SeqInput : public Fl_Input {
public:
    SeqInput(int X, int Y, int W, int H, bool do_expansion,
        bool strip_text = true);
    virtual void resize(int x, int y, int w, int h);
    // Fl_Input_::value() isn't virtual so I get to make up my own.  Yay fltk!
    virtual void set_text(const char *text);
    // Same story with Fl_Input_::insert().
    virtual void insert_text(const char *text);
    // HACK:
    // An expanded SeqInput has overgrown its neighbors, so it needs special
    // handling to get events first and redraws last.  The parent can check
    // this to know if it should treat it specially.
    bool is_expanded() const { return expanded; }

    // SeqInput already uses the single callback provided by fltk.  I could
    // override callback() here, but it's not virtual so overriding is a bit
    // sketchy.  A different name is clear at least.
    // This is used to show and hide the block title.
    void set_callback2(Fl_Callback cb, void *vp) {
        callback2 = cb; callback2_arg = vp;
    }
    Color focus_color;
protected:
    int handle(int evt);
private:
    IPoint proper_size;
    bool expanded;
    bool do_expansion;
    const bool strip_text;
    void expand();
    void contract();
    void redraw_neighbors();
    Fl_Callback *callback2;
    void *callback2_arg;
    static void changed_cb(Fl_Widget *w, void *vp);
};

#endif
