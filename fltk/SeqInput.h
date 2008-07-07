#ifndef __SEQ_INPUT_H
#define __SEQ_INPUT_H

#include <FL/Fl_Input.H>

#include "util.h"

class SeqInput : public Fl_Input {
public:
    SeqInput(int X, int Y, int W, int H, bool do_expansion);
    virtual void resize(int x, int y, int w, int h);
    // Fl_Input's value() isn't virtual so I get to make up my own.  Yay fltk!
    virtual void set_text(const char *text);
    // HACK:
    // An expanded SeqInput has overgrown its neighbors, so it needs special
    // handling to get events first and redraws last.  The parent can check
    // this to know if it should treat it specially.
    bool is_expanded() const { return expanded; }
    Color focus_color;
protected:
    int handle(int evt);
private:
    Point proper_size;
    bool expanded;
    bool do_expansion;
    void expand();
    void contract();
    void redraw_neighbors();
    static void changed_cb(Fl_Widget *w, void *vp);
};

#endif
