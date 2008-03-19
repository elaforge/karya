#ifndef __SEQ_INPUT_H
#define __SEQ_INPUT_H

#include <FL/Fl_Input.H>

#include "util.h"

class SeqInput : public Fl_Input {
public:
    SeqInput(int X, int Y, int W, int H);
    Color focus_color;
protected:
    int handle(int evt);
private:
    static void changed_cb(Fl_Widget *w, void *vp);
};

#endif
