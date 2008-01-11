#ifndef __SEQ_INPUT_H
#define __SEQ_INPUT_H

#include <FL/Fl_Input.H>

class SeqInput : public Fl_Input {
public:
    SeqInput(int X, int Y, int W, int H, char *label = 0) :
        Fl_Input(W, Y, W, H, label)
    {}
};

#endif
