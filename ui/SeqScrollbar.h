#include <FL/Fl_Scrollbar.H>

class SeqScrollbar : public Fl_Scrollbar {
public:
    SeqScrollbar(int X, int Y, int W, int H, char *label = 0) :
        Fl_Scrollbar(X, Y, W, H, label)
    {}
};
