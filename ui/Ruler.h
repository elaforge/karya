#include <FL/Fl_Box.H>

class Ruler : public Fl_Box {
public:
    Ruler(int X, int Y, int W, int H) :
        Fl_Box(X, Y, W, H)
    {
        box(FL_THIN_DOWN_BOX);
        color(FL_MAGENTA);
    }
};
