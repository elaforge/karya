#include <FL/Fl_Tile.H>

class MoveTile : public Fl_Tile {
public:
    MoveTile(int X, int Y, int W, int H, char *label = 0) :
        Fl_Tile(X, Y, W, H, label)
    {}
};
