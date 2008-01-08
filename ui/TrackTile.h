#include "MoveTile.h"

class TrackTile : public MoveTile {
public:
    TrackTile(int X, int Y, int W, int H) :
        MoveTile(X, Y, W, H)
    {}
};
