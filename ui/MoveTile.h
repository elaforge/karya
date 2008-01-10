/*
Differences from Fl_Tile:
When dragging, only resize the widget to the left.  The widgets to the right
get moved.

Some children may be marked as non-resizable.  They are never resized, and
their entire area is available for drag.
*/

#ifndef __MOVE_TILE_H
#define __MOVE_TILE_H

#include <FL/Fl_Tile.H>

class MoveTile : public Fl_Tile {
public:
    MoveTile(int X, int Y, int W, int H, char *label = 0) :
        Fl_Tile(X, Y, W, H, label)
    {}
    // Move widgets dragged in this direction.
    void set_move_direction(bool horizontal) {}
    // Child 'i' won't resize, and its entire area is used for dragging.
    void set_child_not_resizable(int i) {}
};

#endif
