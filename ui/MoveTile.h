/*
Differences from Fl_Tile:
When dragging, only resize the widget to the left.  The widgets to the right
get moved.

Some children may be marked as non-resizable.  They are never resized, and
their entire area is available for drag.

The callback is called when things are dragged.
*/

#ifndef __MOVE_TILE_H
#define __MOVE_TILE_H

#include <vector>

#include <FL/Fl_Group.H>
// #include <FL/Fl_Tile.H>

// class MoveTile : public Fl_Tile {
class MoveTile : public Fl_Group {
public:
    // 'no_move' means you get normal Fl_Tile resizing behaviour.
    // 'move_negative' means widgets in the negative direction move, and
    // ones in the positive direction resize.
    // 'move_positive. is the other way around.
    enum MoveDirection { no_move, move_negative, move_positive };

    MoveTile(int X, int Y, int W, int H, char *label = 0) :
        // Fl_Tile(X, Y, W, H, label)
        Fl_Group(X, Y, W, H, label),
        hmove(no_move), vmove(no_move),
        minimum_size(5, 5),
        grab_area(4)
    {}

    virtual void resize(int X, int Y, int W, int H);
    virtual int handle(int evt);
    void drag_tile(Point drag_from, Point drag_to) {
        this->handle_drag_tile(drag_from, drag_to);
        this->init_sizes();
    }

    void set_move_direction(MoveDirection horizontal, MoveDirection vertical) {
        this->hmove = horizontal;
        this->vmove = vertical;
    }

    // Child won't resize, and its entire area is used for dragging.
    void set_stiff_child(int child);
    bool stiff_child(int child);

    // Pass these constants as child to get the "special" boxes.
    enum { GROUP_SIZE = -2, GROUP_RESIZABLE = -1 };
    // this should be const, but sizes_ isn't declared mutable
    Rect original_box(int child);

private:
    int handle_move(int evt, BoolPoint &drag_state, Point &drag_from);
    void handle_drag_tile(Point drag_from, Point drag_to);

    MoveDirection hmove, vmove;
    // Don't resize a pane smaller than the x and y here.
    Point minimum_size;
    std::vector<bool> stiff_children;

    // Allow dragging of panes this many pixels from the widget edges.
    const int grab_area;
};

#endif
