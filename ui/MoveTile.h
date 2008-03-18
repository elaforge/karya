/*
Differences from Fl_Tile:
When dragging, only resize the widget to the left.  The widgets to the right
get moved, except the rightmost widget, which resizes as normal (it's intended
to be a padding widget to take up the unused space).

The children should always be sorted left to right, top to bottom.  There is
a protected method 'sort_children' that subclasses should call after inserting
widgets.  Unfortunately FLTK's 'insert' is not virtual so this can't be done
automatically.

Some children may be marked as non-resizable.  They are never resized, and
their entire area is available for drag.

The callback is called when things are dragged.

BUGS:
If the grab areas overlap it gets confused.  So minimum size should be
> 2*grab_area.
*/

#ifndef __MOVE_TILE_H
#define __MOVE_TILE_H

#include <vector>

#include <FL/Fl_Group.H>

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
        minimum_size(10, 10),
        hmove(no_move), vmove(no_move),
        grab_area(3)
    {}

    virtual void resize(int X, int Y, int W, int H);
    virtual int handle(int evt);
    void drag_tile(Point drag_from, Point drag_to);

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

protected:
    // Put children in left->right, top->bottom order, and return true if
    // they weren't already sorted when this was called.
    bool sort_children();
    // Don't resize a pane smaller than the x and y here.
    const Point minimum_size;

private:
    // int handle_move(int evt, BoolPoint &drag_state, Point &drag_from);
    int handle_move(int evt, BoolPoint *drag_state, int *dragged_child);
    void handle_drag_tile(Point drag_from, Point drag_to, int dragged_child);
    int find_dragged_child(Point drag_from, BoolPoint *drag_state);
    int previous_track(int i) const;

    MoveDirection hmove, vmove;
    std::vector<bool> stiff_children;

    // Allow dragging of panes this many pixels from the widget edges.
    const int grab_area;
};

#endif
