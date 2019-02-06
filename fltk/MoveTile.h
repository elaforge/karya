// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

/* Differences from Fl_Tile:
    When dragging, only resize the widget to the left.  The widgets to the
    right get moved, except the rightmost widget, which resizes as normal
    (it's intended to be a padding widget to take up the unused space).

    The children should always be sorted left to right, top to bottom.  There
    is a protected method 'sort_children' that subclasses should call after
    inserting widgets.  TODO Unfortunately FLTK's 'insert' is not virtual so
    this can't be done automatically.

    Some children may be marked as non-resizable ("stiff").  They are never
    resized, and their entire area can be dragged.

    The callback is called when things are dragged.

    BUGS:
    If the grab areas overlap it gets confused.  So minimum size should be
    > 2*grab_area.
*/

#pragma once

#include <vector>

#include <FL/Fl_Group.H>


class MoveTile : public Fl_Group {
public:
    // 'no_move' means you get normal Fl_Tile resizing behaviour.
    // 'move_negative' means widgets in the negative direction move, and
    // ones in the positive direction resize.
    // 'move_positive. is the other way around.
    enum MoveDirection { no_move, move_negative, move_positive };

    MoveTile(int x, int y, int w, int h, char *label = 0) :
        Fl_Group(x, y, w, h, label),
        minimum_size(10, 10),
        dragged_child(-1),
        grab_area(3)
    {}

    void resize(int x, int y, int w, int h) override;
    // remove and insert aren't virtual, so use these to manage children.
    virtual void remove_child(Fl_Widget *w);
    virtual void insert_child(Fl_Widget &w, int c);

    int handle(int evt) override;
    void drag_tile(IPoint drag_from, IPoint drag_to);

    // Child won't resize, and its entire area is used for dragging.
    void set_stiff_child(int child) {
        util::vector_put(stiff_children, child, true);
    }
    bool stiff_child(int child);

    // Pass these constants as child to get the "special" boxes.
    enum { GROUP_SIZE = -2, GROUP_RESIZABLE = -1 };
    // this should be const, but sizes_ isn't declared mutable
    IRect original_box(int child);

protected:
    // Put children in left->right, top->bottom order, and return true if
    // they weren't already sorted when this was called.
    bool sort_children();
    // Don't resize a pane smaller than the x and y here.
    const IPoint minimum_size;
    // Child currently being dragged.  Reset to -1 before a drag and after
    // a release.  It's here so subclass callbacks can get the dragged child.
    int dragged_child;

private:
    // int handle_move(int evt, BoolPoint &drag_state, IPoint &drag_from);
    int handle_move(int evt, BoolPoint *drag_state, int *dragged_child);
    void handle_drag_tile(IPoint drag_from, IPoint drag_to, int dragged_child);
    int find_dragged_child(IPoint drag_from, BoolPoint *drag_state);

    std::vector<bool> stiff_children;
    // Allow dragging of panes this many pixels from the widget edges.
    const int grab_area;
};
