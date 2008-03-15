/*
plan9 style scrollbar

bgcolor rectangle with an fgcolor rectangle on it

it redraws the fgcolor rectangle based on
scroll_position(double offset, double size)

Both are 0--1 and represent the percentage of the way down the scroll space,
and the percentage of the total space visible.  The scrollbar is proportional,
but has a minimum size, and won't go totally off the end.

zoom_position(TrackPos max, TrackPos display_size, ZoomInfo zoom)
{
    10, (5, .5)
}

Left click will scroll up/left to place the top/left line at the clicked
position.  Right click will scroll down.
Both put a bar across the new area to represent where the top or bottom of the
display area used to be.

Middle mouse button makes the top of the scrollbar immediately jump to the
clicked or dragged position.

A scroll becomes final when you release the mouse button, and is undone if you
press another button while the first is still down.

So this won't work very well on a mac mouse.
*/

#include <FL/Fl_Group.H>
#include <FL/Fl_Box.H>

#include "util.h"
#include "f_util.h"

class P9Scrollbar : public Fl_Group {
public:
    P9Scrollbar(int X, int Y, int W, int H, Color bg, Color fg);
    void resize(int x, int y, int w, int h);

    enum Orientation { horizontal, vertical };
    void set_orientation(P9Scrollbar::Orientation o);

    double get_offset() const { return offset; }
    double get_size() const { return size; }

    // Set scrollbar to 'offset' with given size.  Both are 0--1.
    void set_scroll(double offset, double size);
    void set_scroll_zoom(double max, double offset, double displayed_area);

protected:
    int handle(int evt);

private:
    Orientation orientation;
    double offset;
    double size;
    int min_size;
    Fl_Box scrollbox;
};
