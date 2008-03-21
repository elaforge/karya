/*
Fl_Scroll does random extraneous crap like wanting to keep track of scrollbars
and not resizing its children.  This is just the bare minimum scrolling widget,
that also resizes like a normal Fl_Group.

Actualy, for now it's just Fl_Scroll that modifies resize();
*/
#ifndef __SIMPLE_SCROLL_H
#define __SIMPLE_SCROLL_H

#include <FL/Fl_Scroll.H>

#include "util.h"

class SimpleScroll : public Fl_Group {
public:
    SimpleScroll(int X, int Y, int W, int H) :
        Fl_Group(X, Y, W, H), offset(0, 0)
    {}

    Point get_offset() const { return offset; }
    void set_offset(Point offset);

protected:
    void draw();

private:
    Point offset;
};

#endif
