// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

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

// This class assumes it has only one child, which is the same size as itself.
// When it resizes, the child will be resized along its right and bottom edges.
// Unfortunately there's no real way to enforce a single child, so don't do
// that.
class SimpleScroll : public Fl_Group {
public:
    SimpleScroll(int x, int y, int w, int h) :
        Fl_Group(x, y, w, h), offset(0, 0)
    {}
    void resize(int x, int y, int w, int h) override;

    IPoint get_offset() const { return offset; }
    void set_offset(IPoint offset);

protected:
    void draw() override;

private:
    IPoint offset;
};

#endif
