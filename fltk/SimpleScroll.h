// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

#include <FL/Fl_Scroll.H>

#include "global.h"

/*
   This is a scroll that expects a single child, and scrolls by moving it.
   The child will be resized so as to cover the entire size.  It's assumed
   that the child will be stretchy on its right and bottom edges.

   Fl_Scroll has automatic scrollbars, and it uses fl_scroll to move its
   children without redrawing them, but then requires them to be able to
   draw incrementally to draw the revealed sliver.  Unfortunately I can't
   do that... I forget exactly why but I think due to floating point rounding
   to pixels, since events have floating point positions.
*/
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
