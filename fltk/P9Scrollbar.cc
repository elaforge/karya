// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <math.h>
#include <algorithm>

#include "P9Scrollbar.h"


P9Scrollbar::P9Scrollbar(int X, int Y, int W, int H, Color bg, Color fg) :
    Fl_Group(X, Y, W, H), orientation(P9Scrollbar::vertical), min_size(4),
    scrollbox(X, Y, W, H)
{
    box(FL_FLAT_BOX);
    color(bg.fl());
    scrollbox.box(FL_FLAT_BOX);
    scrollbox.color(fg.fl());
    add(this->scrollbox);
    end();
    set_scroll(0, 1);
}


void
P9Scrollbar::resize(int x, int y, int w, int h)
{
    Fl_Widget::resize(x, y, w, h);
    this->set_scroll(offset, size);
}


void
P9Scrollbar::set_orientation(P9Scrollbar::Orientation o)
{
    this->orientation = o;
    this->set_scroll(offset, size);
}


void
P9Scrollbar::set_scroll(double offset, double size)
{
    this->offset = offset;
    this->size = size;
    if (orientation == P9Scrollbar::vertical) {
        int pos = std::min(h() - this->min_size, int(offset*h()));
        int sz = std::max(this->min_size, int(size*h()));
        // DEBUG("scroll pos " << DPoint(offset, size) << " actually "
        //         << IPoint(pos, sz));
        scrollbox.resize(x()+1, y() + pos, w()-2, sz);
    } else if (orientation == P9Scrollbar::horizontal) {
        int pos = std::min(w() - this->min_size, int(offset*w()));
        int sz = std::max(this->min_size, int(size*w()));
        scrollbox.resize(x() + pos, y()+1, sz, h()-2);
    } else {
        ASSERT(false); // unknown type
    }
    init_sizes();
    redraw();
}


void
P9Scrollbar::set_scroll_zoom(double max, double offset, double displayed_area)
{
    double scaled_offset = util::clamp(0.0, 1.0, offset / max);
    double size = util::clamp(0.0, max, displayed_area) / max;
    // DEBUG("set_scroll_zoom " << max << ", " << offset << ", "
    //         << displayed_area << " -> " << DPoint(scaled_offset, size));
    this->set_scroll(scaled_offset, size);
}


int
P9Scrollbar::handle(int evt)
{
    if (evt == FL_PUSH || evt == FL_RELEASE || evt == FL_DRAG) {
        double click;
        if (orientation == P9Scrollbar::vertical) {
            click = double(Fl::event_y() - this->y()) / this->h();
        } else {
            click = double(Fl::event_x() - this->x()) / this->w();
        }
        click -= this->size / 2;
        click = util::clamp(0.0, 1 - size, click);
        this->set_scroll(click, this->size);
        this->do_callback();
        return 1;
    }
    return 0;
}
