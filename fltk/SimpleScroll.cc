// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include "f_util.h"
#include "util.h"

#include "SimpleScroll.h"


void
SimpleScroll::resize(int x, int y, int w, int h)
{
    IPoint diff(w - this->w(), h - this->h());
    Fl_Widget *c = child(0);
    c->resize(x + offset.x, y + offset.y, c->w() + diff.x, c->h() + diff.y);
    Fl_Widget::resize(x, y, w, h);
}

void
SimpleScroll::set_offset(IPoint offset)
{
    IPoint shift(offset.x - this->offset.x, offset.y - this->offset.y);
    this->offset = offset;
    // When scrolling in negative x and y (so the child's x and y are getting
    // smaller), grow the child so its right edge stays in the same place.
    // When scrolling back, shrink it.  The assumption is that the child is
    // prepared to grow on the right and bottom edges.
    Fl_Widget *c = child(0);
    c->resize(
        c->x() + shift.x, c->y() + shift.y,
        c->w() - shift.x, c->h() - shift.y);
    this->damage(FL_DAMAGE_SCROLL);
    // DEBUG("offset: " << show_damage(damage()));
}


void
SimpleScroll::draw()
{
    f_util::ClipArea clip(f_util::rect(this));
    if (this->damage() & FL_DAMAGE_SCROLL) {
        // TODO: implement proper scrolling
        // fl_scroll(...);
        Fl_Group::draw();
    } else {
        Fl_Group::draw();
    }
}
