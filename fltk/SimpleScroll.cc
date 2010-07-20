#include "util.h"
#include "f_util.h"

#include "SimpleScroll.h"


void
SimpleScroll::resize(int X, int Y, int W, int H)
{
    IPoint diff(W - w(), H - h());
    Fl_Widget *c = child(0);
    c->resize(X + offset.x, Y + offset.y, c->w() + diff.x, c->h() + diff.y);
    Fl_Widget::resize(X, Y, W, H);
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
    c->resize(c->x() + shift.x, c->y() + shift.y,
            c->w() - shift.x, c->h() - shift.y);
    this->damage(FL_DAMAGE_SCROLL);
    // DEBUG("offset: " << show_damage(damage()));
}


void
SimpleScroll::draw()
{
    ClipArea clip(rect(this));
    if (this->damage() & FL_DAMAGE_SCROLL) {
        // TODO: implement proper scrolling
        // fl_scroll(...);
        Fl_Group::draw();
    } else {
        Fl_Group::draw();
    }
}
