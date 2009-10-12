#include "util.h"
#include "f_util.h"

#include "SimpleScroll.h"


void
SimpleScroll::resize(int X, int Y, int W, int H)
{
    Point diff(W - w(), H - h());
    Fl_Widget *c = child(0);
    c->resize(X + offset.x, Y + offset.y, c->w() + diff.x, c->h() + diff.y);
    Fl_Widget::resize(X, Y, W, H);
}

void
SimpleScroll::set_offset(Point offset)
{
    Point shift(offset.x - this->offset.x, offset.y - this->offset.y);
    this->offset = offset;
    child(0)->position(child(0)->x() + shift.x, child(0)->y() + shift.y);
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
