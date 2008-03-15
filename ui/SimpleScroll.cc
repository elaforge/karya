#include "util.h"
#include "f_util.h"

#include "SimpleScroll.h"

void
SimpleScroll::set_offset(Point offset)
{
    Point shift(offset.x - this->offset.x, offset.y - this->offset.y);
    this->offset = offset;
    // DEBUG("set offset " << offset);
    for (int i = 0; i < children(); i++) {
        // DEBUG(i << ": " << show_widget(child(i)) << " -> " << shift);
        child(i)->position(child(i)->x() + shift.x, child(i)->y() + shift.y);
    }
    this->damage(FL_DAMAGE_SCROLL);
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
