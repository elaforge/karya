// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

// FLTK utilities

#ifndef __F_UTIL_H
#define __F_UTIL_H

#include <ostream>

#include <FL/Fl.H>
#include <FL/fl_draw.H>
#include <FL/Fl_Widget.H>

#include "Color.h"


namespace f_util {

// use more convenient types with standard fltk functions

inline IRect
rect(const Fl_Widget &w) { return IRect(w.x(), w.y(), w.w(), w.h()); }
inline IRect
rect(const Fl_Widget *w) { return rect(*w); }

inline IPoint
mouse_pos()
{
    return IPoint(Fl::event_x(), Fl::event_y());
}


// printing stuff for debugging

const char *show_key(int key);
// Text for the event itself.
const char *show_event(int ev);
// Show extra event information, such as char for key events, position for
// mouse, etc.
const char *show_event_info(int ev);
// Show Fl::event_state().
const char *show_event_state(int state);
const char *show_damage(uchar d);
const char *show_widget(const Fl_Widget *w);
// nlevels is how deep to display children.  A negative number displays all
// children.
const char *show_children(const Fl_Widget *w, int nlevels=-1);

// A wrapper around 'string' that prints it surrounded by double quotes.
class show_string {
public:
    show_string(const char *s) : s(s) {}
    const char *s;
};

std::ostream &operator<<(std::ostream &os, const show_string &s);

void print_widget(const Fl_Widget *w);
// This is the same as 'show_children', but print them.
void print_children(const Fl_Widget *w, int nlevels=-1);

// RAII style clipping

struct ClipArea {
    ClipArea(IRect r) { fl_push_clip(r.x, r.y, r.w, r.h); }
    ~ClipArea() { fl_pop_clip(); }
};


// Intersect 'r' with the clip area.
inline IRect
clip_rect(IRect r)
{
    int x, y, w, h;
    fl_clip_box(r.x, r.y, r.w, r.h, x, y, w, h);
    return IRect(x, y, w, h);
}

// draw

Fl_Color color_cycle();
void draw(const IRect &rect, Color color);

}

#endif
