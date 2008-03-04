// FLTK utilities

#ifndef __F_UTIL_H
#define __F_UTIL_H

#include <FL/Fl.H>
#include <FL/fl_draw.H>
#include <FL/Fl_Widget.H>

#include "util.h"


// use more convenient types with standard fltk functions

inline Rect
rect(const Fl_Widget &w) { return Rect(w.x(), w.y(), w.w(), w.h()); }
inline Rect
rect(const Fl_Widget *w) { return rect(*w); }

inline Fl_Color
color_to_fl(const Color &c)
{
    return fl_rgb_color(c.r, c.g, c.b);
}

inline Point
mouse_pos()
{
    return Point(Fl::event_x(), Fl::event_y());
}


// printing stuff for debugging

const char *show_event(int ev);
const char *show_damage(uchar d);
const char *show_widget(const Fl_Widget *w);
const char *show_children(const Fl_Widget *w, int nlevels=-1, int recurse=0);

void print_widget(const Fl_Widget *w);
void print_children(const Fl_Widget *w, int nlevels=-1, int recurse = 0);

#endif
