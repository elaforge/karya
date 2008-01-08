// FLTK utilities

#ifndef __F_UTIL_H
#define __F_UTIL_H

#include <FL/fl_draw.H>
#include <FL/Fl_Widget.H>

#include "util.h"


inline Rect
rect(const Fl_Widget &w) { return Rect(w.x(), w.y(), w.w(), w.h()); }
inline Rect
rect(const Fl_Widget *w) { return rect(*w); }

char *show_event(int ev);
void print_event(int ev);

char *show_damage(uchar d);

char *show_widget(const Fl_Widget *w);
void print_widget(const Fl_Widget *w);

void print_children(const Fl_Widget *w, int nlevels=-1, int recurse = 0);

inline Fl_Color
color_to_fl(const Color &c)
{
    return fl_rgb_color(c.r, c.g, c.b);
}

#endif
