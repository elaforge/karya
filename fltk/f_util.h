// FLTK utilities

#ifndef __F_UTIL_H
#define __F_UTIL_H

#include <ostream>

#include <FL/Fl.H>
#include <FL/fl_draw.H>
#include <FL/Fl_Widget.H>

#include "util.h"


// use more convenient types with standard fltk functions

inline IRect
rect(const Fl_Widget &w) { return IRect(w.x(), w.y(), w.w(), w.h()); }
inline IRect
rect(const Fl_Widget *w) { return rect(*w); }

inline Fl_Color
color_to_fl(const Color &c)
{
    return fl_rgb_color(c.r, c.g, c.b);
}

inline IPoint
mouse_pos()
{
    return IPoint(Fl::event_x(), Fl::event_y());
}


// printing stuff for debugging

const char *show_key(int key);
const char *show_event(int ev);
const char *show_event_info(int ev);
const char *show_event_state(int state);
const char *show_damage(uchar d);
const char *show_widget(const Fl_Widget *w);
const char *show_children(const Fl_Widget *w, int nlevels=-1, int recurse=0);

class show_string {
public:
    show_string(const char *s) : s(s) {}
    const char *s;
};

std::ostream &operator<<(std::ostream &os, const show_string &s);

void print_widget(const Fl_Widget *w);
void print_children(const Fl_Widget *w, int nlevels=-1, int recurse = 0);

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

#endif
