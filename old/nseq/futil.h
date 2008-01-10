// fltk utilities

#ifndef _FUTIL_H
#define _FUTIL_H

#include <FL/Fl.H>
#include <FL/Fl_Widget.H>
#include <FL/Fl_Group.H>
#include <FL/fl_draw.H>
#include <iostream>
#include <string>

// this is turned on during zooming to indicate to children that they may
// draw in a simpler style
enum { DAMAGE_ZOOM = FL_DAMAGE_USER1 };

// use AIRA to manage the fltk clipping stack
struct Clip_area {
	Clip_area(Rect d) {
		fl_push_clip(d.x, d.y, d.w, d.h);
	}
	~Clip_area() { fl_pop_clip(); }
};


/*
selection in trackpos
it keeps track of color, current selection, and the minimal area that
needs to be redrawn from the last selection. It's up to the client to
turn trackpos into pixels and draw the actual rectangle. Selections are
ranges only, so a multiline text selection would be a start and extent
of (column, character) pairs.
how to represent "no selection"?
an out of range selection is the most elegant, but Selection needs to know what
out of range is then so draw_range won't generate spurious regions
*/
template<class T>
class Selection {
public:
	Selection() : sel_null(true), old_sel_null(true) {}
	bool get_selection(Range_tmpl<T> &r) const {
		r = sel;
		return !sel_null;
	}
	bool get_old_selection(Range_tmpl<T> &r) const {
		r = old_sel;
		return !old_sel_null;
	}
	void selection(const Range_tmpl<T> &o) { sel = o; sel_null = false; }
	void selection(bool n) { sel_null = !n; }
	Range_tmpl<T> draw_range() {
		Range_tmpl<T> r;
		if (sel_null && old_sel_null)
			return r;
		else if (old_sel_null)
			r = sel;
		else if (sel_null)
			r = old_sel;
		else {
			Range_tmpl<T> left(old_sel.start, sel.start - old_sel.start),
				right(old_sel.end(), sel.end() - old_sel.end());
			if (left.empty())
				r = right;
			else if (right.empty())
				r = left;
			else
				r = Range_tmpl<T>(left.start, right.end() - left.start);
		}
		old_sel = sel;
		old_sel_null = sel_null;
		return r;
	}
	Fl_Color color;
private:
	Range_tmpl<T> sel, old_sel;
	bool sel_null, old_sel_null;
};


inline Rect
rect(const Fl_Widget &w) { return Rect(w.x(), w.y(), w.w(), w.h()); }
inline Rect
rect(const Fl_Widget *w) { return rect(*w); }


// debugging
char *show_event(int ev);
char *show_damage(uchar d);

inline void
print_event(int ev)
{
	printf("event: %s\n", show_event(ev));
}

std::ostream &operator<<(std::ostream &os, const Fl_Widget &w);
void remove_children(Fl_Group &g);

// unpack the crazy Fl_Group::sizes() array
// 0 1 2 3 - x r y b obj
inline Rect
rect_from_sizes(short *p)
{
	return Rect(p[0], p[2], p[1] - p[0], p[3] - p[2]);
}

inline Point
mouse_pos()
{
	return Point(Fl::event_x(), Fl::event_y());
}

void print_children(const Fl_Widget *w, int nlevels=-1, int recurse = 0);
void print_widget(const Fl_Widget *w);
std::string show_widget(const Fl_Widget *w);

#endif
