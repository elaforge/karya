#ifndef _TYPES_H
#define _TYPES_H

#include "util.h"
#include "FL/Fl_Widget.H"

enum { Horizontal_time, Vertical_time };

#define HV(a, b) (orientation == Horizontal_time ? a : b)
#define METH(NAME, A1, B1, A2, B2) \
	template<class T> T \
	NAME (const Rect_tmpl<T> &o) const { \
		return (orientation == Horizontal_time ? A1 : B1); \
	} \
	template<class T> T \
	NAME (const Point_tmpl<T> &o) const { \
		return (orientation == Horizontal_time ? A1 : B1); \
	} \
	int NAME (const Fl_Widget &o) const { \
		return (orientation == Horizontal_time ? A2 : B2); \
	} \
	int NAME (const Fl_Widget *o) const { return NAME (*o); }


struct Orientation {
	Orientation(int o = 0) : orientation(o) {}
	// template<class T, class U> T time_pos(const U &o) const {
	// 	return HV(o.x, o.y);
	// }
	// template<> int time_pos(const Fl_Widget &w) const { return HV(w.x(), w.y()); }

	METH(x, o.x, o.y, o.x(), o.y())
	METH(y, o.y, o.x, o.y(), o.x())
	METH(w, o.w, o.h, o.w(), o.h())
	METH(h, o.h, o.w, o.h(), o.w())
	METH(r, o.r(), o.b(), o.x() + o.w(), o.y() + o.h())
	METH(b, o.b(), o.r(), o.y() + o.h(), o.x() + o.w())
	
	void resize(Fl_Widget &o, int x, int y, int w, int h) const {
		HV(o.resize(x, y, w, h), o.resize(y, x, h, w));
	}
	void resize(Fl_Widget *o, int x, int y, int w, int h) const {
		resize(*o, x, y, w, h);
	}
	void position(Fl_Widget &o, int x, int y) const {
		resize(o, x, y, w(o), h(o));
	}
	void position(Fl_Widget *o, int x, int y) const {
		resize(*o, x, y, w(o), h(o));
	}
	void size(Fl_Widget &o, int w, int h) const {
		resize(o, x(o), y(o), w, h);
	}
	void size(Fl_Widget *o, int w, int h) const {
		resize(*o, x(o), y(o), w, h);
	}
	template<class T>
	Point_tmpl<T> point(T x, T y) const {
		return HV(Point_tmpl<T>(x, y), Point_tmpl<T>(y, x));
	}
	template<class T>
	Point_tmpl<T> point(Point_tmpl<T> p) const {
		return HV(Point_tmpl<T>(p.x, p.y), Point_tmpl<T>(p.y, p.x));
	}
	template<class T>
	Rect_tmpl<T> rect(T x, T y, T w, T h) const {
		return HV(Rect_tmpl<T>(x, y, w, h), Rect_tmpl<T>(y, x, h, w));
	}
	template<class T>
	Rect_tmpl<T> rect(Rect_tmpl<T> p) const {
		return HV(Rect_tmpl<T>(p.x, p.y, p.w, p.h), Rect_tmpl<T>(p.y, p.x, p.h, p.w));
	}
	// doesn't actually work though
	/*
	template<class T, class U>
	U call(T f, int x, int y, int w, int h) {
		return HV(f(x, y, w, h), f(y, x, h, w));
	} */

	bool operator==(const Orientation &o) const {
		return orientation == o.orientation;
	}
	bool operator==(int o) const { return orientation == o; }
	bool operator!=(const Orientation &o) const {
		return orientation != o.orientation;
	}
	bool operator!=(int o) const { return orientation != o; }
	char orientation;
};
#undef HV
#undef METH

#endif
