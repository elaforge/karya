// fltk utilities (no dependencies on sequencer)
#include <FL/Fl.H>
#include <FL/Fl_Widget.H>
#include <FL/Fl_Group.H>
#include <iostream>

template<class T>
struct Point_tmpl {
	T x, y;
	Point_tmpl(T x, T y) : x(x), y(y) {};
	Point_tmpl() : x(0), y(0) {};
};

template<class T>
struct Rect_tmpl {
	T x, y, w, h;
	Rect_tmpl(T x, T y, T w, T h) : x(x), y(y), w(w), h(h) {};
	Rect_tmpl() : x(0), y(0), w(0), h(0) {};
	T r() const { return x + w; }
	void r(T v) { x = v - w; }
	T b() const { return y + h; }
	void b(T v) { y = v - h; }
};

template <class T> inline std::ostream &
operator<<(std::ostream &os, const Point_tmpl<T> &p)
{
	return os << "Point(" << p.x << ", " << p.y << ")";
}

template <class T> inline std::ostream &
operator<<(std::ostream &os, const Rect_tmpl<T> &r)
{
	return os << "Rect(" << r.x << ", " << r.y << ", " << r.w << ", " << r.h << ")";
}

typedef Rect_tmpl<double> Drect;
typedef Rect_tmpl<int> Rect;

typedef Point_tmpl<int> Point;
typedef Point_tmpl<double> Dpoint;

inline Rect rect_from_widget(const Fl_Widget &w)
{
	return Rect(w.x(), w.y(), w.w(), w.h());
}
/*
inline Rect rect_from_widget(const Fl_Widget *w)
{
	return rect_from_widget(*w);
} */

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

template<class T> inline T
max(T a, T b)
{
	return a > b ? a : b;
}

template<class T>
inline T min(T a, T b)
{
	return a < b ? a : b;
}

void print_children(Fl_Group *w);
void print_children_r(Fl_Group *w, int recurse = 0);
