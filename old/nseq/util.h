#ifndef _UTIL_H
#define _UTIL_H

#include <algorithm>
#include <iostream>

using std::min;
using std::max;

template<class T>
struct Point_tmpl {
	T x, y;
	Point_tmpl(T x, T y) : x(x), y(y) {}
	Point_tmpl() : x(), y() {}
	bool operator==(const Point_tmpl<T> &o) const {
		return o.x == x && o.y == y;
	}
	bool operator!=(const Point_tmpl<T> &o) const {
		return !(*this == o);
	}
};

template<class T> inline std::ostream &
operator<<(std::ostream &os, const Point_tmpl<T> &p)
{
	return os << "Point(" << p.x << ", " << p.y << ")";
}

template<class T>
struct Rect_tmpl {
	T x, y, w, h;

	Rect_tmpl(T X, T Y, T W, T H) : x(X), y(Y), w(W), h(H) {}
	bool operator==(const Rect_tmpl<T> &o) const {
		return o.x == x && o.y == y && o.w == w && o.h == h;
	}
	bool operator!=(const Rect_tmpl<T> &o) const {
		return !(*this == o);
	}

	T r() const { return x + w; }
	void r(T v) { x = v - w; }
	T b() const { return y + h; }
	void b(T v) { y = v - h; }
	bool contains(const Point_tmpl<T> &p) {
		return (x <= p.x && p.x < r() && y <= p.y && p.y < b());
	}

	// move this to be inside 'o'.  if 'o' is too large, overflow to +xy
	void clamp(const Rect_tmpl<T> &o) {
		r(min(r(), o.r()));
		b(min(b(), o.b()));
		x = max(x, o.x);
		y = max(y, o.y);
	}

	// clip this to lie within o
	void clip(const Rect_tmpl<T> &o) {
		if (x < o.x) {
			w -= o.x - x;
			x = o.x;
		}
		if (y < o.y) {
			h -= o.y - y;
			y = o.y;
		}
		if (r() > o.r())
			w = o.r() - x;
		if (b() > o.b())
			h = o.b() - y;
	}
};

template<class T>
struct Range_tmpl {
	T start, extent;
	Range_tmpl() : start(), extent() {};
	explicit Range_tmpl(T start_, T extent_ = T()) :
		start(start_), extent(extent_)
	{
		if (extent < T()) {
			extent = -extent;
			start -= extent;
		}
	}
	T end() const { return start + extent; }
	bool contains(const T &o) const {
		return start <= o && o < end();
	}
	bool empty() const { return extent == T(); }
	void clip(const Range_tmpl<T> &o) {
		if (start < o.start) {
			extent -= o.start - start;
			start = o.start;
		}
		if (end() > o.end())
			extent = o.end() - start;
	}
	bool operator==(const Range_tmpl<T> &o) const {
		return o.start == start && o.extent == extent;
	}
	bool operator!=(const Range_tmpl<T> &o) const { return !(*this == o); }
};

template <class T> inline std::ostream &
operator<<(std::ostream &os, const Rect_tmpl<T> &r)
{
	return os << "Rect(" << r.x << ", " << r.y << ", "
		<< r.w << ", " << r.h << ")";
}

typedef Range_tmpl<int> Range;

typedef Rect_tmpl<double> Drect;
typedef Rect_tmpl<int> Rect;

typedef Point_tmpl<int> Point;
typedef Point_tmpl<double> Dpoint;


template<class T> typename T::size_type
container_clamp(const T &c, typename T::size_type at)
{
	// or possibly throw exception if out of range
	if (at < 0)
		return 0;
	else if (at >= c.size())
		return c.size();
	else
		return T::size_type(0);
}


#endif
