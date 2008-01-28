// generic utilities

#ifndef __UTIL_H
#define __UTIL_H

#include <iostream>
#include <stdexcept>
#include <vector>

// so code has access to debugging printf
#include <stdio.h>

// Point

template<class T>
struct PointTmpl {
	T x, y;
	PointTmpl(T x, T y) : x(x), y(y) {};
	PointTmpl() : x(), y() {};
	bool operator==(const PointTmpl<T> &o) const {
		return o.x == x && o.y == y;
	}
	bool operator!=(const PointTmpl<T> &o) const { return !(*this == o); }
};

template <class T> inline std::ostream &
operator<<(std::ostream &os, const PointTmpl<T> &p)
{
    return os << "Point(" << p.x << ", " << p.y << ")"; 
}


typedef PointTmpl<int> Point;
typedef PointTmpl<double> DPoint;
typedef PointTmpl<bool> BoolPoint;


// Rect

template<class T>
struct RectTmpl {
	T x, y, w, h;
	RectTmpl(T x, T y, T w, T h) : x(x), y(y), w(w), h(h) {}
	RectTmpl() : x(), y(), w(), h() {}
	template <class U> RectTmpl(const RectTmpl<U> &o) :
		x(U(o.x)), y(U(o.y)), w(U(o.w)), h(U(o.h))
	{}

	bool operator==(const RectTmpl<T> &o) const {
		return o.x == x && o.y == y && o.w == w && o.h == h;
	}
	bool operator!=(const RectTmpl<T> &o) const { return !(this == o); }

	T r() const { return x + w; }
	void r(T v) { x = v - w; }
	T b() const { return y + h; }
	void b(T v) { y = v - h; }
    // Overlaps upper left edge, inside lower right edge.
	bool contains(const PointTmpl<T> &p) {
		return (x <= p.x && p.x < r() && y <= p.y && p.y < b());
	}
    bool intersects(const PointTmpl<T> &p) {
        return ! (p.r() < x || p.b() < y || p.x >= r() || p.y >= b());
    }

    void size(T w, T h) { this->w = w; this->h = h; }

	// move this to be inside 'o'.  if 'o' is too large, overflow to +xy
	void clamp(const RectTmpl<T> &o) {
		r(min(r(), o.r()));
		b(min(b(), o.b()));
		x = max(x, o.x);
		y = max(y, o.y);
	}

	void union_(const RectTmpl<T> &o) {
		if (w == T() || o.w == T()) { // empty dimensions have no effect
			w = max(x, o.w);
			x = max(x, o.x);
		} else {
			w = max(r(), o.r()) - min(x, o.x);
			x = min(x, o.x);
		}
		if (h == T() || o.h == T()) {
			h = max(h, o.h);
			y = max(y, o.y);
		} else {
			h = max(b(), o.b()) - min(y, o.y);
			y = min(y, o.y);
		}
	}

	// clip this to lie within o
	void clip(const RectTmpl<T> &o) {
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

	void normalize() {
		if (w < T()) {
			w = -w;
			x -= w;
		}
		if (h < T()) {
			h = -h;
			y -= h;
		}
	}
};

template <class T> inline std::ostream &
operator<<(std::ostream &os, const RectTmpl<T> &r)
{
    return os << "Rect(" << r.x << ", " << r.y << ", " << r.w << ", "
        << r.h << ")";
}


typedef RectTmpl<int> Rect;
typedef RectTmpl<double> DRect;


struct Color {
    Color(unsigned char r, unsigned char g, unsigned char b, unsigned char a=0)
        : r(r), g(g), b(b), a(a) {}
    Color(unsigned long rgb) :
        r(0xff & (rgb >> 16)), g(0xff & (rgb >> 8)), b(0xff & rgb), a(0) {}

    Color() : r(0), g(0), b(0), a(0) {}
    unsigned char r, g, b, a;
};

inline std::ostream &
operator<<(std::ostream &os, const Color &c)
{
    return os << "Color(" << (int) c.r << ", " << (int) c.g << ", "
        << (int) c.b << ", " << (int) c.a << ")";
}

// Color(unsigned long rgba) :
//     r(0xff & (rgba >> 24)), g(0xff & (rgba >> 16)), b(0xff & (rgba >> 8)),
//         a(0xff & rgba) {}


// assertions
// I use exceptions so c++ errors won't necessarily crash the whole program.

#define ASSERT(x) if (!(x)) do { \
    AssertionError a(#x, __FILE__, __func__, __LINE__); \
    std::cerr << "assertion: " << a << '\n'; \
    throw a; \
} while (0)


struct AssertionError : std::exception {
    AssertionError(const char *expr, const char *file, const char *func,
            int line) :
        expr(expr), file(file), func(func), line(line)
    {}
    const char *expr, *file, *func;
    const int line;
};

inline std::ostream &
operator<<(std::ostream &os, const AssertionError &a)
{
    return os << "<assertion failed at " << a.file << ':' << a.line << ' '
        << a.func << "(): '" << a.expr << "'>";
}


// Supposedly these are in <algorithm>, but when I include that, they're not.
template<class T> inline T
max(T a, T b)
{
    return a > b ? a : b;
}

template<class T> inline T
min(T a, T b)
{
    return a < b ? a : b;
}


#define DEBUG(X) do { std::cout << __FILE__ << ':' << __LINE__ << ' ' \
    << X << '\n'; } while (0)


template <class T> inline std::ostream &
operator<<(std::ostream &os, const std::vector<T> &a)
{
    os << '[';
    for (int i = 0; i < a.size(); i++) {
        if (i)
            os << ", ";
        os << a[i];
    }
    os << ']';
    return os;
}


#endif
