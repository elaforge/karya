// generic utilities

#ifndef __UTIL_H
#define __UTIL_H

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

typedef PointTmpl<int> Point;
typedef PointTmpl<double> DPoint;


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
	bool contains(const PointTmpl<T> &p) {
		return (x <= p.x && p.x < r() && y <= p.y && p.y < b());
	}

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

typedef RectTmpl<int> Rect;
typedef RectTmpl<double> DRect;


struct Color {
    Color(unsigned char r, unsigned char g, unsigned char b, unsigned char a=0)
        : r(r), g(g), b(b), a(a) {}
    Color(unsigned long rgba) :
        r(0xff & (rgba >> 24)), g(0xff & (rgba >> 16)), b(0xff & (rgba >> 8)),
            a(0xff & rgba) {}
    Color() : r(0), g(0), b(0), a(0) {}
    unsigned char r, g, b, a;
};

#endif
