// generic utilities, have no dependence except on stdlib
#include <memory>
#include <iostream>
#include <stdexcept>

struct Assertion_error : std::exception {
public:
	Assertion_error(const char *expr, const char *file, const char *func, int line) :
		expr(expr), file(file), func(func), line(line)
	{}
	const char *expr, *file, *func;
	int line;
};

inline std::ostream &
operator<<(std::ostream &os, const Assertion_error &a)
{
	return os << "<assertion failed at " << a.file << ':' << a.line << ' '
		<< a.func << "(): '" << a.expr << "'>";
}

#define Assert(x) if (!(x)) do { \
	Assertion_error a(#x, __FILE__, __func__, __LINE__); \
	std::cerr << "assertion: " << a << '\n'; \
	throw a; \
} while (0)

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

template<class T>
struct Point_tmpl {
	T x, y;
	Point_tmpl(T x, T y) : x(x), y(y) {};
	Point_tmpl() : x(), y() {};
	bool operator==(const Point_tmpl<T> &o) const {
		return o.x == x && o.y == y;
	}
	bool operator!=(const Point_tmpl<T> &o) const { return !(*this == o); }
};

template<class T>
struct Rect_tmpl {
	T x, y, w, h;
	Rect_tmpl(T x, T y, T w, T h) : x(x), y(y), w(w), h(h) {}
	Rect_tmpl() : x(), y(), w(), h() {}
	template <class U> Rect_tmpl(const Rect_tmpl<U> &o) :
		x(U(o.x)), y(U(o.y)), w(U(o.w)), h(U(o.h))
	{}
	bool operator==(const Rect_tmpl<T> &o) const {
		return o.x == x && o.y == y && o.w == w && o.h == h;
	}
	bool operator!=(const Rect_tmpl<T> &o) const { return !(this == o); }
	
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
	void union_(const Rect_tmpl<T> &o) {
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
operator<<(std::ostream &os, const Range_tmpl<T> &p)
{
	return os << "Range(" << p.start << "--" << p.end() << ")";
}

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

typedef Range_tmpl<int> Range;

typedef Rect_tmpl<double> Drect;
typedef Rect_tmpl<int> Rect;

typedef Point_tmpl<int> Point;
typedef Point_tmpl<double> Dpoint;

// yes, these are in <algorithm>
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

template<class T> inline T
clamp(T lo, T v, T hi)
{
	if (v < lo) return lo;
	else if (v > hi) return hi;
	else return v;
}

template<class T> typename T::size_type
container_clamp(const T &c, typename T::size_type at)
{
	// or possibly throw exception if out of range
	return clamp(typename T::size_type(0), at, c.size());
}

struct Rgba_color {
	unsigned char r, g, b, a;
	Rgba_color() : r(0), g(0), b(0), a(0) {}
	Rgba_color(unsigned char R, unsigned char G,
			unsigned char B, unsigned char A = 0xff) :
		r(R), g(G), b(B), a(A)
	{}
	Rgba_color(unsigned long rgba) :
		r(0xff & (rgba >> 24)), g(0xff & (rgba >> 16)), b(0xff & (rgba >> 8)),
			a(0xff & rgba)
	{}
	bool operator==(const Rgba_color &o) const {
		return r == o.r && g == o.g && b == o.b;
	}
};

inline Rgba_color
Rgb_color(unsigned long rgb)
{
	return Rgba_color(rgb << 8);
}

inline Rgba_color
Rgba_color_f(double r, double g, double b, double a = 1.0)
{
	return Rgba_color(int(r*0xff), int(g*0xff), int(b*0xff), int(a*0xff));
}

namespace util {

// more usual kind of bi-list than the weird STL one
// contents are immutable.  not yet specialized for pointers
template<class T>
class List {
public:
	List(T &head, List<T> *tail) :
		_head(head), _tail(tail), _prev(0)
	{
		if (_tail)
			_tail->_prev = this;
	}
	void append(List<T> *o) {
		_tail = o;
		o->_prev = this;
	}
	const T &head() const { return _head; }
	const List<T> *tail() const { return _tail; }
	const List<T> *prev() const { return _prev; }
private:
	T _head;
	List<T> *_tail;
	List<T> *_prev;
};

}

