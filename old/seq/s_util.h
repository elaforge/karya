// sequencer definitions
#include <vector>
#include <assert.h>

// for Fl_Color
#include <Fl/Enumerations.H>

namespace seq {
/*
int: // ok size
16 bits for each second is sample accurate up to 2**16 samples/sec, 65Khz
15bits for seconds is 546 minutes or 9 hours.
long long: // way huge
32 bits/sec
31 bits for seconds is about 73 years

the problem with doubles is that they have no exact decimal
representation, so I worry about cumulative error and not being
able to save them to files 
double:
11bit exponent, 52bit fraction
*/

class Zoom_t;
class Trackpos {
	typedef long long vtype;
	static const int second_bits = 32;
	static const double per_second = 1LL<<second_bits;
public:
	Trackpos() : v(0) {}
	static Trackpos from_sec(int s) {
		return Trackpos(vtype(s) << second_bits);
	}
	double to_sec() const;
	int to_screen(const Zoom_t &zoom) const;
	static Trackpos from_screen(int p, const Zoom_t &zoom);
	// the alternative is to define operators / conversions so that
	// Trackpos can mix with doubles, and I don't want to do that
	Trackpos scale(double d) const { return Trackpos(vtype(d * v)); }
	double ratio(Trackpos p) const { return double(v) / double(p.v); }

	Trackpos operator-() const { return Trackpos(-v); }
#define OP(X) \
Trackpos operator X(const Trackpos &o) const { \
	Trackpos r(v X o.v); return r; \
}
	OP(+) OP(-) OP(*) OP(/)
#undef OP
#define OP(X) bool operator X(const Trackpos &o) const { return v X o.v; }
	OP(==) OP(!=)
	OP(<) OP(>) OP(<=) OP(>=)
#undef OP
#define OP(X) Trackpos &operator X(const Trackpos &o) { v X o.v; return *this; }
	OP(=) OP(+=) OP(-=) OP(/=) OP(*=)
#undef OP
private:
	explicit Trackpos(vtype v) : v(v) {}
	friend std::ostream &operator<<(std::ostream &os, const Trackpos &p);
	friend class Zoom_t; // just so Zoom_t can make Trackpos directly
	vtype v;
};

inline std::ostream &
operator<<(std::ostream &os, const Trackpos &p)
{
	return os << (p.v / p.per_second) << "sec";
}

typedef Range_tmpl<Trackpos> Trange;
typedef Point_tmpl<Trackpos> Tpoint;
typedef Rect_tmpl<Trackpos> Trect;

/* class Zoom_t {
	typedef double vtype;
public:
	Zoom_t(Trackpos t, int winsz) :
		trackpos_per_pixel(t / Trackpos(winsz))
	{
		assert(trackpos_per_pixel > Trackpos(0));
	}
	Zoom_t() : trackpos_per_pixel(Trackpos(1)) {}
	bool operator<(const Zoom_t &o) const {
		return trackpos_per_pixel < o.trackpos_per_pixel;
	}
	bool operator==(const Zoom_t &o) const {
		return trackpos_per_pixel == o.trackpos_per_pixel;
	}
	bool operator!=(const Zoom_t &o) const { return !(*this == o); }
	Zoom_t scale(double d) const {
		// since zoom is in trackpos_per_pixel, a zoom of 2 means there
		// are 1/2 as many trackpos per pixel
		// (inverse of 2*pixel_per_trackpos)
		return Zoom_t(trackpos_per_pixel.scale(1/d));
	}
	double pixel_per_sec() const { return 1/trackpos_per_pixel.to_sec(); }
private:
	friend std::ostream &operator<<(std::ostream &os, const Zoom_t &p);
	friend int Trackpos::to_screen(const Zoom_t &zoom) const;
	friend Trackpos Trackpos::from_screen(int p, const Zoom_t &zoom);
	Zoom_t(Trackpos t) : trackpos_per_pixel(t) {}
	vtype trackpos_per_pixel;
}; */

class Zoom_t {
	typedef double vtype;
public:
	Zoom_t(Trackpos t, int winsz) :
		trackpos_per_pixel(t.ratio(Trackpos(winsz)))
	{
		// assert(trackpos_per_pixel > Trackpos(0));
	}
	Zoom_t() : trackpos_per_pixel(1.0) {}
	bool operator<(const Zoom_t &o) const {
		return trackpos_per_pixel < o.trackpos_per_pixel;
	}
	bool operator==(const Zoom_t &o) const {
		return trackpos_per_pixel == o.trackpos_per_pixel;
	}
	bool operator!=(const Zoom_t &o) const { return !(*this == o); }
	Zoom_t scale(double d) const {
		// since zoom is in trackpos_per_pixel, a zoom of 2 means there
		// are 1/2 as many trackpos per pixel
		// (inverse of 2*pixel_per_trackpos)
		return Zoom_t(trackpos_per_pixel * d);
		// return Zoom_t(trackpos_per_pixel.scale(1/d));
	}
	double pixel_per_sec() const {
		return 1/
			(trackpos_per_pixel / Trackpos::per_second);
	}
private:
	friend std::ostream &operator<<(std::ostream &os, const Zoom_t &p);
	friend int Trackpos::to_screen(const Zoom_t &zoom) const;
	friend Trackpos Trackpos::from_screen(int p, const Zoom_t &zoom);
	Zoom_t(double t) : trackpos_per_pixel(t) {}
	double trackpos_per_pixel;
};
typedef Point_tmpl<Zoom_t> Zpoint;

inline std::ostream &
operator<<(std::ostream &os, const Zoom_t &p)
{
	return os << p.pixel_per_sec() << "p/sec";
	// return os << "Z(" << p.trackpos_per_pixel << "/pixel)";
}

struct Font_info {
	int face;
	int size;
	Fl_Color color;
};

}
