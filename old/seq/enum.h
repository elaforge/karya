// sequencer definitions

struct Rgba_color {
	unsigned char r, g, b, a;
	Rgba_color(unsigned char R, unsigned char G,
			unsigned char B, unsigned char A = 0) :
		r(R), g(G), b(B), a(A)
	{}
	Rgba_color(double R, double G, double B, double A = 0.0) :
		r(int(R*0xff)), g(int(G*0xff)), b(int(b*0xff)), a(int(a*0xff))
	{}
	Rgba_color(unsigned long rgba) :
		r(0xff & (rgba >> 24)), g(0xff & (rgba >> 16)), b(0xff & (rgba >> 8)),
			a(0xff & rgba)
	{}
};

inline Rgba_color Rgb_color(unsigned long rgb)
{
	return Rgba_color(rgb << 8);
}


class Trackpos {
	// immutable point in time
public:
	const Trackpos &operator<(const Trackpos &o) const;
	const Trackpos &operator>(const Trackpos &o) const;
	const Trackpos &operator==(const Trackpos &o) const;
	// +, -, =, ...
private:
	double pos; // maybe later make this into a true fixed-point
};

class Marklist;
class Mark {
public:
	Rgba_color color; // color of mark on ruler, alpha is used for mark on tracks
	char *name; // possibly drawn on ruler, and used as an identifier
	unsigned char width; // width of mark on screen
	unsigned char rank; // used as an identifier along with name
	bool show_name; // whether to draw the name on the ruler

	Trackpos extent; // mark extent
	// possibly fetch a marklist to start drawing at +pos until the next Mark
	// (pos+extent)
	// the first submark is never drawn since it overlaps with this mark, but
	// its submarklist() will be called to check for subsubmarks
	// zoom_factor is the ratio between Trackpos units and screen pixels,
	// so submarklist() can determine if it wants to generate submarks
	typedef const Marklist &Submarklist_f(double zoom_factor);
	Submarklist_f &submarklist;
};

// each ruler track has a name and a marklist.
// they get drawn left to right / top to bottom.
// higher marklists draw over lower ones
class Marklist {
	typedef const_iterator ...;
	class const_iterator {
		const Mark &mark;
		Trackpos tp;
	};
	const_iterator find_after(const Trackpos &tp);
};
/*
4/4 bar:
f zf = repeat 44_bar
44_bar = repeat [darkblue, "bar", 4, 1, false, 4,
submarklist zf = repeat [medblue, "quarter", 3, 2, false, 1,
	submarklist zf = repeat [lightblue, "eighth", 2, 3, false, .5,
		submarklist zf = repeat [lightbrown, "sixteenth", 1, 4, false, .25,
			submarklist zf = repeat [lightbrown, "thirtysecond", 1, 5, false, .125,
				...
*/

enum {
	Paleyellow	= 0xFFFFAA,
	Darkyellow	= 0xEEEE9E,
	Darkgreen	= 0x448844,
	Palegreen		= 0xAAFFAA,
	Medgreen		= 0x88CC88,
	Darkblue		= 0x000055,
	Palebluegreen	= 0xAAFFFF,
	Paleblue		= 0x0000BB,
	Bluegreen		= 0x008888,
	Greygreen	= 0x55AAAA,
	Palegreygreen	= 0x9EEEEE,
	Yellowgreen	= 0x99994C,
	Medblue		= 0x000099,
	Greyblue		= 0x005DBB,
	Palegreyblue	= 0x4993DD,
	Purpleblue	= 0x8888CC
};
