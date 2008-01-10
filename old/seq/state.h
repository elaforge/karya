#include <FL/Fl_Widget.H>

/* this is different from s_util.h in that it also depends on fltk */

namespace seq {

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

class Cmd_history {
	// undo history
};

/* States can be owned by multiple views, and must be deleted when no longer used.
reference counting:
- Each state must have a refcount and must be deleted to lower refcount.
- Not compacting (no big deal).
+ States don't move so only a singly indirect pointer is needed for python wrapper objects.
Cycles are impossible because states are not closed.

stop and copy:
- Complicated: requires overloaded new operator, must keep track of root objects, etc.
+ Compacting.
+ Possibly faster.
*/
struct Event_state {
	const struct Colors {
		Fl_Color bg, text;
	} *colors;
	Trackpos start, extent;
protected:
	unsigned ref_count;
};

struct Track_state {
	std::vector<Event_state *> events;
	Trackpos length;
	const struct Colors {
		Fl_Color bg;
		std::vector<Fl_Color> selections;
	} *colors;
protected:
	unsigned ref_count;
};

struct Block_state {
	const char *title;
	std::vector<Marklist *> marklists;
	Cmd_history edit_history;
	const struct Colors {
		Fl_Color scroll_box, ruler_box, ruler, track_tile, selection;
	} *colors;
protected:
	unsigned ref_count;
};

class Zoom_info {
public:
	// all coords are de-oriented (x = time, y = track)
	// view window offset in time and track
	Tpoint offset() const { return Tpoint(_win.x, _win.y); }
	void offset(Tpoint o) { _win.x = o.x; _win.y = o.y; }
	// opaque zoom type
	Zpoint zoom() const { return _zoom; }

	void win(Trect zw, const Point winsz) {
		_win = zw;
		_win.clamp(_zoom_area);
		_zoom = Zpoint(Zoom_t(zw.w, winsz.x), Zoom_t(zw.h, winsz.y));
	}
	Trect win() const { return _win; }
	// wait for subsequent zooms to clamp
	void zoom_area(const Tpoint &t) {
		_zoom_area =Trect(Trackpos(), Trackpos(), t.x, t.y);
	}

	void center_zoom(Dpoint z, Dpoint center);
	// flip around for widgets that don't know about orientation
	Zoom_info reorient(Orientation o) const;
private:
	Trect _win; // zoom window
	Zpoint _zoom; // trackpos per pixel
	Trect _zoom_area; // 'interesting' area we are allowed to zoom in on
};

}

namespace widgets {
// Make access to popular sequencer types from s_util.h easier for users of seq in
// widgets.
using seq::Trackpos;
using seq::Trange;
using seq::Tpoint;
using seq::Trect;
using seq::Orientation;
using seq::Zoom_info;
}


