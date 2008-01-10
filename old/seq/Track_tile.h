#include <stdexcept>

namespace widgets {

/*
dimensions is the logical size of the tracks in Trackpos
callback is called when dimensions() changes
when tiles are dragged, tracks get dimensions() messages

event editing clicks and keys go directly to the event
button 1 and 2 send selection(n, ...) msgs to all tracks in yrange, and then release(n)
click3/drag3 goes directly to track (which will move events)


*/

class Track_tile : public Tile_ext {
	static bool Debug;
public:
	Track_tile(int X, int Y, int W, int H, Orientation o, int move_button);
	void draw();
	void resize(int X, int Y, int W, int H);
	Track *insert_track(int at, Tpoint tdim);
	void remove_track(int at);
	void orientation(Orientation orient) {
		o = orient;
		damage(FL_DAMAGE_ALL);
	}
	void scroll(Tpoint p);
	void zoom(const Point_tmpl<seq::Zoom_t> &z);
	void bg_color(Fl_Color c) {
		_bg_color = c;
		for (int i = 0; i < tracks(); i++)
			hpad(i)->color(c);
		vpad.color(c);
	}
	Tpoint dimensions() const { return _dimensions; }
	// even children are tracks, odd ones are hpads, last child is vpad
	int tracks() const { return (children()-1) / 2; }
	Track *track(int i) {
		if (i < 0 || i >= tracks())
			throw std::out_of_range("track()");
		return static_cast<Track *>(child(i*2));
	}
	void invariant() const;
	// a signal/slot system as in boost would be better, but I'm reluctant to introduce
	// a dependency for just this usage
	void resize_callback(Fl_Callback *cb, void *arg) {
		_resize_cb = cb;
		_resize_cb_arg = arg;
	}
	void selection_callback(void (*cb)(Fl_Widget *, void *, const Trange &),
		void *arg)
	{
		_selection_cb = cb;
		_selection_cb_arg = arg;
	}
	void do_resize_callback() {
		if (_resize_cb)
			_resize_cb(this, _resize_cb_arg);
	}
	void do_selection_callback(const Trange &r) {
		if (_selection_cb)
			_selection_cb(this, _selection_cb_arg, r);
	}
protected:
	int handle(int ev);
private:
	Fl_Box *hpad(int i) { return static_cast<Fl_Box *>(child(i*2+1)); }
	void dimensions(Tpoint d) {
		if (_dimensions != d) {
			_dimensions = d;
			do_resize_callback();
		}
	}
	int track_screen_size(const Track *t) const;
	void fix_sizes();
	void fix_vpad();
	void move_children(Point d);

	static void draw_area_cb(void *vp, int x, int y, int w, int h);
	static void tile_cb(Fl_Widget *w, void *vp);
	void update_dimensions();
	int track_from_screen(int p) {
		Point m = o.point(mouse_pos());
		for (int i = 0; i < tracks(); i++)
			if (m.y <= o.b(track(i)))
				return i;
		return tracks()-1;
	}

	struct sel_info {
		sel_info() {}
		sel_info(Trackpos p, int s) : start(p), strack(s) {}
		Trackpos start;
		int strack;
	};
	std::vector<sel_info> selections;
	Orientation o;
	Tpoint offset;
	Point pixel_offset, old_pixel_offset;
	Point_tmpl<seq::Zoom_t> cur_zoom, old_zoom;
	Tpoint _dimensions;
	Fl_Color _bg_color;
	int _move_button;
	Fl_Callback *_resize_cb;
	void (*_selection_cb)(Fl_Widget *, void *, const Trange &);
	void *_resize_cb_arg, *_selection_cb_arg;
	Fl_Box vpad; // box to take up space below bottom track
};

}
