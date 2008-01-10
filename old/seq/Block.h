#include <assert.h>

#include <FL/Fl.H>
#include <FL/Fl_Tile.H>

/*
	block
	/	\
title		body ___
		/ |		\
scroll_box time_sb	tile _____
				/		\
		ruler_group		track_group
	/		\			|	\
ruler		ruler_box		track_sb	track_zoom
							/
					track_tile
					/
				track, ...
				transparent_ruler
				/
			event, ...

also, there should be readouts for insertion point pos, zoom box,
block length, ...
these should be in both Trackpos units and relative to Mark units
(controllable from python)

bugs:
scrollbars don't resize properly when the Block is resized, in fact, no one
seems to realize the Block has changed sizes

*/

namespace widgets {

/*
class Track_zoom : public Fl_Box {
public:
	Track_zoom(int X, int Y, int W, int H, Track_tile &t) :
		Fl_Box(X, Y, W, H)
	{ box(FL_FLAT_BOX); color(FL_GREEN); }
	void resize(int X, int Y, int W, int H) {
		Fl_Box::resize(X, Y, W, H);
		print_widget(parent()->parent()->child(0));
		print_widget(parent());
	}
		
	virtual void zoom_speed(Dpoint zs) {}
	Drect zoom_win() const { return Drect(0, 0, 0, 0); }
	void set_lower_right(Dpoint p) { lower_right = p; }
	Dpoint lower_right;
}; */

class Track_zoom : public Zoom {
public:
	Track_zoom(int X, int Y, int W, int H) :
		Zoom(X, Y, W, H),
		_zoom_speed(0, 0)
	{}
	int handle(int ev) {
		if (ev == FL_ENTER) // grab focus on mouseover
			Fl::focus(this);
		else if (ev == FL_LEAVE)
			; // Fl::unfocus(this); // no, I want lazy focus-follows mouse
		return Zoom::handle(ev);
	}
	void zoom_speed(Dpoint zs) { _zoom_speed = zs; }
protected:
	bool is_zoom_key(int key);
private:
	seq::Zoom_info *zoom_info;
	Dpoint _zoom_speed;
};

class Block : public Fl_Group {
public:
	Block(int X, int Y, int W, int H, seq::Orientation o);
	void resize(int X, int Y, int W, int H);
	int handle(int ev);
	void update_sizes(int title_size, int ruler_size, int sb, seq::Orientation o);
	void update_zoom_speed(double time, double track);
	void absolute_zoom(const Trect &zw);
	void relative_zoom(Dpoint z, Dpoint center);
	void update_sb();
	void update_title(const char *s);
	void update_colors(const seq::Block_state::Colors *c);
	void insert_ruler_marklist(int i, const seq::Marklist *m) {
		ruler.insert_marklist(i, m);
	}
	void remove_ruler_marklist(int i) { ruler.remove_marklist(i); }
	void insert_sb_marklist(int i, const seq::Marklist *m) {
		// time_sb.insert_marklist(i, m);
	}
	void remove_sb_marklist(int i) { /* time_sb.remove_marklist(i); */ }
	Track_tile *tracks() { return &track_tile; }
	// Track_zoom *zoom() { return &track_zoom; }
	/* Tpoint track_tile_dimensions() const {
		return track_tile.dimensions();
	} */
	Trect zoom_win() const { return zoom_info.win(); }
private:
	// very large number for scrollbar units
	// scrollvalue doesn't take doubles and Trackpos is too big
	// sb.value() is in offset / track_length * Sb_scale
	enum { Sb_scale = 1<<30 - 1 };
	seq::Zoom_info zoom_info;
	Orientation orientation;

	A_input title;
	Fl_Group body;
		Fl_Box scroll_box;
		A_scrollbar time_sb;
		Fl_Tile tile;
			Fl_Group ruler_group;
				Fl_Box ruler_box;
				Ruler ruler;
			Fl_Group track_group;
				A_scrollbar track_sb;
				Track_zoom track_zoom;
					Track_tile track_tile;

	void update_ruler();
	void update_track_zoom();
	static void sb_cb(Fl_Widget *w, void *vp);
	static void track_zoom_cb(Fl_Widget *w, void *vp);
	static void track_tile_resize_cb(Fl_Widget *w, void *vp);
	static void track_tile_selection_cb(Fl_Widget *w, void *vp, const Trange &r);
};

}
