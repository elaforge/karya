rename
# DividerModel -> DividerModel
TrackModel -> EventTrackModel
RulerModel -> RulerTrackModel
TracklikeModel -> TrackModel

# DividerView -> DividerTrackView
TrackView -> EventTrackView
RulerView -> RulerTrackView
TracklikeView -> TrackView


#include <FL/Fl_Box.H>
#include <FL/Fl_Group.H>
#include <vector>

/*

Overlay_ruler : Fl_Group

Ruler draws lines on a plain background, has scroll() and zoom() methods
Overlay_ruler has children.  It also has scroll() and zoom() methods, which
do not affect the children.  It actually only needs one child, so it need not
be a Group.  

Track_tile
	|
Overlay_ruler
	|
Track

In this case, the overlay ruler would be limited in size by Track_tile.
On the other hand, having the ruler directly on the track means
ruler lines don't draw over track boundaries and different tracks can
have different ruler display settings (show different marks).  And not
having lines displayed past the track end is not a big problem.

But I begin to think track_tile width should be
max(longest track, window width).  Then you could increase track length
by dragging it.

This also means that Overlay_ruler works by changing size along with its
child, and does not have zoom() and scroll() methods.

I should write it as a general Overlay widget then.  But if the track_tile doesn't
resize smaller than the window, then how does the overlay change size?
They would both have to convert a resize smaller than the window into
a resize to the window and a scaling of the children to make them the size
they would have been had the object actually shrunk.  This seems silly

zooming:
objects have original rects and a zoom ratio which multiplies those rects.
you zoom them by sending zoom messages, not resizes, so the widget
never changes size.  or rather:

zoom msgs sent to Ruler and Track_tile.  Track_tile converts those into
resizes on its tracks.

this means that the ruler and each track will hit each marklist on every
redraw, including scrolling.  Is this ok performance-wise?  shint and ehint
should help here, since they'll likely be the same for each track.

*/
/* bugs:
mouse_pos doesn't keep sync with zooming

I haven't thought about whether using an int as pixel_offset will cause
problems if offset is really large. we should be zoomed out then, right?

Ruler marks are still off by a pixel sometimes due to roundoff I assume.
This leads to graphical glitches when a scrolled ruler is redrawn by the
mouse_pos or at the edge of the scroll.

*/

namespace widgets {

class Overlay_ruler : public Fl_Group {
public:
	Overlay_ruler(int X, int Y, int W, int H, Orientation o) :
		Fl_Group(X, Y, W, H), o(o),
		show_names(true)
	{}
	void resize(int X, int Y, int W, int H);
	void draw();
	void orientation(Orientation no) {
		o = no;
		damage(FL_DAMAGE_ALL);
	}
	void zoom(seq::Zoom_t z) {
		if (z != _zoom) {
			damage(DAMAGE_ZOOM);
			_zoom = z;
		}
	}
	void scroll(Trackpos p) {
		if (p != offset) {
			damage(FL_DAMAGE_SCROLL);
			offset = p;
		}
	}
	void insert_marklist(int i, const seq::Marklist *m) {
		i = int(container_clamp(_marklists, i));
		_marklists.insert(_marklists.begin()+i, seq::Marklist_view(m));
		damage(FL_DAMAGE_ALL);
	}
	void remove_marklist(int i) {
		(void) _marklists.at(i); // since there's no range-checked remove()
		_marklists.erase(_marklists.begin()+i);
		damage(FL_DAMAGE_ALL);
	}
protected:
	virtual void draw_area(Rect c);
	virtual Rect mark_rect(const seq::Mark *m, int xoffset, int i) const;
	void draw_marks(Rect d);
	void draw_mark(const seq::Mark *mark, int xoffset, int i);
	int to_x(Trackpos p) const {
		return (p - offset).to_screen(_zoom) + o.x(this);
	}
	int to_w(Trackpos p) const { return p.to_screen(_zoom); }
	seq::Orientation o;
	seq::Marklists _marklists;
	seq::Zoom_t _zoom;
	Trackpos offset;
	int pixel_offset;
	bool show_names;
private:
	static void draw_area_cb(void *vp, int x, int y, int w, int h);
};

class Selection_ruler : public Overlay_ruler {
public:
	Selection_ruler(int X, int Y, int W, int H, Orientation o, int cursor_width) :
		Overlay_ruler(X, Y, W, H, o),
		cursor_width(cursor_width)
	{}
	void selection(const Trange r) {
		_selection.selection(r);
		selection_damage(_selection);
	}
	void clear_selection() {
		_selection.selection(false);
		selection_damage(_selection);
	}
	void selection_color(Fl_Color c) {
		_selection.color = c;
		damage(FL_DAMAGE_ALL); // XXX just damage selection area
	}
protected:
	void draw_area(Rect c);
	void selection_damage(Selection<Trackpos> &sel);
	void draw_selection(const Selection<Trackpos> &sel);
	void draw_cursor(const Selection<Trackpos> &sel);
	Selection<Trackpos> _selection;
private:
	int cursor_width;
};

class Ruler : public Selection_ruler {
public:
	Ruler(int X, int Y, int W, int H, seq::Orientation o) :
		Selection_ruler(X, Y, W, H, o, 2),
		background(X, Y, W, H),
		_mouse_pos(-1)
	{
		end();
		background.box(FL_FLAT_BOX);
	}
	void color(Fl_Color c) { background.color(c); }
	// draw line based on position of mouse
	void mouse_pos(Point p);
protected:
	void draw_area(Rect c);
	Rect mark_rect(const seq::Mark *m, int xoffset, int i) const;
private:
	Rect mouse_pos_rect() const {
		return o.rect(_mouse_pos - pixel_offset, o.y(this), 2, o.h(this));
	}
	void draw_mouse_pos();
	Fl_Box background;
	int _mouse_pos;
};

}
