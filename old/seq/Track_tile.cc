#include "util.h"
#include "s_util.h"
#include "Marklist.h"
#include "state.h"

#include "f_util.h"
#include "Ruler.h"
#include "Track.h"
#include "Zoom.h"
#include "Tile_ext.h"

#include "Track_tile.h"

#include <algorithm> // for swap
#include <vector>
#include <iostream>
#include <stdlib.h>

namespace widgets {

#define DPRINT(X) if (Track_tile::Debug) std::cerr << X
bool Track_tile::Debug = false;

Track_tile::Track_tile(int X, int Y, int W, int H, Orientation o, int move_button) :
	Tile_ext(X, Y, W, H),
	o(o), _move_button(move_button),
	_resize_cb(0), _selection_cb(0),
	vpad(X, Y, W, H, "vpad")
{
	end();
	vpad.box(FL_FLAT_BOX);
	resizable(this);
	Tile_ext::callback(tile_cb, this);
}

void
Track_tile::invariant() const
{
	// children start at offset.y
	// n tracks + n children + vpad
	Assert(children() % 2 == 1);
	for (int i = 0, ty = o.y(this) - pixel_offset.y;
			i < children()-1;
			i += 2, ty += o.h(child(i)))
	{
		// children are vertically ordered
		Assert(ty <= o.y(child(i)));
		// even children are tracks, odd are hpads
		Assert(dynamic_cast<Track *>(child(i)));
		Assert(dynamic_cast<Fl_Box *>(child(i+1)));
		// tracks and hpads are aligned and complementary
		Assert(o.y(child(i)) == o.y(child(i+1)) && o.b(child(i)) == o.b(child(i+1)));
		Assert(o.r(child(i)) == o.x(child(i+1)));
	}
	// vpad is always last
	Assert(child(children()-1) == &vpad);
}

void
Track_tile::resize(int X, int Y, int W, int H)
{
	invariant();
	move_children(o.point(X - x(), Y - y()));
	Fl_Widget::resize(X, Y, W, H);
	fix_sizes();
	invariant();
}

int
Track_tile::handle(int ev)
{
	static int btn, last_track;
	if (Tile_ext::handle(ev)) // give tile code a chance to deal with it
		return 1;
	if (ev == FL_PUSH)
		btn = Fl::event_button()-1; // fltk numbers buttons from 1
	// pass through _move_button, all others turn into select(n, ...) msgs to all
	// tracks in yrange
	if ((ev == FL_PUSH || ev == FL_RELEASE || ev == FL_DRAG)
		&& btn != _move_button)
	{
		while (int(selections.size()) <= btn)
			selections.push_back(sel_info());
		Point m = mouse_pos();
		int strack = track_from_screen(o.y(m));
		Trackpos p = Trackpos::from_screen(o.x(m) - o.x(this), cur_zoom.x)
			+ offset.x;
		sel_info &sel = selections[btn];
		int lowt = min(strack, sel.strack), hight = max(strack, sel.strack);
		Trange srange;
		switch (ev) {
		case FL_PUSH:
			selections[btn] = sel_info(p, strack);
			srange = Trange(p);
			for (int i = 0; i < tracks(); i++) {
				if (i == strack)
					track(i)->select_range(btn, srange);
				else
					track(i)->clear_selection(btn);
			}
			break;
		case FL_RELEASE:
			for (int i = lowt; i < hight; i++)
				track(i)->select_release(btn);
			break;
		case FL_DRAG:
			srange = Trange(sel.start, p-sel.start);
			for (int i = lowt; i <= hight; i++)
				track(i)->select_range(btn, srange);
			if (last_track < lowt || hight < last_track)
				track(last_track)->clear_selection(btn);
			break;
		}
		if ((ev == FL_PUSH || ev == FL_DRAG) && btn == 0)
			do_selection_callback(srange);
		last_track = strack;
		return 1;
	}
	return 0;
}

int
Track_tile::track_screen_size(const Track *t) const
{
	int xp = t->dimensions().x.to_screen(cur_zoom.x);
	// w()+2 so right edge of frame is not visible
	return clamp(0, xp - pixel_offset.x, w()+2);
}

void
Track_tile::zoom(const Point_tmpl<seq::Zoom_t> &z)
{
	if (z == old_zoom)
		return;
	if (z == cur_zoom)
		damage(FL_DAMAGE_ALL);
	else
		damage(FL_DAMAGE_ALL | DAMAGE_ZOOM);
	cur_zoom = z;
	fix_sizes();
}

void
Track_tile::scroll(Tpoint o)
{
	offset = o;
	Point p(offset.x.to_screen(cur_zoom.x), offset.y.to_screen(cur_zoom.y));
	Point d(pixel_offset.x - p.x, pixel_offset.y - p.y);
	if (!d.x && !d.y)
		return;
	pixel_offset = p;
	DPRINT("scroll: " << offset << " (" << pixel_offset << ")\n");
	if (d.x) {
		damage(FL_DAMAGE_ALL);
		fix_sizes();
	} else { // optimize if we only scrolled in y
		damage(FL_DAMAGE_SCROLL);
		move_children(d);
		fix_vpad();
	}
}

void
Track_tile::fix_sizes()
{
	// w according to offset and zoom, x is always 0
	// h is t->height * zoom, y is track(i-1).b()
	DPRINT("fix_sizes:\n");
	int tx = o.x(this);
	int ty = o.y(this) - pixel_offset.y;
	for (int i = 0; i < tracks(); i++) {
		Track *t = track(i);
		int th = t->dimensions().y.to_screen(cur_zoom.y);
		int tb = ty + th;
		if (tb < o.y(this))
			; // above window
		else if (ty <= o.b(this)) {
			int tw = track_screen_size(t);
			o.resize(t, tx, ty, tw, th);
			o.resize(hpad(i), o.r(t), ty, max(0, w()-tw), th);
			t->scroll(offset.x);
			t->zoom(cur_zoom.x);
		} else
			break;
		ty = tb;
		DPRINT('\t' << i << ": " << rect(t) << " | " << rect(hpad(i)) << '\n');
	}
	fix_vpad();
	DPRINT("\td: " << show_widget(&vpad) << '\n');
	init_sizes();
}

void
Track_tile::fix_vpad()
{
	// dummy fills up space from last track to the bottom of the
	// window, if there is any
	int tb = tracks() == 0 ? o.y(this) : o.b(track(tracks()-1));
	o.resize(vpad, o.x(this), tb, o.w(this), max(0, o.b(this) - tb));
}

void
Track_tile::move_children(Point d)
{
	for (int i = 0; i < children(); i++) {
		Fl_Widget *c = child(i);
		o.position(c, o.x(c) + d.x, o.y(c) + d.y);
	}
}

/* vscrolling is normal.  hscrolling is all a fake.  It actually *resizes* tracks
if their ends have scrolled into the window.  Actual track scrolling and
zooming is done by the tracks.
b widgets::Track_tile::draw
*/
void
Track_tile::draw()
{
	Clip_area _clip(rect(this));
	if (damage() & FL_DAMAGE_ALL) {
		Tile_ext::draw();
		return;
	}
	if (damage() & FL_DAMAGE_SCROLL) {
		int dy = pixel_offset.y - old_pixel_offset.y;
		if (dy) {
			Point p = o.point(0, dy);
			fl_scroll(x(), y(), w(), h(), p.x, p.y, draw_area_cb, this);
		}
	}
	if (damage() & FL_DAMAGE_CHILD) {
		// update children because only a child is damaged
		draw_children();
	}
	old_pixel_offset = pixel_offset;
}

void
Track_tile::draw_area_cb(void *vp, int x, int y, int w, int h)
{
	Track_tile *self = static_cast<Track_tile *>(vp);
	Clip_area _clip(Rect(x, y, w, h));
	self->draw_children();
}

Track *
Track_tile::insert_track(int at, Tpoint tdim)
{
	Assert(0 <= at && at <= tracks());
	Tpoint nd = dimensions();
	nd.x = max(nd.x, tdim.x);
	nd.y += tdim.y;
	dimensions(nd);
	// insert at proper place, leave sizes to fix_sizes()
	int ty = at == 0 ? o.y(this) : o.b(track(at-1));
	Track *t = new Track(0, 0, 1, 1, o, tdim);
	o.resize(t, o.x(this), ty, 1, 1);
	insert(*t, at*2);

	Fl_Box *hpad = new Fl_Box(0, 0, 1, 1, "h");
	o.resize(hpad, o.r(t), ty, 1, 1);
	hpad->color(_bg_color);
	hpad->box(FL_FLAT_BOX);
	insert(*hpad, at*2+1);
	fix_sizes();
	invariant();
	return t;
}

void
Track_tile::remove_track(int at)
{
	Assert(0 <= at && at <= tracks());
	Track *t = track(at);
	Fl_Box *h = hpad(at);
	// move up other tracks
	for (int i = at; i < tracks(); i++) {
		o.position(track(i), o.x(this), o.y(track(i) - o.h(t)));
		o.position(hpad(i), o.x(this), o.y(hpad(i) - o.h(t)));
	}
	remove(t);
	remove(h);
	delete t;
	delete h;
	Tpoint nd = dimensions();
	for (int i = 0; i < tracks(); i++)
		nd.x = max(nd.x, track(i)->dimensions().x);
	nd.y -= t->dimensions().y;
	dimensions(nd);
	fix_sizes();
	invariant();
}

/* track has been dragged
set track dimensions() based on current widget sizes.
xdim is only modified if the current xdim is not what it "should" be according to
offset and zoom calculation.  Padding widgets are adjusted appropriately.
*/
void
Track_tile::tile_cb(Fl_Widget *w, void *vp)
{
	Track_tile *self = static_cast<Track_tile *>(vp);
	self->update_dimensions();
	self->do_resize_callback();
}

void
Track_tile::update_dimensions()
{
	Tpoint m;
	for (int i = 0; i < tracks(); i++) {
		Track *t = track(i);
		if (o.b(t) < o.y(this))
			continue;
		if (o.b(this) < o.y(t))
			break;
		Tpoint d = t->dimensions();
		if (o.w(t) != track_screen_size(t))
			d.x = Trackpos::from_screen((pixel_offset.x + o.w(t)), cur_zoom.x);
		d.y = Trackpos::from_screen(o.h(t), cur_zoom.y);
		t->dimensions(d);
		m.x = max(m.x, d.x);
		m.y += d.y;
		DPRINT("track dimensions: " << i << ": " << d << '\n');
	}
	fix_vpad();
	DPRINT("tile dimensions: " << dimensions() << " -> " << m << '\n');
	dimensions(m);
}

}
