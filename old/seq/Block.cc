#include <Fl/Fl.H>
#include <FL/Fl_Group.H>
#include <FL/Fl_Scroll.H>
#include <FL/Fl_Tile.h>
#include <FL/Fl_Box.h>

// sequencer stuff
#include "util.h"
#include "s_util.h"
#include "Marklist.h"
#include "state.h"

// widgets
#include "f_util.h"
#include "A_input.h"
#include "A_scrollbar.h"
#include "Ruler.h"
#include "Event.h"
#include "Track.h"
#include "Zoom.h"
#include "Tile_ext.h"
#include "Track_tile.h"

#include "Block.h"

namespace widgets {

#define DPRINT(X) if (Block_debug) std::cout << X
namespace { const bool Block_debug = true; }

bool
Track_zoom::is_zoom_key(int key)
{
	if (key == FL_Shift_L || key == FL_Shift_R) {
		Zoom::zoom_speed(Dpoint(_zoom_speed.x, 0));
		return true;
	} else if (key == FL_Control_L || key == FL_Control_R) {
		Zoom::zoom_speed(Dpoint(0, _zoom_speed.y));
		return true;
	} else
		return false;
}

////

Block::Block(int X, int Y, int W, int H, seq::Orientation o) :
	Fl_Group(X, Y, W, H),
	orientation(o),
	title(0, 0, 1, 1),
	body(0, 0, 1, 1),
		scroll_box(0, 0, 1, 1, "s"),
		time_sb(0, 0, 1, 1),
		tile(0, 0, 1, 1),
			ruler_group(0, 0, 1, 1),
				ruler_box(0, 0, 1, 1, "r"),
				ruler(0, 0, 1, 1, o),
			track_group(0, 0, 1, 1),
				track_sb(0, 0, 1, 1),
				track_zoom(0, 0, 1, 1),
					track_tile(0, 0, 1, 1, o, 2)
{
	// the sizes of 1 are so that groups realize that their children
 	// are inside of them. this
	 // satisfies invariants about children being inside their parents (even
 	// though we'll be resizing everything anyway)
	current(0); // done adding widgets
	tile.add(track_group);
	//ruler_group.remove(ruler);
	scroll_box.box(FL_THIN_DOWN_BOX);
	ruler_box.box(FL_THIN_DOWN_BOX);
	resizable(body);
	body.resizable(tile);
	ruler_group.resizable(ruler);
	track_group.resizable(track_zoom);

	time_sb.callback(sb_cb, this);
	track_sb.callback(sb_cb, this);
	track_zoom.callback(track_zoom_cb, this);
	track_tile.resize_callback(track_tile_resize_cb, this);
	track_tile.selection_callback(track_tile_selection_cb, this);
}

int
Block::handle(int ev)
{
	// DPRINT(show_event(ev) << '\n');
	if (ev == FL_MOVE) {
		if (Fl::event_inside(&ruler) || Fl::event_inside(&track_zoom))
			ruler.mouse_pos(mouse_pos());
	}
	int r = Fl_Group::handle(ev);
	if (ev == FL_ENTER)
		return 1; // make sure we always get FL_MOVEs
	else
		return r;
}

void
Block::resize(int X, int Y, int W, int H)
{
	DPRINT("block resize: " << Rect(X, Y, W, H) << '\n');
	int sb = scroll_box.w();
	Fl_Group::resize(X, Y, W, H);
	// scroll_box wants to resize when the block resizes, so fix it up
	Rect p = rect(body);
	const seq::Orientation o = orientation;
	scroll_box.resize(p.x, p.b() - sb, sb, sb);
	o.resize(time_sb, o.r(scroll_box), o.y(time_sb), o.w(body) - sb, sb);

	print_children(this);
}

void
Block::update_sizes(int title_size, int ruler_size, int sb, const seq::Orientation o)
{
	// if we resize from parent to child, then children get a lot of spurious
	// resizes as their parents move them around.  on the other hand, if
	// we go the other way, parents mess up their children.
	// spurious resizes it is.
	orientation = o;
	title.resize(x(), y(), w(), title_size);
	body.resize(x(), y() + title_size, w(), h() - title_size);

	Rect p = rect(body);
	scroll_box.resize(p.x, p.b() - sb, sb, sb);
	if (o == seq::Horizontal_time) {
		time_sb.resize(sb, p.b() - sb, p.w - sb, sb);
		time_sb.type(FL_HORIZONTAL);
		track_sb.type(FL_VERTICAL);
		tile.resize(p.x, p.y, p.w, p.h - sb);
	} else {
		time_sb.resize(p.x, p.y, sb, p.h - sb);
		time_sb.type(FL_VERTICAL);
		track_sb.type(FL_HORIZONTAL);
		tile.resize(p.x + sb, p.y, p.w - sb, p.h);
	}
	o.resize(ruler_group, o.x(tile), o.y(tile), o.w(tile), ruler_size);
	p = rect(ruler_group);
	if (o == seq::Horizontal_time) {
		ruler_box.resize(p.x, p.y, sb, ruler_size);
		ruler.resize(p.x + sb, p.y, p.w - sb, ruler_size);
	} else {
		ruler_box.resize(p.x, p.b() - sb, ruler_size, sb);
		ruler.resize(p.x, p.y, ruler_size, p.h - sb);
	}
	ruler.orientation(o);

	o.resize(track_group, o.x(tile), o.y(tile) + ruler_size,
		o.w(tile), o.h(tile) - ruler_size);
	p = rect(track_group);
	if (o == seq::Horizontal_time) {
		track_sb.resize(p.x, p.y, sb, p.h);
		track_zoom.resize(p.x + sb, p.y, p.w - sb, p.h);
	} else {
		track_sb.resize(p.x, p.b() - sb, p.w, sb);
		track_zoom.resize(p.x, p.y, p.w, p.h - sb);
	}
	track_tile.orientation(o);
	track_tile.resize(track_zoom.x(), track_zoom.y(),
		track_zoom.w(), track_zoom.h());
	
	init_sizes();
	body.init_sizes();
	tile.init_sizes();
	ruler_group.init_sizes();
	track_group.init_sizes();
	track_tile.init_sizes();
	// print_children(this);
}

void
Block::update_zoom_speed(double time, double track)
{
	track_zoom.zoom_speed(orientation.point(time, track));
}

// 'absolute' means the zoom window is absolute to the current window size,
// rather than being relative to the last zoom window (i.e. still using the last
// window size).  At least one update_zoom must be absolute or zoom will stay 0!
void
Block::absolute_zoom(const Trect &zw)
{
	zoom_info.win(zw, orientation.point(track_tile.w(), track_tile.h()));
	update_ruler();
	update_track_zoom();
	update_sb();
}

void
Block::relative_zoom(Dpoint z, Dpoint center)
{
	zoom_info.center_zoom(z, center);
	update_ruler();
	update_track_zoom();
	update_sb();
}

void
Block::update_sb()
{
	Trect zw = zoom_info.win();
	Tpoint s = track_tile.dimensions();

	// normalize zoom window to 0..1
	Drect d(min(1.0, zw.x.ratio(s.x)), min(1.0, zw.y.ratio(s.y)),
		min(1.0, zw.w.ratio(s.x)), min(1.0, zw.h.ratio(s.y)));
	// int Fl_Slider::scrollvalue(int p, int W, int t, int l)
	//  p = position, first line displayed
	//  w = window, number of lines displayed
	//  t = top, number of first line
	//  l = length, total number of lines
	time_sb.scrollvalue(int(d.x * Sb_scale), int(d.w * Sb_scale),
		0, Sb_scale);
	track_sb.scrollvalue(int(d.y * Sb_scale), int(d.h * Sb_scale),
		0, Sb_scale);
}

void
Block::update_title(const char *s)
{
	title.value(s);
}

void
Block::update_colors(const seq::Block_state::Colors *c)
{
	scroll_box.color(c->scroll_box);
	ruler_box.color(c->ruler_box);
	ruler.color(c->ruler);
	ruler.selection_color(c->selection);
	// time_sb.selection_color(c->selection);
	track_tile.bg_color(c->track_tile);
}

/// private

void
Block::update_ruler()
{
	Tpoint o = zoom_info.offset();
	ruler.zoom(zoom_info.zoom().x);
	ruler.scroll(o.x);
	track_tile.zoom(zoom_info.zoom());
	track_tile.scroll(o);
}

void
Block::update_track_zoom()
{
	Zoom_info z = zoom_info.reorient(orientation);
	track_zoom.zoom_win(z);
}

void
Block::sb_cb(Fl_Widget *w, void *vp)
{
	Block *self = static_cast<Block *>(vp);

	// slide zoom window to sb positions
	Tpoint sz = self->track_tile.dimensions();
	Tpoint o(sz.x.scale(double(self->time_sb.value()) / Sb_scale),
		sz.y.scale(double(self->track_sb.value()) / Sb_scale));
	self->zoom_info.offset(o);
	// tell everyone but sb
	self->update_ruler();
	self->update_track_zoom();
}

void
Block::track_zoom_cb(Fl_Widget *w, void *vp)
{
	Block *self = static_cast<Block *>(vp);
	Zoom_info z = self->track_zoom.zoom_win().reorient(self->orientation);
	DPRINT("block zoom: " << z.win() << '\n');
	self->zoom_info = z;
	// tell everyone but track_zoom
	self->update_ruler();
	self->update_sb();
}

void
Block::track_tile_resize_cb(Fl_Widget *w, void *vp)
{
	Block *self = static_cast<Block *>(vp);
	self->zoom_info.zoom_area(self->track_tile.dimensions());
	// DPRINT("zoom_area: " << self->track_tile.dimensions() << '\n');
	self->update_track_zoom();
}

void
Block::track_tile_selection_cb(Fl_Widget *w, void *vp, const Trange &r)
{
	Block *self = static_cast<Block *>(vp);
	self->ruler.selection(r);
	// self->time_sb.selection(r);
}

} // namespace widget
