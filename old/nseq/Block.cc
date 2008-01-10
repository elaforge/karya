#include "types.h"

#include "Block.h"
#include "util.h"
#include "futil.h"

Block::Block(int X, int Y, int W, int H, Orientation o) :
	Fl_Group(X, Y, W, H),
	o(o),
	// dummy sizes, but children must be inside parents to satisfy invariants
	// widgets will be resized properly by update_size
	title(0, 0, 1, 1),
	body(0, 0, 1, 1),
		scroll_box(0, 0, 1, 1, "s"),
		time_sb(0, 0, 1, 1),
		body_tile(0, 0, 1, 1),
			ruler_group(0, 0, 1, 1),
				ruler_box(0, 0, 1, 1, "r"),
				ruler(0, 0, 1, 1, o),
				// track_group wil wind up in ruler_group, fix this in
				// constructor
			track_group(0, 0, 1, 1),
				track_sb(0, 0, 1, 1),
				track_zoom(0, 0, 1, 1),
					track_tile(0, 0, 1, 1)
{
	current(0); // done adding widgets to this group
	body_tile.add(track_group);

	scroll_box.box(FL_THIN_DOWN_BOX);
	ruler_box.box(FL_THIN_DOWN_BOX);
	resizable(body);
	body.resizable(body_tile);
	ruler_group.resizable(ruler);
	track_group.resizable(track_zoom);
}

void
Block::resize(int X, int Y, int W, int H)
{
	int sb = scroll_box.w();
	Fl_Group::resize(X, Y, W, H);

	// scroll_box wants to resize when the block resizes, so fix it up
	Rect b = rect(body);

	scroll_box.resize(b.x, b.b() - sb, sb, sb);
	time_sb.resize(b.x, b.y, sb, b.h - sb);

	// o.resize(time_sb, o.r(scroll_box), o.y(time_sb), o.w(body) - sb, sb);
}

void
Block::update_size(int title_sz, int ruler_sz, int sb_sz, Orientation o)
{
	// resize from parent to child so parent resizes don't mess up the
	// children
	title.resize(x(), y(), w(), title_sz);
	body.resize(x(), y() + title_sz, w(), h() - title_sz);

	Rect b = rect(body);
	scroll_box.resize(b.x, b.b() - sb_sz, sb_sz, sb_sz);

	if (o == Horizontal_time) {
		time_sb.resize(sb_sz, b.b() - sb_sz, b.w - sb_sz, sb_sz);
		time_sb.type(FL_HORIZONTAL);
		track_sb.type(FL_VERTICAL);
		body_tile.resize(b.x, b.y, b.w, b.h - sb_sz);
	} else {
		time_sb.resize(b.x, b.y, sb_sz, b.h - sb_sz);
		time_sb.type(FL_VERTICAL);
		track_sb.type(FL_HORIZONTAL);
		body_tile.resize(b.x + sb_sz, b.y, b.w - sb_sz, b.h);
	}
	o.resize(ruler_group, o.x(body_tile), o.y(body_tile),
		o.w(body_tile), ruler_sz);
	b = rect(ruler_group);
	if (o == Horizontal_time) {
		ruler_box.resize(b.x, b.y, sb_sz, ruler_sz);
		ruler.resize(b.x + sb_sz, b.y, b.w - sb_sz, ruler_sz);
	} else {
		ruler_box.resize(b.x, b.b() - sb_sz, ruler_sz, sb_sz);
		ruler.resize(b.x, b.y, ruler_sz, b.h - sb_sz);
	}
	// ruler.orientation(o);

	o.resize(track_group, o.x(body_tile), o.y(body_tile) + ruler_sz,
		o.w(body_tile), o.h(body_tile) - ruler_sz);
	b = rect(track_group);
	if (o == Horizontal_time) {
		track_sb.resize(b.x, b.y, sb_sz, b.h);
		track_zoom.resize(b.x + sb_sz, b.y, b.w - sb_sz, b.h);
	} else {
		track_sb.resize(b.x, b.b() - sb_sz, b.w, sb_sz);
		track_zoom.resize(b.x, b.y, b.w, b.h - sb_sz);
	}
	// track_tile.orientation(o);
	track_tile.resize(track_zoom.x(), track_zoom.y(),
		track_zoom.w(), track_zoom.h());
 
	init_sizes();
	body.init_sizes();
	body_tile.init_sizes();
	ruler_group.init_sizes();
	track_group.init_sizes();
	// track_tile.init_sizes();

	print_children(this);
}
