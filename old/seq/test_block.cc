#include <FL/Fl_Double_Window.H>
#include <FL/Fl.H>
#include <FL/Fl_Box.H>

#include "util.h"
// sequencer stuff
#include "s_util.h"
#include "Marklist.h"
#include "state.h"

// widgets
#include "f_util.h"
#include "A_input.h"
#include "A_scrollbar.h"
#include "Event.h"
#include "Ruler.h"
#include "Track.h"
#include "Zoom.h"
#include "Tile_ext.h"
#include "Track_tile.h"
#include "Block.h"

// sequencer stuff, but depends on widgets
#include "view.h"

using seq::Trackpos;
using seq::Zoom_t;
using seq::Mlist;

const Fl_Color Cblack =
	fl_rgb_color(0, 0, 0),
	Cwhite = fl_rgb_color(0xff, 0xff, 0xff),
	Cred = fl_rgb_color(0xff, 0x00, 0x00),
	Cpaleyellow = fl_rgb_color(0xff, 0xff, 0xdd),
	Cdarkyellow = fl_rgb_color(0xff, 0xff, 0x99),
	Cpaleblue = fl_rgb_color(0xdd, 0xff, 0xff);

Trackpos seconds(int p) { return Trackpos::from_sec(p); }

void
set_defaults(seq::Defaults &d)
{
	seq::Block_state::Colors block_colors;
	seq::Track_state::Colors track_colors;
	seq::Event_state::Colors event_colors;
	d.title_size = 20;
	d.scrollbar_size = 16;
	d.ruler_size = 24;
	d.window_size.x = 400 + d.scrollbar_size;
	d.window_size.y = 200 + d.scrollbar_size + d.ruler_size;
	d.orientation = seq::Orientation(seq::Horizontal_time);
	d.track_size = 64;
	d.time_zoom_speed = 100;
	d.track_zoom_speed = 25;
	block_colors.scroll_box = block_colors.ruler_box = Cpaleblue;
	block_colors.ruler = Cdarkyellow;
	block_colors.selection = fl_darker(block_colors.ruler);
	block_colors.track_tile = FL_GRAY;
	track_colors.bg = Cpaleyellow;
	track_colors.selections.push_back(Cdarkyellow);
	track_colors.selections.push_back(Cred);
	event_colors.bg = Cwhite;
	event_colors.text = Cblack;
	d.block_colors = block_colors;
	d.track_colors = track_colors;
	d.event_colors = event_colors;
}

static Mlist *
infinite_list(seq::Stack_mark m, Trackpos offset)
{
	seq::Stack_mark om;
	om.extent = offset;
	Mlist *list = new Mlist(m, 0),
		*olist = new Mlist(om, list);
	list->append(list);
	return olist;
}

static Mlist *
infinite_list(seq::Stack_mark m)
{
	Mlist *list = new Mlist(m, 0);
	list->append(list);
	return list;
}

static const Mlist *mlist_end(const Zoom_t z, int rank) { return 0; }

static const Mlist *
m44_quarter(const Zoom_t z, int rank)
{
	static const int min_pixels = 10; // don't display if < this much space
	static Mlist *list;
	if (!list) {
		seq::Stack_mark m;
		m.color = Rgba_color(0xd0, 0x90, 0xa0, 0x40);
		m.name = "quarter";
		m.width = 2;
		m.show_name = false;
		m.extent = seconds(10);
		m.sublist = mlist_end;
		list = infinite_list(m);
	}
	if (z.pixel_per_sec() * 10 < min_pixels)
		return 0;
	else
		return list;
}

static const Mlist *
m44_measure(const Zoom_t z, int rank)
{
	static Mlist *list;
	if (!list) {
		seq::Stack_mark m;
		m.color = Rgba_color(0x50, 0x50, 0xff, 0x7f);
		m.name = "measure";
		m.width = 4;
		m.show_name = false;
		m.extent = seconds(40);
		m.sublist = m44_quarter;
		list = infinite_list(m, Trackpos());
	}
	return list;
}

static const char *
fmt_markpos(const seq::Mark *self)
{
	static char buf[64];
	snprintf(buf, sizeof buf, "%.3gs", self->pos.to_sec());
	return buf;
}

static const Mlist *
mtime_ruler(const Zoom_t z, int rank)
{
	// for reasons I don't yet understand, if lists contains Mlists directly
	// return addresses means they get garbaged after a few accesses
	// so we use pointers and dynamic store
	static std::vector<Mlist *> lists;
	static seq::Font_info font_info = { fl_font(), 8, FL_BLACK };
	int extent = max(1, 128>>rank);
	if (rank != 0 && ((128>>rank == 0) || z.pixel_per_sec()*extent < 32))
		return 0;
	while (int(lists.size()) <= rank) {
		seq::Stack_mark m;
		m.mkname = fmt_markpos;
		m.font_info = &font_info;
		m.width = 1;
		m.color = Rgba_color(0, 0, 0);
		m.show_name = true;
		m.extent = seconds(extent);
		m.sublist = mtime_ruler;
		if (rank == 0)
			lists.push_back(infinite_list(m, Trackpos()));
		else
			lists.push_back(infinite_list(m));
	}
	return lists[rank];
}

int
main(int argc, char **argv)
try {
	static seq::Block_state bst;
	static seq::Defaults d;

	set_defaults(d);
	bst.title = "test block";
	bst.colors = &d.block_colors;

	Fl_Double_Window win(d.window_size.x, d.window_size.y);
	win.resizable(win);
	seq::Block_view *block = new seq::Block_view(&win, &bst, &d);
	seq::Marklist mmeasures("measures", m44_measure);
	seq::Marklist mtime("time", mtime_ruler);

	static seq::Track_state tst, tst2;
	tst.length = seconds(100);
	tst.colors = &d.track_colors;
	block->insert_track(0, &tst);
	tst2.length = seconds(80);
	tst2.colors = &d.track_colors;
	block->insert_track(1, &tst2);
	for (int i = 0; i < 2; i++) {
		// block->track(i)->insert_marklist(0, &mtime);
		block->track(i)->insert_marklist(0, &mmeasures);
	}
	// block->insert_ruler_marklist(0, &mtime);
	block->insert_ruler_marklist(1, &mmeasures);

	// Rect z(0, 32, 100, 64); // vzoom
	// Rect z(50, 0, 50, 64*3); // hzoom
	Rect z(0, 0, 60, 64*3);
	block->absolute_zoom(seq::Trect(
		seconds(z.x), seconds(z.y), seconds(z.w), seconds(z.h)));
	// block->relative_zoom(Dpoint(2, 1), Dpoint(.5, 0));
	widgets::print_children(&win);
	win.show(argc, argv);
	Fl::run();
// } catch (Assertion_error a) {
} catch (int a) {
	std::cerr << a << '\n';
	exit(1);
}
