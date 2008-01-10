#include "util.h"
// sequencer stuff
#include "s_util.h"
#include "Marklist.h"
#include "state.h"

// widgets
#include "f_util.h"

#include "Event.h"
#include "Ruler.h"
#include "Track.h"

namespace widgets {

#define DPRINT(X) if (Track::Debug) std::cout << X
bool Track::Debug = true;

Track::Track(int X, int Y, int W, int H, Orientation o, Tpoint sz) :
	Selection_ruler(X, Y, W, H, o, 3),
	_dimensions(sz),
	selections(Max_button-1)
{
	end();
}


void
Track::dimensions(const Tpoint &d)
{
	// resize self, possibly delete events out of range
	_dimensions = d;
}

int
Track::handle(int ev)
{
	return Overlay_ruler::handle(ev);
}

void
Track::draw()
{
	fl_color(color());
	fl_rectf(x(), y(), w(), h());
	Overlay_ruler::draw();
	Rect r = o.rect(o.x(this)-4, o.y(this), o.w(this)+4, o.h(this));
	draw_box(FL_DOWN_FRAME, r.x, r.y, r.w, r.h, color());
}

void
Track::draw_area(Rect c)
{
	Fl_Group::draw();
	draw_selection(_selection);
	for (unsigned i = 0; i < selections.size(); i++)
		draw_selection(selections[i]);
	draw_marks(c);
	draw_cursor(_selection);
}

/* later, the selection functions will invoke callbacks */
void
Track::select_range(int n, Trange r)
{
	r.clip(Trange(Trackpos(), dimensions().x));
	DPRINT("t" << o.y(this) << " select_range: " << n << ' ' << r << ' '
		<<dimensions().x<<'\n');
	getsel(n).selection(r);
	selection_damage(getsel(n));
}

void
Track::select_release(int n)
{
	DPRINT("t" << o.y(this) << " select_release: " << n << '\n');
}

void
Track::clear_selection(int n)
{
	DPRINT("t" << o.y(this) << " clear_selection: " << n << '\n');
	getsel(n).selection(false);
	selection_damage(getsel(n));
}

}
