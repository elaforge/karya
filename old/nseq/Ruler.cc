#include <FL/Fl_Widget.H>
#include <FL/Fl_Window.H>
#include <FL/fl_draw.H>

// sequencer stuff
#include "util.h"
#include "s_util.h"
#include "Marklist.h"
#include "state.h"

// widgets
#include "f_util.h"

#include "Ruler.h"

#include "alpha_draw.h" // for transparent cursor

namespace widgets {

#define DPRINT(X) if (Ruler_debug) std::cout << X
namespace { bool Ruler_debug = true; }

void
Overlay_ruler::resize(int X, int Y, int W, int H)
{
	//DPRINT("resize\n");
	Fl_Group::resize(X, Y, W, H);
	//print_widget(this);
	//print_widget(parent()->parent()->child(1));
}

void
Overlay_ruler::draw()
{
	Rect c = rect(this);
	fl_clip_box(c.x, c.y, c.w, c.h, c.x, c.y, c.w, c.h);
	draw_area(c);
	return;
	if (damage() & (FL_DAMAGE_ALL | DAMAGE_ZOOM)) {
		draw_area(c);
	} else if (damage() & FL_DAMAGE_SCROLL) {
		Point d(pixel_offset - offset.to_screen(_zoom), 0);
		DPRINT("offset: " << offset.to_screen(_zoom) << " poff: " << d.x << '\n');
		fl_scroll(x(), y(), w(), h(), d.x, d.y, draw_area_cb, this);
	}
	if (damage() & FL_DAMAGE_CHILD)
		draw_area(c);
	pixel_offset = offset.to_screen(_zoom);
}

void
Overlay_ruler::draw_area_cb(void *vp, int x, int y, int w, int h)
{
	Overlay_ruler *self = static_cast<Overlay_ruler *>(vp);
	Rect c(x, y, w, h);
	Clip_area _clip(c);
	self->draw_area(c);
}

void
Overlay_ruler::draw_area(Rect c)
{
	Fl_Group::draw();
	draw_marks(c);
}

void // b widgets::Overlay_ruler::draw_marks
Overlay_ruler::draw_marks(Rect c)
{
	Trackpos tstart = Trackpos::from_screen(o.x(c) - o.x(this), _zoom) + offset;
	Trackpos tend = Trackpos::from_screen(o.r(c) - o.x(this), _zoom) + offset;
	// DPRINT("marks " << tstart << "--" << tend << ", zoom: " << _zoom
	// 	<< '\n');
	// draw from bottom up
	for (int i = _marklists.size() - 1; i >= 0; i--) {
			static int lastx;
			lastx = 0;
			static Trackpos lastp;
			lastp = Trackpos();
			if (o.y(this) < 40)
		DPRINT(i << ' ' << offset << ":");
		seq::Marklist_view &m = _marklists[i];
		m.find(tstart, _zoom);
		for (; m.mark() && m.mark()->pos < tend; m.next()) {
			int xoffset = to_x(m.mark()->pos);
			// DPRINT(' ' << (m.mark()->name ? m.mark()->name[0] : ' ')
			// 	<< m.mark()->pos << '@' << to_x(m.mark()->pos)-o.x(this));
			if (o.y(this) < 40 && xoffset < 160 && xoffset-o.x(this) > 0)
			DPRINT('\t' // << (m.mark()->name ? m.mark()->name[0] : ' ')
				<< m.mark()->pos - offset << '\t'
				<< (m.mark()->pos - offset).to_screen(_zoom) );
				// << xoffset - lastx << ' '
				// << m.mark()->pos << " - " << offset << ' ' << _zoom);
				// << m.mark()->pos - lastp); //  << ' ' << xoffset - o.x(this));
			lastx = xoffset;
			lastp = m.mark()->pos;
			// is g++ smart enough to notice that 'o' is constant and push o.x out
			// of the loop?
			draw_mark(m.mark(), to_x(m.mark()->pos), i);
		}
			if (o.y(this) < 40)
		DPRINT('\n');
	}
}

Rect
Overlay_ruler::mark_rect(const seq::Mark *m, int xoffset, int i) const
{
	return o.rect(xoffset, o.y(this), int(m->width), o.h(this));
}

void
Overlay_ruler::draw_mark(const seq::Mark *m, int xoffset, int i)
{
	Rect r = mark_rect(m, xoffset, i);
	if (m->width) {
		fl_color(rgba_to_fl(m->color));
		fl_rectf(r.x, r.y, r.w, r.h);
	}
	if (show_names && m->show_name && (m->name || m->mkname)) {
		const char *s = m->name ? m->name : m->mkname(m);
		fl_font(m->font_info->face, m->font_info->size);
		fl_color(m->font_info->color);
		if (o == seq::Horizontal_time) {
			fl_draw(s, r.x, r.y - fl_descent() + fl_height());
		} else {
			// draw tiny m->state->name vertically
		}
	}
}

///

void
Selection_ruler::draw_area(Rect c)
{
	Fl_Group::draw();
	draw_selection(_selection);
	draw_marks(c);
	draw_cursor(_selection);
}

void
Selection_ruler::selection_damage(Selection<Trackpos> &sel)
{
	Trange ns, os;
	Rect d;
	// account for cursor width
	if (sel.get_selection(ns) && ns.empty()) {
		Rect r = o.rect(to_x(ns.start), o.y(this), cursor_width, o.h(this));
		DPRINT("y" << o.y(this) << " ns dmg: " << r << '\n');
		d.union_(r);
	}
	if (sel.get_old_selection(os) && os.empty()) {
		Rect r = o.rect(to_x(os.start), o.y(this), cursor_width, o.h(this));
		DPRINT("y" << o.y(this) << " os dmg: " << r << '\n');
		d.union_(r);
	}
	Trange td = sel.draw_range();
	Rect r = o.rect(to_x(td.start), o.y(this), to_w(td.extent), o.h(this));
	DPRINT("y" << o.y(this) << " td dmg: " << r << '\n');
	d.union_(r);
	d.clip(rect(this)); // selection should be clipped to Trackpos range already, but still
	if (d.w && d.h) {
		DPRINT("y" << o.y(this) << " sel dmg: " << td << ' ' << d
			<< " (" << d.r() << ' ' << d.b() << ")\n");
		damage(FL_DAMAGE_ALL, d.x, d.y, d.w, d.h);
	}
}

void
Selection_ruler::draw_selection(const Selection<Trackpos> &sel)
{
	Trange s;
	if (!sel.get_selection(s))
		return;
	Rect r = o.rect(to_x(s.start), o.y(this), to_w(s.extent), o.h(this));
	if (fl_not_clipped(r.x, r.y, r.w, r.h)) {
		fl_color(sel.color);
		fl_rectf(r.x, r.y, r.w, r.h);
	}
}

void
Selection_ruler::draw_cursor(const Selection<Trackpos> &sel)
{
	Trange s;
	if (!sel.get_selection(s) || !s.empty())
		return;
	Rect r = o.rect(to_x(s.start), o.y(this), cursor_width, o.h(this));
	if (fl_not_clipped(r.x, r.y, r.w, r.h))
		draw::rectf(r, Rgba_color(0, 0, 0, 0x7f));
}

///

void
Ruler::draw_area(Rect c)
{
	Selection_ruler::draw_area(c);
	draw_mouse_pos();
}

Rect
Ruler::mark_rect(const seq::Mark *m, int xoffset, int i) const
{
	int ry = int(o.h(this) * (double(i) / _marklists.size()));
	return o.rect(xoffset, o.y(this) + ry, int(m->width), o.h(this) - ry);
}

void
Ruler::draw_mouse_pos()
{
	if (_mouse_pos < 0)
		return;
	Rect r = mouse_pos_rect();
	fl_color(fl_darker(background.color()));
	fl_rectf(r.x, r.y, r.w, r.h);
}

/*
this used to use fl_overlay, but
the problem with this is that it doesn't notice if the ruler has been redrawn
(expose, etc) erasing the overlay, so there's an extra fl_overlay_clear
which results in an extra line hanging out
ruler should be redrawn *with* the overlay.
draw() doesn't get called whenever the ruler is redrawn.
someone is keeping a bitmap of the ruler and redrawing it without the overlay
*/
void
Ruler::mouse_pos(Point p)
{
	Rect r = mouse_pos_rect();
	damage(FL_DAMAGE_ALL, r.x, r.y, r.w, r.h);
	_mouse_pos = o.x(p) + pixel_offset;
	r = mouse_pos_rect();
	damage(FL_DAMAGE_ALL, r.x, r.y, r.w, r.h);
}

}
