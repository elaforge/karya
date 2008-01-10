#include <FL/Fl.H>
#include <FL/Fl_Scroll.H>
#include <FL/fl_draw.H>
#include <FL/Fl_Window.H>
#include <math.h>

#include "util.h"
// sequencer stuff
#include "s_util.h"
#include "Marklist.h"
#include "state.h"

#include "f_util.h"
#include "Zoom.h"

#include "alpha_draw.h"

namespace widgets {

Zoom::Zoom(int X, int Y, int W, int H) :
	Fl_Group(X, Y, W, H),
	_zoom_speed(0, 0),
	zoom_button(1), zooming(false),
	zoom_key_down(0),
	box_zooming(false), box_key_down(0)
{}

void
Zoom::draw()
{
	fl_overlay_clear();
	Fl_Group::draw();
	if (zooming) {
		// draw arrows?
		if (_zoom_speed.x)
			draw::rectf(Rect(zoom_center.x - 2, y(), 4, h()),
				Rgba_color(0, 0, 0, 0xaa));
		if (_zoom_speed.y)
			draw::rectf(Rect(x(), zoom_center.y - 2, w(), 4),
				Rgba_color(0, 0, 0, 0xaa));
	}
}

int
Zoom::handle(const int ev)
{
	const int key = Fl::event_key();
	const int button = Fl::event_button();

	switch (ev) {
	case FL_FOCUS:
		if (_zoom_speed.x || _zoom_speed.y)
			return true;
	case FL_KEYDOWN:
		if (!rect(this).contains(mouse_pos()))
			break;
		if (!box_zooming && !zoom_key_down && is_zoom_key(key)
			&& (_zoom_speed.x || _zoom_speed.y))
		{
			zoom_key_down = key;
			if (_zoom_speed.x && _zoom_speed.y)
				fl_cursor(FL_CURSOR_MOVE);
			else if (_zoom_speed.x)
				fl_cursor(FL_CURSOR_WE);
			else
				fl_cursor(FL_CURSOR_NS);
			return true;
		} else if (!zooming && !box_key_down && is_box_key(key)) {
			box_key_down = key;
			fl_cursor(FL_CURSOR_CROSS);
			return true;
		}
		break;
	case FL_KEYUP:
		if (zoom_key_down && key == zoom_key_down) {
			zoom_key_down = 0;
			fl_cursor(FL_CURSOR_DEFAULT);
			// erase zoombar overlay
			damage(FL_DAMAGE_ALL);
			return true;
		} else if (box_key_down && key == box_key_down) {
			box_key_down = 0;
			fl_cursor(FL_CURSOR_DEFAULT);
			return true;
		}
		break;
	case FL_PUSH:
		if (zoom_key_down && button == zoom_button) {
			zoom_center = mouse_pos();
			old_zoom = cur_zoom;
			zooming = true;
			return true;
		} else if (zooming) { // cancel zoom
			assert(button != zoom_button);
			zoom(old_zoom);
			zooming = false;
			return true;
		} else if (box_key_down && button == zoom_button) {
			box_zooming = true;
			box_start = mouse_pos();
		}
		break;
	case FL_RELEASE:
		if (zooming && button == zoom_button) {
			// let children redraw in non-zoom mode
			damage(FL_DAMAGE_ALL);
			zooming = false;
			return true;
		} else if (box_zooming && button == zoom_button) {
			box_zooming = false;
			window()->make_current();
			fl_overlay_clear();
			Point m = mouse_pos();
			Rect b(box_start.x, box_start.y,
				m.x - box_start.x, m.y - box_start.y);
			b.normalize();
			// XXX fix bounding box stuff
			/* Drect z(cur_zoom.x * (double(b.x) / w()),
				cur_zoom.y * (double(b.y) / h()),
				cur_zoom.w * (double(b.w) / w()),
				cur_zoom.h * (double(b.h) / h()));
			zoom(z); */
		}
		break;
	case FL_DRAG:
		if (zooming) {
			zoom(handle_drag(old_zoom, zoom_center, mouse_pos()));
			return true;
		} else if (box_zooming) {
			Point m = mouse_pos();
			Rect r(box_start.x, box_start.y, m.x - box_start.x,
				m.y - box_start.y);
			r.normalize();
			r.clip(rect(this));
			window()->make_current();
			fl_overlay_clear();
			fl_overlay_rect(r.x, r.y, r.w, r.h);
		}
		break;
	case FL_ENTER:
		if (zoom_key_down) {
			if (Fl::get_key(zoom_key_down)) {
				if (_zoom_speed.x && _zoom_speed.y)
					fl_cursor(FL_CURSOR_MOVE);
				else if (_zoom_speed.x)
					fl_cursor(FL_CURSOR_WE);
				else
					fl_cursor(FL_CURSOR_NS);
			} else {
				damage(FL_DAMAGE_ALL);
				zoom_key_down = 0;
				fl_cursor(FL_CURSOR_DEFAULT);
			}
		}
	case FL_LEAVE:
		//if (zooming)
		//	damage(FL_DAMAGE_ALL);
		break;
	}
	if (zoom_key_down || box_key_down)
		return true; // no one else gets events while in zoom mode
	else
		return Fl_Group::handle(ev);
}

static double
zoom_func(double speed, int d)
{
	// mouse moves zoom_speed, x doubles or halves
	if (d >= 0)
		return 1 + d / speed;
	else
		return  1 / (1 + fabs(d) / speed);
}

// given mouse and zoom_center, return appropriate zoom rectangle
Zoom_info
Zoom::handle_drag(const Zoom_info &old_zoom, const Point &center,
		const Point &mouse) const
{
	Point d(mouse.x - center.x, mouse.y - center.y);
	Dpoint z(
		_zoom_speed.x == 0 ? 1 : zoom_func(_zoom_speed.x, d.y),
		_zoom_speed.y == 0 ? 1 : zoom_func(_zoom_speed.y, d.x));
	// zoom around the middle of the pixel
	Dpoint c((center.x - x() + .5) / w(), (center.y - y() + .5) / h());
	Zoom_info r(old_zoom);
	r.center_zoom(z, c);
	return r;
}

void
Zoom::zoom(const Zoom_info &to)
{
	damage(FL_DAMAGE_ALL | DAMAGE_ZOOM);
	cur_zoom = to;
	do_callback();
}

}
