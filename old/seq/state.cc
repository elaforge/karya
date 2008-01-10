#include "util.h"
#include "s_util.h"
#include "Marklist.h"
#include "state.h"

namespace seq {

#define DPRINT(X) if (State_debug) std::cout << X
namespace { const bool State_debug = true; }

void
Zoom_info::center_zoom(Dpoint z, Dpoint center)
{
	// DPRINT(Tpoint(_win.x, _win.w) << " <- " << z.x << ' ' << center.x <<
	// 	" za:" << _zoom_area.w << " -> ");
	_zoom.x = _zoom.x.scale(z.x);
	_zoom.y = _zoom.y.scale(z.y);
	// at zoom=2, zoom window is x/2, so scale by inverse:
	Tpoint n(_win.w.scale(1/z.x), _win.h.scale(1/z.y));
	// translate by total new space scaled by relative center pos
	_win.x += (_win.w - n.x).scale(center.x);
	_win.y += (_win.h - n.y).scale(center.y);
	_win.w = n.x;
	_win.h = n.y;
	// DPRINT(Tpoint(_win.x, _win.w) << '\n');
	_win.clamp(_zoom_area);
}

Zoom_info
Zoom_info::reorient(Orientation o) const
{
	Zoom_info r;
	r._zoom = o.point(_zoom);
	r._win = o.rect(_win);
	r._zoom_area = o.rect(_zoom_area);
	return r;
}

}
