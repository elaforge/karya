#include <FL/Fl_Widget.H>
#include "util.h"
#include "s_util.h"

namespace seq {

// can't be inlined because they need the complete type of Zoom_t
int
Trackpos::to_screen(const Zoom_t &zoom) const
{
	// everyone who uses to_screen() is ok with the clamping, right?
	double z = zoom.trackpos_per_pixel;
	return int(clamp(double(INT_MIN), v / z, double(INT_MAX)));
}

Trackpos
Trackpos::from_screen(int p, const Zoom_t &zoom)
{
	return Trackpos(vtype(p * zoom.trackpos_per_pixel));
}

double
Trackpos::to_sec() const
{
	return double(v) / per_second;
}

}
