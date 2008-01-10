/*
*/

#ifndef _ZOOM_H
#define	_ZOOM_H

class Zoom {
public:
	Zoom() : trackpos_per_pixel(1.0) {}
	bool operator==(const Zoom &o) const {
		return trackpos_per_pixel == o.trackpos_per_pixel;
	}
	bool operator!=(const Zoom &o) const { return !(*this == o); }
private:
	double trackpos_per_pixel;
};

#endif
