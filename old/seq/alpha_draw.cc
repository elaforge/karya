#include "util.h"
#include "alpha_draw.h"

#include <FL/Fl_Image.H>

#include <string.h>
#include <stdio.h>

namespace draw {

// draw rectangle with c
// use fl_read_image, but it would be better to use Xrender
// download libcairo and cairo-demo to use that?
// or even use GL
void rectf(Rect r, Rgba_color c)
{
	int nbytes = r.w * r.h * 3;
	unsigned char *data = new unsigned char[nbytes];

	fl_read_image(data, r.x, r.y, r.w, r.h, false);
	for (int i = 0; i < nbytes; i++) {
		data[i] = int(data[i] * .7);
	}
	Fl_RGB_Image im(data, r.w, r.h, 3);
	im.draw(r.x, r.y);
	/*
	memset(data, 0, nbytes);
	for (int i = 3; i < nbytes; i += 4)
		data[i] = 0x20;
	Fl_RGB_Image im(data, r.w, r.h, 4);
	im.draw(r.x, r.y);
	// printf("%d %d %d\n", data[0], im[1], im[2]);
	*/
	delete[] data;
}

}
