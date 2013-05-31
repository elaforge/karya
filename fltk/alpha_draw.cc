// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

/*
    Fancy alpha-channel using draw routines.
*/
#include <FL/Fl_Image.H>
#include <FL/fl_draw.H>

#include "f_util.h"
#include "alpha_draw.h"


void alpha_rectf(IRect r, Color c)
{
    if (!fl_not_clipped(r.x, r.y, r.w, r.h))
        return;
    // If it has no alpha, don't bother doing the aplha stuff.
    if (c.a == 0xff) {
        fl_color(color_to_fl(c));
        fl_rectf(r.x, r.y, r.w, r.h);
        return;
    }
    int nbytes = r.w * r.h * 4;
    // Don't crash if the caller wants a negative sized rect.
    if (nbytes <= 0)
        return;
    unsigned char *data = new unsigned char[nbytes];
    for (int i = 0; i < nbytes; i+=4) {
        data[i] = c.r;
        data[i+1] = c.g;
        data[i+2] = c.b;
        // Just for fun, make the last line more transparent.
        /*
        if (r.h > 1 && i >= r.w * (r.h-1) * 4)
            data[i+3] = c.a / 3;
        else
        */
            data[i+3] = c.a;
    }
    Fl_RGB_Image im(data, r.w, r.h, 4);
    im.draw(r.x, r.y);
    delete[] data;
}
