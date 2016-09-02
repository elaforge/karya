// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

/*
    Fancy alpha-channel using draw routines.
*/
#include <FL/Fl_Image.H>
#include <FL/fl_draw.H>

#include "Color.h"
#include "f_util.h"

#include "alpha_draw.h"


void alpha_rectf(IRect r, Color c)
{
    if (!fl_not_clipped(r.x, r.y, r.w, r.h))
        return;
    // Fast path for no alpha.
    if (c.a == 0xff) {
        fl_color(c.fl());
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
        data[i+3] = c.a;
    }
    Fl_RGB_Image im(data, r.w, r.h, 4);
    im.draw(r.x, r.y);
    delete[] data;
}
