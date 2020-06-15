// Copyright 2020 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include "f_util.h"
#include "util.h"

#include <FL/Fl_Image_Surface.H>
#include <FL/Fl_RGB_Image.H>

#include "CachedScroll.h"


void
CachedScroll::resize(int x, int y, int w, int h)
{
    // Override Fl_Group::resize to not resize the child.
    Fl_Widget::resize(x, y, w, h);
}


void
CachedScroll::set_offset(IPoint offset)
{
    this->offset = offset;
    this->damage(FL_DAMAGE_SCROLL);
}


void
CachedScroll::draw()
{
    if (!image || !valid) {
        this->redraw();
        valid = true;
    };

    // Draw the background if the image won't cover it.  Offset goes negative,
    // so this is a minus.
    if (image->w() + offset.x < w() || image->h() + offset.y < h()) {
        fl_color(bg.fl());
        fl_rectf(x(), y(), w(), h());
    }
    // DEBUG("draw image to " << f_util::rect(this) << " offset: " << offset);
    // DEBUG("(" << x() << ", " << y() + offset.y << ")" );
    image->draw(x(), y(), w(), h(), -offset.x, -offset.y);
}


void
CachedScroll::redraw()
{
    ASSERT(children() == 1);
    const int highres = 1; // OS X retina
    // TODO how to detect high dpi?

    Fl_Widget *c = child(0);
    // seems the set_current() resets coords back to 0, so they draw shifted.

    Fl_Image_Surface surface(c->w(), c->h(), highres);
    // Apparently this will translate the child's (x, y) back to (0, 0) so it
    // gets drawn at (0, 0) on the surface.
    surface.draw(c);

    this->image.reset(surface.image());
    // DEBUG("REDRAW: " << f_util::rect(this) << ", child: "
    //     << f_util::show_children(c));
    // DEBUG("image size: " << image->w() << "x" << image->h() << ", "
    //     << (image->w() * image->h() * image->d()) / 1024 << "kb");
}
