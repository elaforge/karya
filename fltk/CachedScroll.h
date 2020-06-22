// Copyright 2020 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

#include <memory>
#include <FL/Fl_Scroll.H>
#include <FL/Fl_RGB_Image.H>

#include "global.h"


// This class assumes it has only one child, which will typically be larger
// than its parent.  On the first draw, the child will be cached in an image,
// and subsequent draws will reuse that image, until explicitly invalidated.
class CachedScroll : public Fl_Group {
public:
    CachedScroll(int x, int y, int w, int h) :
        Fl_Group(x, y, w, h), valid(false), offset(0, 0), image(nullptr)
    {}
    // Force a redraw.  See NOTE [invalidate].
    void invalidate() { valid = false; }
    void resize(int x, int y, int w, int h) override;
    void set_bg(Color c) {
        this->bg = c;
        damage(FL_DAMAGE_SCROLL);
    }

    IPoint get_offset() const { return offset; }
    void set_offset(IPoint offset);

protected:
    void draw() override;

private:
    void redraw();
    bool valid;
    IPoint offset;
    IPoint prev_offset;
    Color bg;
    std::unique_ptr<Fl_RGB_Image> image;
};

/*
   NOTE [invalidate]:

    CachedScroll has an extra invalidate() method.  Normally this is what
    damage is for, specifically FL_DAMAGE_SCROLL, but this doesn't work when
    the CachedScroll is moved (as SimpleScroll will do).  The SimpleScroll will
    set FL_DAMAGE_SCROLL on itself, but that makes Fl_Group force a redraw on
    all children.  The way to do it with damage would be set FL_DAMAGE_SCROLL
    on the CachedScroll, but that breaks modularity since now SimpleScroll has
    to know if there are any CachedScrolls underneath it.  And once the parent
    has a non FL_DAMAGE_CHILD damage, it will force FL_DAMAGE_ALL on all its
    children, which is all bits set, so any existing damage will be masked out.

    Really the CachedScroll should know itself that it can redraw efficiently
    when it's been moved, in addition to scrolled.  But I can't think of any
    way to detect that.  So I flip the default and make it always use the cache
    unless explicitly invalidated.  This is like adding my own damage bit, but
    it doesn't get masked by FL_DAMAGE_ALL.  Of course then I risk not
    redrawing when I should, but it should be pretty obvious when it happens.
*/
