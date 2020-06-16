// Copyright 2016 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <utility>

#include <FL/fl_draw.H>

#include "alpha_draw.h"
#include "f_util.h"
#include "types.h"

#include "SelectionOverlay.h"


// Selections are always at least this many pixels.
const static int selection_min_size = 2;


static void
draw_arrow(int x, int y, const Selection &sel)
{
    const int sz = SelectionOverlay::selection_point_size;
    // Cur track and cur time gets an arrow.  Not cur track gets no arrows.
    // Hopefully it's visible enough if it's a point selection.
    switch (sel.orientation) {
    case Selection::SelNone:
        break;
    case Selection::Positive:
        fl_color(sel.color.fl());
        fl_polygon(
            x, y,
            x+sz, y,
            x, y+sz);
        break;
    case Selection::Negative:
        fl_color(sel.color.fl());
        fl_polygon(
            x, y-sz,
            x+sz, y,
            x, y);
        break;
    case Selection::Both:
        fl_color(sel.color.fl());
        fl_polygon(
            x, y-sz,
            x+sz, y,
            x, y+sz);
        break;
    }
}


void
SelectionOverlay::draw(int x, int y, int w, const Zoom &zoom)
{
    IRect sel_rect;
    for (const std::vector<Selection> &sels : this->selections) {
        for (const Selection &sel : sels) {
            if (sel.empty())
                continue;
            int start = y + zoom.to_pixels(sel.low() - zoom.offset);
            int height = std::max(
                selection_min_size, zoom.to_pixels(sel.high() - sel.low()));
            // IRect intersection is half-open ranges, but rect drawing is
            // inclusive pixel ranges.  So add one to ensure that if I share a
            // pixel border with the clip rect, I'll still draw that pixel
            // line.
            sel_rect = f_util::clip_rect(IRect(x, start, w, height + 1));
            fl_line_style(FL_SOLID, 0);
            // TODO darken for orientation == Negative?
            alpha_rectf(sel_rect, sel.color);

            // Darken the the cur pos a bit, and make it non-transparent.
            fl_color(sel.color.brightness(0.5).fl());
            int cur = y + zoom.to_pixels(sel.cur - zoom.offset);
            fl_line(x + 2, cur, x + w - 2, cur);
            draw_arrow(x, cur, sel);
        }
    }
}


const std::vector<Selection> &
SelectionOverlay::get(int selnum)
{
    selections.resize(std::max(int(selections.size()), selnum + 1));
    return selections[selnum];
}


void
SelectionOverlay::set(int selnum, const std::vector<Selection> &news)
{
    selections.resize(std::max(int(selections.size()), selnum + 1));
    selections[selnum] = news;
}
