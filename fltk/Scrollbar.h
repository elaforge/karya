// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

#include <FL/Fl_Group.H>
#include <FL/Fl_Box.H>

#include "f_util.h"
#include "global.h"


// Simple scrollbar.
class Scrollbar : public Fl_Group {
public:
    Scrollbar(int x, int y, int w, int h, Color bg, Color fg);
    void resize(int x, int y, int w, int h) override;

    enum Orientation { horizontal, vertical };
    void set_orientation(Scrollbar::Orientation o);

    double get_offset() const { return offset; }
    double get_size() const { return size; }

    // Set scrollbar to 'offset' with given size.  Both are 0--1 and represent
    // the percentage of the way down the scroll space, and the percentage
    // of the total space visible.
    void set_scroll(double offset, double size);
    void set_scroll_zoom(double max, double offset, double displayed_area);

protected:
    int handle(int evt) override;

private:
    Orientation orientation;
    double offset;
    double size;
    int min_size;
    Fl_Box scrollbox;
};
