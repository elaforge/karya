// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

/*
    A Scrollbar with some additions
    It has an optional overlay ruler.
*/
#include <FL/Fl_Scroll.H>

#include "Color.h"
#include "Scrollbar.h"
#include "RulerTrack.h"

class SeqScrollbar : public Scrollbar {
public:
    SeqScrollbar(int x, int y, int w, int h) :
        Scrollbar(x, y, w, h, Color(255, 255, 200), Color(127, 100, 50))
    {
    }

    void set_ruler(const Marklists marklists);
};


class FlSeqScrollbar : public Fl_Scrollbar {
public:
    FlSeqScrollbar(int x, int y, int w, int h) :
        Fl_Scrollbar(x, y, w, h)
    {}
    int handle(int evt) override {
        if (evt == FL_RELEASE)
            this->do_callback();
        return Fl_Scrollbar::handle(evt);
    }
    void set_orientation(Scrollbar::Orientation o) {
        if (o == Scrollbar::horizontal)
            this->type(FL_HORIZONTAL);
        else
            this->type(FL_VERTICAL);
    }

    void scroll_position(double offset, double size);
    // This isn't const only because Fl_Scrollbar::value isn't.
    double get_offset() {
        return double(this->value()) / this->length();
    }
    // double get_size() const { return size; }
    void set_scroll_zoom(double max, double offset, double displayed_area) {
        // DEBUG("set scroll zoom " << max << " " << offset << " "
        //         << displayed_area << " value " << value());
        offset = util::clamp(0.0, 1.0, offset / max);
        displayed_area = util::clamp(0.0, max, displayed_area) / max;
        // int Fl_Slider::scrollvalue(int p, int W, int t, int l)
        //  p = position, first line displayed
        //  w = window, number of lines displayed
        //  t = top, number of first line
        //  l = length, total number of lines
        // DEBUG("scrollvalue " << int(offset*length()) << " "
        //         << int(displayed_area*length()) << " 0 " << length());

        // Without the ceil() here, this scrolls backwards by one pixel
        // every time you set it (i.e. get_offset() -> set_scroll_zoom() round
        // trip winds up subtracting off one pixel).  I don't fully understand
        // why, but I imagine it has to do with int() rounding down...
        scrollvalue(int(ceil(offset*length())), int(displayed_area*length()),
                0, length());
    }

    // Not supported.
    void set_ruler(const Marklists marklists) {}

private:
    int length() const {
        if (type() == FL_HORIZONTAL)
            return w();
        else
            return h();
    }
};
