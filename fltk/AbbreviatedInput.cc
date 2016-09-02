// Copyright 2016 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <FL/fl_draw.H>
#include <FL/Fl_Input_.H>
#include <FL/Fl_Multiline_Input.H>

#include "AbbreviatedInput.h"
#include "config.h"


AbbreviatedInput::AbbreviatedInput(int x, int y, int w, int h) :
    Fl_Multiline_Input(x, y, w, h)
{
    color(FL_WHITE);
    textsize(Config::font_size::input);
    box(FL_THIN_DOWN_BOX);
    when(0);
}


int
AbbreviatedInput::mouse_position()
{
    // handle_mouse() is undocumented.
    // Fl_Input uses x()+Fl::box_dx(box()), y()+Fl::box_dy(box()) for the first
    // two parameters.  In practice, though, it seems to always be 1, 1, and
    // that's what works.  3rd and 4th parameters are unused.
    //
    // This has the side-effect of setting the cursor, but since I never focus
    // on one of these, it shouldn't matter.
    Fl_Input_::handle_mouse(1, 1, 0, 0, false);
    return position();
}


int
AbbreviatedInput::handle(int evt)
{
    switch (evt) {
    case FL_FOCUS:
    case FL_PUSH:
        do_callback();
        break;
    }
    return true;
}


void
AbbreviatedInput::draw()
{
    Fl_Multiline_Input::draw();
    if (fl_width(value()) > w()) {
        fl_color(Config::abbreviation_color.fl());
        fl_rectf(x(), y() + h() - 3, w(), 3);
    }
}
