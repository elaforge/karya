// Copyright 2016 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#ifndef __ABBREVIATED_INPUT_H
#define __ABBREVIATED_INPUT_H

#include <string>
#include <FL/Fl_Multiline_Input.H>

#include "global.h"


// Just like Fl_Multiline_Input, but draw a blue bar if the text exceeds the
// display width.
class AbbreviatedInput : public Fl_Multiline_Input {
public:
    AbbreviatedInput(int x, int y, int w, int h);
    int mouse_position();
    int handle(int evt) override;
protected:
    void draw() override;
};

#endif
