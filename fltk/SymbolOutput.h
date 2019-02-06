// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

#include <stdlib.h>
#include <string>

#include <FL/Fl_Box.H>


// This is like Fl_Output, except it understands `` symbols.
class SymbolOutput : public Fl_Box {
public:
    SymbolOutput(int x, int y, int w, int h, const char *label = 0)
        : Fl_Box(x, y, w, h, label)
    {
        box(FL_FLAT_BOX);
        color(FL_WHITE);
    }

    void value(const char *text);
protected:
    void draw() override;
private:
    std::string text;
};
