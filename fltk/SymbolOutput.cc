// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <string.h>
#include <stdlib.h>

#include <FL/Fl_Box.H>

#include "SymbolTable.h"

#include "SymbolOutput.h"


void
SymbolOutput::value(const char *text)
{
    if (text)
        this->text = std::string(text);
    else
        this->text.clear();
    this->redraw();
}


void
SymbolOutput::draw()
{
    static const SymbolTable::Style style(
        Config::font, Config::font_size::track_status, FL_BLACK);

    Fl_Box::draw();
    IPoint p(x() + 2, y() + Config::font_size::track_status);
    SymbolTable::get()->draw(this->text, p, style);
}
