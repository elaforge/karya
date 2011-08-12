#include <string.h>
#include <stdlib.h>

#include <FL/Fl_Box.H>

#include "SymbolOutput.h"
#include "SymbolTable.h"


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
    Fl_Box::draw();
    IPoint p(x() + 2, y() + Config::font_size::track_status);
    SymbolTable::get()->draw(this->text, p,
        Config::font, Config::font_size::track_status, FL_BLACK);
}
