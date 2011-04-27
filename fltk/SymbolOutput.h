#ifndef __SYMBOL_OUTPUT_H
#define __SYMBOL_OUTPUT_H

#include <stdlib.h>
#include <string>

#include <FL/FL_Box.H>


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
    void draw();
private:
    std::string text;
};

#endif
