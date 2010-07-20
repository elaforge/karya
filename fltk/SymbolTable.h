#ifndef __SYMBOL_TABLE_H
#define __SYMBOL_TABLE_H

#include <map>
#include <string>

#include <FL/fl_draw.H>

#include "config.h"


using std::string;


// This class is responsible for drawing text, mapping specially quoted sequences
// to Symbols, which are a composition of glyphs from some font.
//
// The class should be accessed through a single global instance.
class SymbolTable {
    SymbolTable();
public:
    typedef Fl_Font Font;
    typedef int Size;

    // font_default means use the same font used for normal letters.
    enum { font_default = -1, font_not_found = -2};

    struct Glyph {
        Glyph(const char *utf8, Font font = font_default, Size size = 0,
                DPoint align = DPoint()) :
            utf8(utf8), font(font), size(size), align_x(align.x), align_y(align.y)
        {}
        // This is owned by the glyph and must be freed explicitly.
        const char *utf8;
        Font font;
        // This is a size delta, that will be added to the overall font size.
        Size size;
        // Move the glyph by the alignment scaled by the size.
        // This would be a DPoint but haskell likes simple types.
        double align_x, align_y;
    };
    struct Symbol {
        Symbol(DPoint box) : box(box) {}
        // A bounding box for the symbol, scaled by its size.  If it's 0, use
        // the box of the first glyph.
        DPoint box;
        std::vector<Glyph> glyphs;
    };

    // A few convenience Symbol constructors.
    static Symbol simple(const Glyph &glyph1) {
        Symbol sym(DPoint(0, 0));
        sym.glyphs.push_back(glyph1);
        return sym;
    }

    static Symbol symbol(DPoint box, const Glyph &glyph1, const Glyph &glyph2) {
        Symbol sym(box);
        sym.glyphs.push_back(glyph1);
        sym.glyphs.push_back(glyph2);
        return sym;
    }

    static Symbol symbol(DPoint box, const Glyph &glyph1, const Glyph &glyph2,
            const Glyph &glyph3)
    {
        Symbol sym(box);
        sym.glyphs.push_back(glyph1);
        sym.glyphs.push_back(glyph2);
        sym.glyphs.push_back(glyph3);
        return sym;
    }

    // Convert a font name into a Font.  NULL means font_default.
    // This will return font_not_found if 'name' is not valid.
    Font font(const char *name) const;

    // Return all loaded fonts as a null-terminated array.  The caller is
    // responsible for freeing both the array and the strings.
    char **fonts() const;

    // Create a name -> Symbol association.  An existing one will be replaced.
    void insert(const string &name, const Symbol &sym);

    // Draw the text, rendering `` symbols in their proper font.  Symbols that
    // are not found are drawn as normal text.
    //
    // Return the bounding box of the symbols that were drawn, or would have been
    // drawn if measure is true.  The bounding box includes ascenders but doesn't
    // include descenders, because those can generally overlap.
    Point draw(const string &text, Point pos, Font font, Size size,
        bool measure = false) const;

    // Specialization for 'draw' that just measures.
    Point measure(const string &text, Font font, Size size) const;

    static SymbolTable *table();
private:
    typedef std::map<string, Symbol> SymbolMap;
    SymbolMap symbol_map;
    std::map<string, Font> font_map;
};

#endif
