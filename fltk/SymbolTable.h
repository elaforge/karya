#ifndef __SYMBOL_TABLE_H
#define __SYMBOL_TABLE_H

#include <map>
#include <string>
#include <utility>

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

    enum { font_not_found = -1 };

    struct Glyph {
        Glyph(const char *utf8, Font font = Config::font, Size size = 0,
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
        Symbol() {}
        Symbol(const Glyph &g1) {
            glyphs.push_back(g1);
        }
        Symbol(const Glyph &g1, const Glyph &g2) {
            glyphs.push_back(g1);
            glyphs.push_back(g2);
        }
        Symbol(const Glyph &g1, const Glyph &g2, const Glyph &g3) {
            glyphs.push_back(g1);
            glyphs.push_back(g2);
            glyphs.push_back(g3);
        }
        std::vector<Glyph> glyphs;
    };

    // Convert a font name into a Font.  NULL means Config::font.
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
    IPoint draw(const string &text, IPoint pos, Font font, Size size,
        bool measure = false) const;

    // Specialization for 'draw' that just measures.
    IPoint measure(const string &text, Font font, Size size) const;

    // Measure the Symbol by actually drawing it and seeing how many pixels it
    // occupies.  This is expensive so it's cached.
    //
    // If this is called before the window is shown, it will crash horribly.
    IRect measure_symbol(const Symbol &sym, Size size) const;

    static SymbolTable *table();
private:
    typedef std::map<string, Symbol> SymbolMap;
    SymbolMap symbol_map;
    std::map<string, Font> font_map;

    typedef std::pair<const Symbol *, Size> CacheKey;
    mutable std::map<const CacheKey, IRect> box_cache;
};

#endif
