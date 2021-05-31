// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

#include <map>
#include <string>
#include <unordered_map>
#include <utility>

#include <FL/fl_draw.H>

#include "config.h"
#include "global.h"


// This class is responsible for drawing text, mapping specially quoted
// sequences to Symbols, which are a composition of glyphs from some font.
//
// The class should be accessed through a single global instance.
class SymbolTable {
    SymbolTable();
public:
    typedef Fl_Font Font;
    typedef int Size;
    struct Style {
        Style(Font font, Size size, Fl_Color color)
            : font(font), size(size), color(color) {}
        Font font;
        Size size;
        Fl_Color color;

        void set() const {
            fl_font(font, size);
            fl_color(color);
        }
    };

    enum { font_not_found = -1 };

    // A glyph is an indivisible graphic made from a string, font, and size.
    struct Glyph {
        Glyph(const char *utf8, Font font = Config::font, Size size = 0,
                geom::DPoint align = DPoint(), int rotate = 0) :
            utf8(utf8), font(font), size(size),
            align_x(align.x), align_y(align.y), rotate(rotate)
        {}
        // True if the font was font_not_found, which means you can't draw this
        // Glyph.
        bool invalid() const { return font < 0; }
        // This is owned by the glyph and must be freed explicitly.
        const char *utf8;
        Font font;
        // This is a size delta, that will be added to the overall font size.
        Size size;
        // Move the glyph by the alignment scaled by the size.
        // This would be a DPoint but haskell likes simple types.
        double align_x, align_y;
        // Rotate the glyph by degrees.
        int rotate;
    };

    // A Symbol is a group of glyphs.
    struct Symbol {
        Symbol() : absolute_y(false) {}
        Symbol(const Glyph &g1) {
            Symbol();
            glyphs.push_back(g1);
        }
        Symbol(const Glyph &g1, const Glyph &g2) {
            Symbol();
            glyphs.push_back(g1);
            glyphs.push_back(g2);
        }
        Symbol(const Glyph &g1, const Glyph &g2, const Glyph &g3) {
            Symbol();
            glyphs.push_back(g1);
            glyphs.push_back(g2);
            glyphs.push_back(g3);
        }
        // SymbolTable::insert will skip any invalid Glyph.
        bool invalid() const {
            for (const Glyph &g : glyphs) {
                if (g.invalid())
                    return true;
            }
            return false;
        }
        // Turn off automatic y placement.  If the glyphs have descenders and
        // you want them to actually descend, turn this on.
        bool absolute_y;
        std::vector<Glyph> glyphs;
    };

    // `+3/abc` => ParsedSymbol("abc", 3, 0)
    // `bold/abc` => ParsedSymbol("abc", 0, FL_BOLD)
    struct ParsedSymbol {
        ParsedSymbol(std::string text, Size size = 0,
                Fl_Font attributes = 0)
            : text(text), size(size), attributes(attributes)
        {}
        std::string text;
        Size size;
        Fl_Font attributes; // 0, FL_BOLD, FL_ITALIC, FL_BOLD | FL_ITALIC
    };

    // Convert a font name into a Font.  NULL means Config::font.
    // This will return font_not_found if 'name' is not valid.
    Font font(const char *name) const;

    // Return all loaded fonts as a null-terminated array.  The caller is
    // responsible for freeing both the array and the strings.
    char **fonts() const;

    // Create a name -> Symbol association.  An existing one will be replaced.
    void insert(const std::string &name, const Symbol &sym);

    // Wrapped words, as [(Line, BoundingBox)].
    typedef std::vector<std::pair<std::string, DPoint>> Wrapped;
    Wrapped wrap(const std::string &text, const Style &style, int wrap_width)
        const;

    // Draw the text, rendering `` symbols in their proper font.  Symbols that
    // are not found are drawn as normal text.
    //
    // Return the bounding box of the symbols that were drawn, or would have
    // been drawn if measure is true.  The bounding box includes ascenders but
    // doesn't include descenders, because those can generally overlap.
    DPoint draw(const std::string &text, IPoint pos, Style style) const;
    // Like 'draw' but just measure, don't actually draw.
    DPoint measure(const std::string &text, size_t start, size_t end,
        Style style) const;

    // Measure the Symbol by actually drawing it and seeing how many pixels it
    // occupies.  This is expensive so it's cached.
    //
    // If this is called before the window is shown, it will crash horribly.
    IRect measure_symbol(const Symbol &sym, Size size) const;

    static SymbolTable *get();
private:
    DPoint draw_or_measure(
        const std::string &text, size_t start, size_t end, IPoint pos,
        Style style, bool measure) const;
    DPoint draw_backticks(
        const std::string &text, IPoint pos, const Style &style, bool measure)
            const;
    int measure_backticks(const char *text, Size size) const;
    double measure_glyph(const char *p, int size) const;

    DPoint wrap_glyphs(const std::string &text, int start, const Style &style,
        int wrap_width, int *wrap_at) const;

    typedef std::unordered_map<std::string, Symbol> SymbolMap;
    SymbolMap symbol_map;
    std::unordered_map<std::string, Font> font_map;

    typedef std::pair<const Symbol *, Size> CacheKey;
    typedef std::map<const CacheKey, IRect> CacheMap;
    // Cache the exact dimensions of the glyphs since the calculation process
    // is gross and manual.
    mutable CacheMap box_cache;
};
