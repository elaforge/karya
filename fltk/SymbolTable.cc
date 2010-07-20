#include <FL/fl_draw.H>
#include <FL/Fl.H>

#include "SymbolTable.h"
#include "util.h"

using std::string;


SymbolTable::SymbolTable()
{
    int nfonts = Fl::set_fonts();
    for (int i = 0; i < nfonts; i++) {
        this->font_map.insert(std::make_pair(string(Fl::get_font(i)), i));
    }
}


Fl_Font
SymbolTable::font(const char *name) const
{
    if (!name)
        return font_default;
    std::map<string, Font>::const_iterator it = font_map.find(string(name));
    if (it == font_map.end())
        return font_not_found;
    else
        return it->second;
}


char **
SymbolTable::fonts() const
{
    char **fonts = (char **) calloc(font_map.size() + 1, sizeof(char *));
    char **cur = fonts;
    std::map<string, Font>::const_iterator font = font_map.begin();
    for (; font != font_map.end(); ++font) {
        *cur++ = strdup(font->first.c_str());
    }
    *cur = NULL;
    return fonts;
}

void
SymbolTable::insert(const string &name, const Symbol &sym)
{
    SymbolMap::iterator it = this->symbol_map.find(name);
    if (it != symbol_map.end()) {
        for (size_t i = 0; i < it->second.glyphs.size(); i++) {
            free(const_cast<char *>(it->second.glyphs[i].utf8));
        }
        symbol_map.erase(it);
    }
    symbol_map.insert(std::make_pair(name, sym));
}


// Draw the given text and return its width.
static double
draw_text(const char *text, int n, IPoint pos, bool measure,
    DPoint align = DPoint(0, 0))
{
    if (n == 0)
        return 0;
    // DEBUG("draw_text " << n << ": " << string(text, n) << " " << pos);

    pos = pos + IPoint(align.x * fl_size(), align.y * fl_size());
    if (!measure)
        fl_draw(text, n, pos.x, pos.y);
    return fl_width(text, n);
}

static void
set_font(const SymbolTable::Glyph &glyph, SymbolTable::Size size)
{
    // Make sure font_not_found doesn't creep in.
    ASSERT(glyph.font == SymbolTable::font_default || glyph.font >= 0);
    fl_font(glyph.font == SymbolTable::font_default ? fl_font() : glyph.font,
        size + glyph.size);
}


// Draw a group of glyphs and return their width.
static double
draw_glyphs(IPoint pos, const SymbolTable::Symbol &sym, SymbolTable::Size size)
{
    double w = 0;
    for (std::vector<SymbolTable::Glyph>::const_iterator
            glyph = sym.glyphs.begin(); glyph != sym.glyphs.end(); ++glyph)
    {
        set_font(*glyph, size);
        w += draw_text(glyph->utf8, strlen(glyph->utf8), pos, false,
            DPoint(glyph->align_x, glyph->align_y));
    }
    return w;
}


// Measure a group of glyphs and return their bounding box.
//
// If the Symbol has an explicit box set, use that, otherwise take the box from
// the first glyph.
static IPoint
measure_glyphs(const SymbolTable::Symbol &sym, SymbolTable::Size size)
{
    IPoint box;
    if (sym.box == DPoint(0, 0) && sym.glyphs.size() > 0) {
        const SymbolTable::Glyph &glyph = sym.glyphs[0];
        // Figure out the box automatically from the first glyph.
        set_font(glyph, size);
        box.x = fl_width(glyph.utf8);
        box.y = fl_height() - fl_descent();
    } else {
        box.x = size * sym.box.x;
        box.y = size * sym.box.y;
    }
    return box;
}


IPoint
SymbolTable::draw(const string &text, IPoint pos, Font font, Size size,
        bool measure) const
{
    size_t start = 0;
    size_t i, j;

    fl_font(font, size);
    // Keep track of the current bounding box.
    IPoint box(0, fl_height() - fl_descent());

    while ((i = text.find('`', start)) < text.size()) {
        i++;
        j = text.find('`', i);
        if (j >= text.size())
            break;

        fl_font(font, size);
        box.x += draw_text(text.c_str() + start, i-start-1,
            IPoint(pos.x + box.x, pos.y), measure);

        SymbolMap::const_iterator it = this->symbol_map.find(text.substr(i, j-i));
        if (it == symbol_map.end()) {
            box.x += draw_text(text.c_str() + i - 1, j-i + 2,
                IPoint(pos.x + box.x, pos.y), measure);
        } else {
            if (measure) {
                IPoint glyphs_box = measure_glyphs(it->second, size);
                box.x += glyphs_box.x;
                box.y = std::max(box.y, glyphs_box.y);
            } else {
                box.x += draw_glyphs(
                    IPoint(pos.x + box.x, pos.y), it->second, size);
            }
        }
        start = j + 1;
    }
    fl_font(font, size);
    if (start < text.size()) {
        box.x += draw_text(text.c_str() + start, text.size() - start,
            IPoint(pos.x + box.x, pos.y), measure);
    }
    return box;
}


IPoint
SymbolTable::measure(const string &text, Font font, Size size) const
{
    return this->draw(text, Point(0, 0), font, size, true);
}


SymbolTable *
SymbolTable::table()
{
    static SymbolTable *table = new SymbolTable();
    return table;
}
