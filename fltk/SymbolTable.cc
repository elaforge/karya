#include <FL/fl_draw.H>
#include <FL/Fl.H>
#include <FL/x.H> // Needed for Fl_Offscreen.

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


// Draw a group of glyphs.
static void
draw_glyphs(IPoint pos, const SymbolTable::Symbol &sym, SymbolTable::Size size)
{
    for (std::vector<SymbolTable::Glyph>::const_iterator
            glyph = sym.glyphs.begin(); glyph != sym.glyphs.end(); ++glyph)
    {
        set_font(*glyph, size);
        draw_text(glyph->utf8, strlen(glyph->utf8), pos, false,
            DPoint(glyph->align_x, glyph->align_y));
    }
}


IPoint
SymbolTable::draw(const string &text, IPoint pos, Font font, Size size,
        bool measure) const
{
    size_t start = 0;
    size_t i, j;

    fl_font(font, size);
    // Keep track of the current bounding box.
    IPoint box(0, fl_height() - fl_descent() + 1);

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
            IRect sym_box = this->measure_symbol(it->second, size);
            // The box measures the actual bounding box of the symbol.  Clip
            // out the spacing inserted by the characters by translating back by
            // the box's offsets.
            if (!measure) {
                draw_glyphs(
                    IPoint(pos.x + box.x - sym_box.x, pos.y - sym_box.y),
                    it->second, size);
            }
            box.x += sym_box.w;
            box.y = std::max(box.y, sym_box.h);
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
    return this->draw(text, IPoint(0, 0), font, size, true);
}


static bool
white(const unsigned char *p)
{
    return p[0] == 255 && p[1] == 255 && p[2] == 255;
}

static IRect
find_box(const unsigned char *buf, int w, int h)
{
    IPoint ul(w, h);
    IPoint lr(0, 0);
    int line_start = -1, line_end = -1;
    bool previous_white = true;

    // printf("   ");
    // for (int i = 0; i < w; i++) {
    //     printf("%02d", i);
    // }
    // printf("\n");
    for (int line = 0; line < h; line++) {
        int start = -1, end = -1;
        // printf("%02d:", line);
        for (int col = 0; col < w; col++) {
            const unsigned char *p = buf + (line*w + col) * 3;
            // printf("%02hhx", p[0]);
            if (start == -1) {
                if (!white(p))
                    start = col;
            }
            if (!white(p) && (col + 1 == w || white(p+3))) {
                end = col + 1;
            }
        }
        // printf("\n");
        bool white_line = start == -1 && end == -1;
        if (line_start == -1) {
            if (!white_line)
                line_start = line;
        }
        if (white_line && !previous_white) {
            line_end = line;
        }
        previous_white = white_line;

        if (white_line)
            continue; // all white
        else if (start == -1)
            start = 0;
        else if (end == -1)
            end = w;

        ul.x = std::min(ul.x, start);
        lr.x = std::max(lr.x, end);
    }
    if (line_start == -1)
        line_start = 0;
    if (line_end == -1)
        line_end = h;

    ul.y = line_start;
    lr.y = line_end;
    // DEBUG("rect " << ul << " -- " << lr);
    return IRect(ul.x, ul.y, lr.x - ul.x, lr.y - ul.y);
}

static IRect
do_measure_symbol(const SymbolTable::Symbol &sym, SymbolTable::Size size)
{
    // I don't bother to guess how big it will be, so give it plenty of
    // room on all sides.
    const int w = size*3;
    const int h = size*3;
    Fl_Offscreen screen = fl_create_offscreen(w, h);
    fl_begin_offscreen(screen);
    fl_color(FL_WHITE);
    fl_rectf(-1, -1, w+2, h+2);
    fl_color(FL_BLACK);
    draw_glyphs(IPoint(size, size*2), sym, size);
    unsigned char *buf = fl_read_image(NULL, 0, 0, w, h);
    fl_end_offscreen();
    IRect box = find_box(buf, w, h);
    delete[] buf;
    fl_delete_offscreen(screen);

    // Clip the extra spacing back off.  If the symbol extends before or above
    // the insertion point, this will be negative, meaning it should be shifted
    // forward.
    box.x -= size + 2;
    box.y -= size;
    // Leave some space around it.
    box.w += 3;
    return box;
}

IRect
SymbolTable::measure_symbol(const Symbol &sym, Size size) const
{
    std::map<const CacheKey, IRect>::iterator it =
        this->box_cache.find(std::make_pair(&sym, size));
    if (it == box_cache.end()) {
        IRect box = do_measure_symbol(sym, size);
        box_cache.insert(std::make_pair(std::make_pair(&sym, size), box));
        return box;
    } else {
        return it->second;
    }
}


SymbolTable *
SymbolTable::table()
{
    static SymbolTable *table = new SymbolTable();
    return table;
}
