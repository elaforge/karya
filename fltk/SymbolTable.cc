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
        return Config::font;
    std::map<string, Font>::const_iterator it = font_map.find(string(name));
    if (it == font_map.end()) {
        DEBUG("font not found: " << name);
        return font_not_found;
    } else
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
        // Clear it out of the box_cache.
        for (CacheMap::iterator cache = box_cache.begin();
            cache != box_cache.end();)
        {
            // Trickiness to avoid incrementing an erased iterator.
            if (cache->first.first == &it->second)
                box_cache.erase(cache++);
            else
                ++cache;
        }
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
    DPoint align = DPoint(), int rotate = 0)
{
    if (n == 0)
        return 0;
    // DEBUG("draw_text " << n << ": " << string(text, n) << " " << pos);

    pos = pos + IPoint(align.x * fl_size(), align.y * fl_size());
    if (!measure)
        fl_draw(rotate, text, n, pos.x, pos.y);
    return fl_width(text, n);
}

static void
set_font(const SymbolTable::Glyph &glyph, SymbolTable::Size size)
{
    // Make sure font_not_found doesn't creep in.
    ASSERT(glyph.font >= 0);
    fl_font(glyph.font, size + glyph.size);
}


// Draw a group of glyphs.
static void
draw_glyphs(IPoint pos, const SymbolTable::Symbol &sym, SymbolTable::Size size,
    int rotate)
{
    for (std::vector<SymbolTable::Glyph>::const_iterator
            glyph = sym.glyphs.begin(); glyph != sym.glyphs.end(); ++glyph)
    {
        set_font(*glyph, size);
        draw_text(glyph->utf8, strlen(glyph->utf8), pos, false,
            DPoint(glyph->align_x, glyph->align_y), glyph->rotate + rotate);
    }
}


IPoint
SymbolTable::draw(const string &text, IPoint pos, Font font, Size size,
        Fl_Color color, bool vertical, bool measure) const
{
    size_t start = 0;
    size_t i, j;

    fl_font(font, size);
    fl_color(color);
    // Keep track of the current bounding box.  The box is the width and height
    // of the text, so if vertical is true, the box's 'x' is the vertical
    // "width" of the text.
    IPoint box(0, fl_height() - fl_descent());
    int rotate = vertical ? -90 : 0;

    while ((i = text.find('`', start)) < text.size()) {
        i++;
        j = text.find('`', i);
        if (j >= text.size())
            break;

        // Draw text before ``s.
        fl_font(font, size);
        box.x += draw_text(text.c_str() + start, i-start-1,
            vertical ? IPoint(pos.x, pos.y + box.x)
                : IPoint(pos.x + box.x, pos.y),
            measure, DPoint(), rotate);

        SymbolMap::const_iterator it =
            this->symbol_map.find(text.substr(i, j-i));
        if (it == symbol_map.end()) {
            // Unknown symbol, draw it as plain text including the ``s.
            box.x += draw_text(text.c_str() + i - 1, j-i + 2,
                vertical ? IPoint(pos.x, pos.y + box.x)
                    : IPoint(pos.x + box.x, pos.y),
                measure, DPoint(), rotate);
        } else {
            // Draw symbol inside ``s.
            IRect sym_box = this->measure_symbol(it->second, size);
            // The box measures the actual bounding box of the symbol.  Clip
            // out the spacing inserted by the characters by translating back
            // by the box's offsets.
            // DEBUG("draw " << text << " sym " << sym_box << ", pos " << pos
            //         << " -> " << IPoint(pos.x + box.x - sym_box.x,
            //           pos.y + sym_box.y));
            if (!measure) {
                draw_glyphs(
                    vertical ? IPoint(pos.x, pos.y + box.x)
                        : IPoint(pos.x + box.x - sym_box.x, pos.y + sym_box.y),
                    it->second, size, rotate);
            }
            box.x += sym_box.w;
            box.y = std::max(box.y, sym_box.h);
        }
        start = j + 1;
    }
    // Draw trailing text.
    fl_font(font, size);
    if (start < text.size()) {
        box.x += draw_text(text.c_str() + start, text.size() - start,
            vertical ? IPoint(pos.x, pos.y + box.x)
                : IPoint(pos.x + box.x, pos.y),
            measure, DPoint(), rotate);
    }
    return box;
}


IPoint
SymbolTable::measure(const string &text, Font font, Size size) const
{
    return this->draw(text, IPoint(0, 0), font, size, FL_BLACK, false, true);
}


IPoint
SymbolTable::draw_wrapped(const string &text, IPoint pos,
    int wrap_width, Font font, Size size, Fl_Color color, bool measure) const
{
    // This function is a real rat's nest.
    IPoint total_size(0, 0);
    const char *line_start = text.c_str();
    const char *last_space = line_start;
    // Keep track of the height of the line up to the last space.  If the line
    // needs to wrap at last_space, I'll want to shift down by that much.
    IPoint last_size(0, 0);

    for (const char *p = line_start;; p++) {
        // The end of the string is also like a space.
        if (!*p || *p == ' ') {
            // Size up to 'p'.
            IPoint p_size = this->measure(
                string(line_start, p - line_start), font, size);
            // DEBUG("measure " << string(line_start, p-line_start) << ": "
            //     << p-line_start << ' ' << line_size);
            const bool should_wrap = p_size.x > wrap_width;
            if (!*p || should_wrap) {
                // Draw up to the last space, but if  I'm out of letters and
                // don't actually have to break, or can't break because there
                // are no spaces, then draw up to here.
                const char *break_at;
                IPoint line_size;
                if (!should_wrap || last_space == line_start) {
                    break_at = p;
                    line_size = p_size;
                } else {
                    break_at = last_space;
                    line_size = last_size;
                }
                // DEBUG("break_at: " << break_at - line_start << " p "
                //     << (*p ? *p : '_'));
                total_size.x = std::max(total_size.x, line_size.x);
                total_size.y += line_size.y;
                if (!measure) {
                    IPoint line_pos(pos.x, pos.y + total_size.y);
                    this->draw(
                        string(line_start, break_at - line_start), line_pos,
                        font, size, color);
                }
                if (!*break_at) {
                    break;
                } else {
                    p = break_at; // for()'s p++ will increment past the space.
                    line_start = p + 1;
                    last_space = line_start;
                    last_size = line_size;
                }
            } else {
                last_space = p;
                last_size = p_size;
            }
        } else if (*p == '`') {
            // A glyph is considered one letter, so skip to the next `.
            p++;
            while (*p && *p != '`')
                p++;
            if (!*p)
                p--;
        }
    }
    return total_size;
}


IPoint
SymbolTable::measure_wrapped(const string &text, IPoint pos,
    int wrap_width, Font font, Size size) const
{
    return draw_wrapped(text, pos, wrap_width, font, size, FL_BLACK, true);
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

    // printf("wh: %d %d\n", w, h);
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
            // printf("%02hhx", std::min(std::min(p[0], p[1]), p[2]));
            if (start == -1) {
                if (!white(p))
                    start = col;
            }
            if (!white(p) && (col + 1 == w || white(p+3))) {
                end = col;
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

enum {
    text_pad_left = 2,
    text_pad_right = 2,
    text_pad_top = 0,
    text_pad_bottom = 0
};

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

    // Due to boundary issues, drawing text that touches the bottom of a box
    // means drawing one above the bottom.  I don't totally understand this.
    draw_glyphs(IPoint(size, size*2 - 1), sym, size, 0);
    unsigned char *buf = fl_read_image(NULL, 0, 0, w, h);
    fl_end_offscreen();
    IRect box = find_box(buf, w, h);
    delete[] buf;
    fl_delete_offscreen(screen);

    // Clip the extra spacing back off.  If the symbol extends before or above
    // the insertion point, this will be negative, meaning it should be shifted
    // forward.
    box.x -= size;
    // Since this measure the pixels directly, it doesn't understand the glyph
    // baseline.  So glyphs that stick down a little will be out of line with
    // non-symbol text.  If it looks really ugly, I can disable vertical
    // placement for specific symbols by setting box.y = 0.
    if (sym.absolute_y) {
        box.y = 0;
    } else {
        // The box is the bounding box of the glyph, but I want the amounts
        // to translate the glyph to place it nicely.  Since I drew the glyph
        // at a baseline of size*2, the box tells me how far from that
        // baseline it actually appears.
        box.y = size*2 - box.b();
        box.y -= text_pad_bottom;
        box.h += text_pad_bottom + text_pad_top;
    }
    box.x -= text_pad_left;
    box.w += text_pad_left + text_pad_right;

    // DEBUG("translate " << box);
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
SymbolTable::get()
{
    static SymbolTable table = SymbolTable();
    return &table;
}
