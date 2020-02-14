// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <string.h>

#include <FL/fl_draw.H>
#include <FL/Fl.H>
#include <FL/x.H> // Needed for Fl_Offscreen.

#include "f_util.h"
#include "utf8.h"
#include "util.h"

#include "SymbolTable.h"

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
    std::string sname(name);
#ifdef __linux__
    // fltk on linux does this weird scheme where it prepends ' ' for normal,
    // 'B' for bold, 'I' for italic, and 'P' for bold+italic.
    sname.insert(0, " ");
#endif
    std::map<string, Font>::const_iterator it = font_map.find(sname);
    if (it == font_map.end()) {
        DEBUG("font not found: '" << name << "'");
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
    *cur = nullptr;
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
draw_text(const char *text, int len, IPoint pos, bool measure,
    DPoint align = DPoint(), int rotate = 0)
{
    if (len == 0)
        return 0;

    pos = pos + IPoint(align.x * fl_size(), align.y * fl_size());
    if (!measure) {
        fl_draw(rotate, text, len, pos.x, pos.y);
        util::timing(2, "fl_draw");
    }
    return fl_width(text, len);
}

static void
set_font(const SymbolTable::Glyph &glyph, SymbolTable::Size size)
{
    // Make sure font_not_found doesn't creep in.
    ASSERT(glyph.font >= 0);
    fl_font(glyph.font, size + glyph.size);
}


// Draw a Symbol.
static void
draw_symbol(IPoint pos, const SymbolTable::Symbol &sym, SymbolTable::Size size,
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


DPoint
SymbolTable::draw(const string &text, IPoint pos, Style style) const
{
    return draw_or_measure(text, 0, text.length(), pos, style, false);
}


DPoint
SymbolTable::measure(const string &text, size_t start, size_t end, Style style)
    const
{
    return draw_or_measure(text, start, end, IPoint(0, 0), style, true);
}


static SymbolTable::ParsedSymbol
parse_symbol(const std::string &text)
{
    using ParsedSymbol = SymbolTable::ParsedSymbol;

    const char *divider = strchr(text.c_str(), '/');
    // DEBUG("parse " << text);
    if (divider == nullptr) {
        return ParsedSymbol(text);
    } else if (divider == text.c_str()) {
        return ParsedSymbol(divider + 1);
    } else if (text[0] == '+' || text[0] == '-') {
        char *end;
        int size = strtol(text.c_str(), &end, 10);
        if (end != divider) {
            return ParsedSymbol(text);
        } else {
            // DEBUG("found " << (divider + 1) << ": " << size);
            return ParsedSymbol(divider + 1, size, 0);
        }
    } else {
        static const std::vector<std::pair<const char *, Fl_Font>> table =
            { { "bold", FL_BOLD }
            , { "italic", FL_ITALIC }
            , { "bold+italic", FL_BOLD | FL_ITALIC }
            };
        int len = divider - text.c_str();
        for (const auto &p : table) {
            if (strncmp(text.c_str(), p.first, len) == 0) {
                // DEBUG("found " << (divider + 1) << ": " << p.second);
                return ParsedSymbol(divider + 1, 0, p.second);
            }
        }
    }
    return ParsedSymbol(text);
}


DPoint
SymbolTable::draw_backticks(
    const std::string &text, IPoint pos, const Style &style, bool measure)
    const
{
    ParsedSymbol parsed(parse_symbol(text));

    if (parsed.size != 0 || parsed.attributes != 0) {
        fl_font(fl_font() + parsed.attributes, fl_size() + parsed.size);
        double width = draw_text(
            parsed.text.c_str(), parsed.text.size(), pos, measure, DPoint());
        return DPoint(width, fl_height() - fl_descent());
    } else {
        SymbolMap::const_iterator it = this->symbol_map.find(parsed.text);

        if (it == symbol_map.end()) {
            // Unknown symbol, draw it as plain text including the ``s.
            double width = draw_text(
                parsed.text.c_str(), parsed.text.size(), pos, measure,
                DPoint());
            return DPoint(width, fl_height() - fl_descent());
        } else {
            // Draw symbol inside ``s.
            IRect sym_box = this->measure_symbol(it->second, style.size);
            // The box measures the actual bounding box of the symbol.  Clip
            // out the spacing inserted by the characters by translating back
            // by the box's offsets.
            if (!measure) {
                draw_symbol(
                    IPoint(pos.x - sym_box.x, pos.y + sym_box.y),
                    it->second, style.size, 0);
            }
            return DPoint(sym_box.w, sym_box.h);
        }
    }
}


DPoint
SymbolTable::draw_or_measure(const string &text, size_t start, size_t end,
    IPoint pos, Style style, bool measure) const
{
    size_t i, j;

    style.set();
    // Keep track of the current bounding box.  The box is the width and height
    // of the text.
    DPoint box(0, fl_height() - fl_descent());

    while ((i = text.find('`', start)) < end) {
        i++;
        j = text.find('`', i);
        if (j >= end)
            break;

        // Draw text before ``s.
        style.set();
        box.x += draw_text(
            text.c_str() + start, i-start-1, IPoint(pos.x + box.x, pos.y),
            measure, DPoint());

        DPoint sym_box = draw_backticks(
            text.substr(i, j-i), IPoint(pos.x + box.x, pos.y), style, measure);
        box.x += sym_box.x;
        box.y = std::max(box.y, sym_box.y);
        start = j + 1;
    }
    // Draw trailing text.
    style.set();
    if (start < end) {
        box.x += draw_text(text.c_str() + start, end - start,
            IPoint(pos.x + box.x, pos.y), measure, DPoint());
    }
    return box;
}


// Measure the width of the symbol between backticks, or -1 if it's not
// actually a symbol.
//
// TODO It's unpleasant how this is somewhat duplicated with the code in
// draw(), maybe they can be integrated better.
int
SymbolTable::measure_backticks(const char *text, Size size) const
{
    text++;
    const char *start = text;
    while (*text && *text != '`')
        text++;
    if (!*text) {
        // Oops, it was unclosed.
        return -1;
    } else {
        SymbolMap::const_iterator it = this->symbol_map.find(
            string(start, text-start));
        if (it == symbol_map.end()) {
            return -1;
        } else {
            return this->measure_symbol(it->second, size).w;
        }
    }
}


// Get the width of a character, or a `symbol`.
double
SymbolTable::measure_glyph(const char *p, int size) const
{
    if (*p == '`') {
        double width = this->measure_backticks(p, size);
        if (width == -1)
            return utf8::width(p);
        else
            return width;
    } else if (*p) {
        // fl_width seems to under-report widths by a small amount on
        // retina displays.
        return utf8::width(p) + 1;
    } else {
        return 0;
    }
}


static size_t
next_split(const string &text, size_t start)
{
    size_t i = start;
    while (i < text.length() && text[i] == ' ')
        i++;
    while (i < text.length() && text[i] != ' ')
        i++;
    return i;
}


static size_t
next_symbol(const string &text, size_t start)
{
    if (text[start] == '`') {
        size_t end = start + 1;
        while (end < text.length() && text[end] != '`')
            end++;
        if (end == text.length()) { // unclosed `
            return start + 1;
        } else {
            return end + 1;
        }
    } else {
        return start
            + utf8::bytes(text.c_str() + start, text.length() - start, 1);
    }
}

SymbolTable::Wrapped
SymbolTable::wrap(const string &text, const Style &style, int wrap_width) const
{
    std::vector<std::pair<string, DPoint>> lines;
    string line;
    line.reserve(text.length());

    // Otherwise glyphs will overlap the edge a bit anyway.
    wrap_width--;

    size_t start = 0;
    DPoint line_box(0, 0);
    // DEBUG("text: " << text << " (" << text.length() << ")");
    while (start < text.length()) {
        size_t end = next_split(text, start);
        DPoint word_box = this->measure(text, start, end, style);
        if (line_box.x + word_box.x <= wrap_width) {
            // DEBUG(start << "--" << end << ": " << word_box << " <= "
            //     << wrap_width);
            line.append(text, start, end - start);
            line_box.x += word_box.x;
            line_box.y = std::max(line_box.y, word_box.y);
            start = end;
        } else {
            // I exceeded the width, so I have to break before this word.
            // DEBUG(start << "--" << end << ": " << word_box << " > "
            //     << wrap_width << ", line: " << line);
            if (line.empty()) {
                // A single word doesn't fit, so split by glyph.
                int end;
                line_box = wrap_glyphs(text, start, style, wrap_width, &end);
                // DEBUG("wrap_glyphs: " << start << "--" << end << " box: "
                //     << line_box);
                line.append(text, start, end - start);
                start = end;
            }
            // DEBUG("text '" << line << "' sum " << line_box << " recalc "
            //     << measure(line, 0, line.length(), style));
            lines.push_back(std::make_pair(line, line_box));
            line_box = DPoint(0, 0);
            line.clear();
            // I just broke a line, so I can skip all the whitespace.
            while (text[start] == ' ')
                start++;
        }
    }
    if (!line.empty()) {
        // DEBUG("FINAL text '" << line << "' sum " << line_box << " recalc "
        //     << measure(line, 0, line.length(), style));
        lines.push_back(std::make_pair(line, line_box));
    }

    // for (int i = 0; i < lines.size(); i++) {
    //     DEBUG("LINE " << i << ": " << lines[i]);
    // }
    return lines;
}


DPoint
SymbolTable::wrap_glyphs(const string &text, int start, const Style &style,
    int wrap_width, int *wrap_at) const
{
    // Always include at least one symbol, otherwise I could loop forever.
    size_t end = next_symbol(text, start);
    DPoint prev_box = this->measure(text, start, end, style);
    DPoint box = prev_box;
    for (size_t prev_end = end; end < text.length();
            prev_end = end, prev_box = box) {
        end = next_symbol(text, end);
        box = this->measure(text, start, end, style);
        // DEBUG(IPoint(start, end)
        //     << "'" << text.substr(start, end - start) << "'"
        //     << ": " << box);
        if (box.x > wrap_width) {
            end = prev_end;
            box = prev_box;
            break;
        }
    }
    *wrap_at = end;
    return box;
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
    draw_symbol(IPoint(size, size*2 - 1), sym, size, 0);
    unsigned char *buf = fl_read_image(nullptr, 0, 0, w, h);
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
