// Copyright 2016 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt


#include "alpha_draw.h"
#include "SymbolTable.h"

#include "RulerOverlay.h"


// Marklist

void
Marklist::incref()
{
    ASSERT(references > 0);
    references++;
    // DEBUG("incref " << length << ": " << references);
}

void
Marklist::decref()
{
    ASSERT(references > 0);
    references--;
    // DEBUG("decref " << length << " : " << references);
    if (references == 0) {
        for (int i = 0; i < length; i++) {
            if (marks[i].mark.name)
                free(marks[i].mark.name);
        }
        free((void *) marks);
        // Make sure if someone uses it they get a segfault right away.
        marks = nullptr;
    }
}


// RulerOverlay

RulerOverlay::RulerOverlay(const RulerConfig &config, bool is_ruler_track) :
    config(config)
{
    // No matter what the config, ruler tracks always have these settings.
    // This means that event tracks can look like ruler tracks if they want
    // to, but in general the same RulerConfig can be used for both event
    // and ruler tracks.
    if (is_ruler_track) {
        this->config.show_names = true;
        this->config.use_alpha = false;
        this->config.full_width = false;
    }
}


void
RulerOverlay::set_config(bool is_ruler_track, const RulerConfig &config)
{
    this->delete_config();
    this->config = config;
    // Same as is_ruler_track in constructor.
    if (is_ruler_track) {
        this->config.show_names = true;
        this->config.use_alpha = false;
        this->config.full_width = false;
    }
}

void
RulerOverlay::delete_config()
{
    for (auto &mlist : config.marklists)
        mlist->decref();
}


static bool
compare_marks(const PosMark &m1, const PosMark &m2)
{
    return m1.pos < m2.pos;
}


static bool
prev_text_is_first(const PosMark *begin, const PosMark *cur)
{
    if (cur == begin)
        return false;
    // True if the prev mark with name == begin
    const PosMark *p = cur - 1;
    for (;p >= begin; p--) {
        if (p->mark.name) {
            return p == begin;
        }
    }
    return false;
}


static const PosMark *
rewind_to_prev_visible(const PosMark *begin, const PosMark *cur, double zoom)
{
    // Ruler marks have their own thickness, so I have to start drawing from
    // the previous visible mark.
    while (cur > begin) {
        cur--;
        if (zoom >= cur->mark.zoom_level)
            break;
    }
    return cur;
}


void
RulerOverlay::draw(const IRect &box, const Zoom &zoom, const IRect &clip)
{
    // IRect clip = f_util::clip_rect(f_util::rect(this));
    // DEBUG("clip: " << clip);
    if (clip.w == 0 || clip.h == 0)
        return;
    // int y = this->y() + 2; // TODO track_start()

    ScoreTime start = zoom.to_time(clip.y - box.y);
    ScoreTime end = start + zoom.to_time(clip.h);
    start = start + zoom.offset;
    end = end + zoom.offset;
    // DEBUG("RULER CLIP: " << start << "--" << end << ", "
    //         << SHOW_RANGE(clip));

    // Later marklists will draw over earlier ones.
    for (auto &mlist : config.marklists) {
        const PosMark *marks_end = mlist->marks + mlist->length;
        const PosMark *m = std::lower_bound(mlist->marks, marks_end,
            PosMark(start, Mark()), compare_marks);
        if (config.show_names && prev_text_is_first(mlist->marks, m))
            m = mlist->marks;
        else
            m = rewind_to_prev_visible(mlist->marks, m, zoom.factor);
        for (; m < marks_end; m++) {
            int offset = box.y + zoom.to_pixels(m->pos - zoom.offset);
            bool drew_text = draw_mark(
                box, zoom, m->pos == ScoreTime(0), offset, m->mark);
            // There probably isn't any ruler text this tall.
            if ((drew_text && m->pos > end) || offset > clip.b() + 15)
                break;
        }
    }
}


// Return true if I drew a text label.
//
// If the mark at pos 0 has a name, it will never be seen.  Since a mark
// at 0 is likely for e.g. a beginning of piece cue, there's a special hack
// to draw that name below rather than above the line.
bool
RulerOverlay::draw_mark(
    const IRect &box, const Zoom &zoom, bool at_zero, int offset,
    const Mark &mark)
{
    Color c = mark.color;
    if (this->config.align_to_bottom)
        offset -= mark.width - 1;

    // DEBUG("mark: @" << offset << " r" << mark.rank << " c: " << mark.color);
    if (!this->config.use_alpha)
        c.a = 0xff;

    double width = box.w - 2; // 2 pixels to keep away from the box edges
    // The rank->width sequence goes [1/1, 3/4, 1/2, 1/3, ...]
    if (this->config.full_width || mark.rank == 0)
        ;
    else if (mark.rank == 1)
        width *= 3.0 / 4.0;
    else
        width *= 1.0 / mark.rank;
    width = floor(width);

    if (zoom.factor >= mark.zoom_level)
        alpha_rectf(IRect(box.r() - width - 1, offset, width, mark.width), c);

    bool drew_text = false;
    if (zoom.factor >= mark.name_zoom_level && this->config.show_names
        && mark.name)
    {
        static SymbolTable::Style style(
            Config::font, Config::font_size::ruler, FL_BLACK);
        int xmin = box.x + 2;
        int ypos = at_zero ? offset + fl_height()  : offset - 1;
        SymbolTable::get()->draw(mark.name, IPoint(xmin, ypos), style);
        drew_text = true;
    }
    return drew_text;
}
