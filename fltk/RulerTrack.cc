// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <math.h>
#include <utility>

#include "alpha_draw.h"
#include "config.h"
#include "f_util.h"
#include "util.h"

#include "RulerTrack.h"


// Height in pixels both above and below  of the special indicator that is
// drawn on a 0 size selection.
const static int selection_point_size = 6;
// Selections are always at least this many pixels.
const static int selection_min_size = 2;

// Hack for debugging.
#define SHOW_RANGE(r) (r).y << "--" << (r).b()


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


// OverlayRuler

OverlayRuler::OverlayRuler(const RulerConfig &config, bool is_ruler_track) :
    Fl_Widget(0, 0, 1, 1), damaged_area(), config(config), zoom()
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
OverlayRuler::set_selection(
    int selnum, int tracknum, const std::vector<Selection> &news)
{
    this->selections.resize(std::max(int(selections.size()), selnum + 1));
    for (auto &sel : selections[selnum])
        damage_range(sel.low(), sel.high(), true);
    for (auto &sel : news)
        damage_range(sel.low(), sel.high(), true);
    this->selections[selnum] = news;
}


void
OverlayRuler::set_config(bool is_ruler_track, const RulerConfig &config,
    ScoreTime start, ScoreTime end)
{
    this->delete_config();
    this->config = config;
    // Same as is_ruler_track in constructor.
    if (is_ruler_track) {
        this->config.show_names = true;
        this->config.use_alpha = false;
        this->config.full_width = false;
    }
    this->damage_range(start, end, false);
}

void
OverlayRuler::delete_config()
{
    for (auto &mlist : config.marklists)
        mlist->decref();
}


void
OverlayRuler::draw()
{
    // This relies on the parent having clipped out bits like the bevel.
    bool clip = false;
    // DEBUG("ruler damage " << show_damage(damage()));
    if (damage() == OverlayRuler::DAMAGE_RANGE) {
        IRect c = f_util::rect(this).intersect(this->damaged_area);
        fl_push_clip(c.x, c.y, c.w, c.h);
        // DEBUG("draw range " << c << ": " << c.height_range());
        clip = true;
    } else {
        // DEBUG("draw all");
    }
    this->draw_marklists();
    this->draw_selections();
    this->damaged_area.w = this->damaged_area.h = 0;
    this->last_offset = this->zoom.offset;
    if (clip)
        fl_pop_clip();
}


void
OverlayRuler::damage_range(ScoreTime start, ScoreTime end, bool selection)
{
    IRect r = f_util::rect(this);
    if (start == ScoreTime(-1) && end == ScoreTime(-1)) {
        ; // leave it covering the whole widget
    } else {
        r.y += this->zoom.to_pixels(start - this->zoom.offset);
        r.h = this->zoom.to_pixels(end - start);
        if (selection) {
            // Extend the damage area to cover the bevel arrow thing in
            // draw_selections().
            r.y -= selection_point_size;
            // +2, otherwise retina displays get a hanging pixel.
            r.h += selection_point_size * 2 + 2;
        }
    }

    // DEBUG("damage_range(" << start << ", " << end << "): "
    //     << SHOW_RANGE(damaged_area) << " + " << SHOW_RANGE(r)
    //     << " = " << SHOW_RANGE(damaged_area.union_(r)));
    this->damaged_area = this->damaged_area.union_(r);
    this->damage(OverlayRuler::DAMAGE_RANGE);
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
OverlayRuler::draw_marklists()
{
    IRect clip = f_util::clip_rect(f_util::rect(this));
    // DEBUG("clip: " << clip);
    if (clip.w == 0 || clip.h == 0)
        return;
    int y = this->track_start();

    ScoreTime start = this->zoom.to_time(clip.y - y);
    ScoreTime end = start + this->zoom.to_time(clip.h);
    start = start + this->zoom.offset;
    end = end + this->zoom.offset;
    // DEBUG("RULER CLIP: " << start << "--" << end << ", "
    //         << SHOW_RANGE(clip));

    fl_font(Config::font, Config::font_size::ruler);
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
            int offset = y + zoom.to_pixels(m->pos - zoom.offset);
            bool drew_text = draw_mark(
                m->pos == ScoreTime(0), offset, m->mark);
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
OverlayRuler::draw_mark(bool at_zero, int offset, const Mark &mark)
{
    Color c = mark.color;
    if (this->config.align_to_bottom)
        offset -= mark.width - 1;

    // DEBUG("mark: @" << offset << " r" << mark.rank << " c: " << mark.color);
    if (!this->config.use_alpha)
        c.a = 0xff;

    double width = w() - 2; // 2 pixels to keep away from the box edges
    // The rank->width sequence goes [1/1, 3/4, 1/2, 1/3, ...]
    if (this->config.full_width || mark.rank == 0)
        ;
    else if (mark.rank == 1)
        width *= 3.0/4.0;
    else
        width *= 1.0/mark.rank;
    width = floor(width);

    if (this->zoom.factor >= mark.zoom_level)
        alpha_rectf(IRect(x()+w() - width - 1, offset, width, mark.width), c);

    bool drew_text = false;
    if (this->zoom.factor >= mark.name_zoom_level && this->config.show_names
        && mark.name)
    {
        int xmin = x() + 2;
        int ypos = at_zero ? offset + fl_height()  : offset - 1;
        fl_color(FL_BLACK);
        fl_draw(mark.name, xmin, ypos);
        drew_text = true;
    }
    return drew_text;
}


void
OverlayRuler::draw_selections()
{
    IRect sel_rect;
    int y = this->track_start();
    for (const std::vector<Selection> &sels : selections) {
        for (const Selection &sel : sels) {
            if (sel.empty())
                continue;
            int start = y + this->zoom.to_pixels(sel.low() - this->zoom.offset);
            int height = std::max(selection_min_size,
                this->zoom.to_pixels(sel.high() - sel.low()));
            // IRect intersection is half-open ranges, but rect drawing is
            // inclusive pixel ranges.  So add one to ensure that if I share a
            // pixel border with the clip rect, I'll still draw that pixel
            // line.
            sel_rect = f_util::clip_rect(IRect(x(), start, w(), height + 1));
            fl_line_style(FL_SOLID, 0);
            alpha_rectf(sel_rect, sel.color);

            // Darken the the cur pos a bit, and make it non-transparent.
            fl_color(sel.color.brightness(0.5).fl());
            int cur = y + this->zoom.to_pixels(sel.cur - this->zoom.offset);
            fl_line(x() + 2, cur, x() + w() - 2, cur);
            if (sel.is_point() || sel.draw_arrow) {
                // Draw a little arrow bevel thingy, so you can see a point
                // selection, or see the cur track for a non-point selection.
                // 'damage_range' will extend the damage a bit to cover this.
                const int sz = selection_point_size;
                if (sel.draw_arrow) {
                    fl_color(FL_RED);
                    fl_polygon(
                        x(), cur - sz,
                        x() + sz, cur,
                        x(), cur + sz);
                } else {
                    fl_color(sel.color.fl());
                    fl_polygon(
                        x(), cur - sz,
                        x() + sz, cur,
                        x(), cur);
                }
            }
        }
    }
}


// RulerTrack

RulerTrack::RulerTrack(const RulerConfig &config) :
    Track("ruler"),
    title_box(0),
    bg_box(0, 0, 1, 1),
    ruler(config, true)
{
    this->add(bg_box);
    this->add(ruler);
    end();

    bg_box.box(FL_THIN_DOWN_BOX);
    bg_box.color(config.bg.fl());
}


// Don't create the title widget until it is actually requested.  This avoids
// creating a title box for Rulers that don't have one.
Fl_Box &
RulerTrack::title_widget()
{
    if (!this->title_box) {
        this->title_box = new Fl_Box(0, 0, 1, 1);
        title_box->box(FL_FLAT_BOX);
        title_box->color(this->ruler.config.bg.fl());
    }
    return *this->title_box;
}


void
RulerTrack::set_zoom(const ZoomInfo &new_zoom)
{
    // duplicated, like the scroll in ::draw, with EventTrack
    if (new_zoom == ruler.zoom)
        return;
    if (ruler.zoom.factor == new_zoom.factor)
        this->damage(FL_DAMAGE_SCROLL);
    else
        this->damage(FL_DAMAGE_ALL);
    ruler.set_zoom(new_zoom);
}


void
RulerTrack::update(const Tracklike &track, ScoreTime start, ScoreTime end)
{
    ASSERT_MSG(track.ruler && !track.track,
        "updated a ruler track with an event track config");
    this->ruler.set_config(true, *track.ruler, start, end);
    if (track.ruler->bg.fl() != bg_box.color()) {
        bg_box.color(track.ruler->bg.fl());
        bg_box.redraw();
        if (title_box) {
            title_box->color(track.ruler->bg.fl());
            title_box->redraw();
        }
    }
}

void
RulerTrack::finalize_callbacks()
{
    ruler.delete_config();
}

void
RulerTrack::draw()
{
    IRect draw_area = f_util::rect(this);

    // I used to look for FL_DAMAGE_SCROLL and use fl_scroll() for a fast
    // blit, but it was too hard to get right.  The biggest problem is that
    // events are at floats which are then rounded to ints for pixel positions.
    if (damage() == FL_DAMAGE_CHILD) {
        // Only CHILD damage means a selection was set.  But since I overlap
        // with the child, I have to draw too.
        // DEBUG("intersection with child: "
        //     << SHOW_RANGE(draw_area) << " + "
        //     << SHOW_RANGE(ruler.damaged_area) << " = "
        //     << SHOW_RANGE(draw_area.intersect(ruler.damaged_area)));
        draw_area = draw_area.intersect(this->ruler.damaged_area);
    } else {
        this->damage(FL_DAMAGE_ALL);
    }
    // Prevent marks at the top and bottom from drawing outside the ruler.
    f_util::ClipArea clip_area(draw_area);
    this->draw_child(this->bg_box);

    // This is more than one pixel, but otherwise I draw on top of the bevel on
    // retina displays.
    IRect inside_bevel = f_util::rect(this);
    inside_bevel.x += 2; inside_bevel.w -= 3;
    inside_bevel.y += 2; inside_bevel.h -= 3;
    f_util::ClipArea clip_area2(inside_bevel);

    if (damage() & FL_DAMAGE_ALL)
        this->draw_child(this->ruler);
    else
        this->update_child(this->ruler);
}

std::string
RulerTrack::dump() const
{
    return "type ruler";
}
