/*
*/
#include <math.h>

#include "util.h"
#include "f_util.h"
#include "alpha_draw.h"

#include "config.h"
#include "Ruler.h"


// Height in pixels both above and below  of the special indicator that is
// drawn on a 0 size selection.
const static int selection_point_size = 4;
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
        marks = NULL;
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
OverlayRuler::set_selection(int selnum, int tracknum, const Selection &sel)
{
    ASSERT(0 <= selnum && selnum < Config::max_selections);
    // DEBUG("set selection " << sel.start_pos << "--" << sel.cur_pos);
    TrackSelection news(sel, tracknum);
    const TrackSelection &olds = this->selections[selnum];

    if (olds.empty() && !news.empty()) {
        damage_range(news.low(), news.high());
    } else if (!olds.empty() && news.empty()) {
        damage_range(olds.low(), olds.high());
    } else if (!olds.empty() && !news.empty()) {
        if (olds.high() <= news.low() || news.high() <= olds.low()) {
            // Not overlapping
            damage_range(olds.low(), olds.high());
            damage_range(news.low(), news.high());
        } else {
            ScoreTime start0 = std::min(olds.low(), news.low());
            ScoreTime end0 = std::max(olds.low(), news.low());
            ScoreTime start1 = std::min(olds.high(), news.high());
            ScoreTime end1 = std::max(olds.high(), news.high());
            if (end0 > start0)
                damage_range(start0, end0);
            if (end1 > start1)
                damage_range(start1, end1);
            // DEBUG("overlap: 0 " << start0 << "--" << end0
            //     << ", 1 " << start1 << "--" << end1);
        }
    }
    this->selections[selnum] = news;
}


ScoreTime
OverlayRuler::time_end() const
{
    return this->config.last_mark_pos;
    // Now that I go to the end of the ruler, I probably don't need to check
    // the selections any more.
    /*
    ScoreTime end(0);
    for (int i = 0; i < Config::max_selections; i++) {
        if (!selections[i].empty())
            end = std::max(end, selections[i].end);
    }
    return end;
    */
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
    this->damage_range(start, end);
}

void
OverlayRuler::delete_config()
{
    for (Marklists::iterator mlist = config.marklists.begin();
            mlist != config.marklists.end(); ++mlist)
    {
        (*mlist)->decref();
    }
}


// I redraw the scroll revealed area separately, so pass a dummy to fl_scroll.
static void dummy_scroll_draw(void *, int, int, int, int) {}

void
OverlayRuler::draw()
{
    // This relies on the parent having clipped out bits like the bevel.
    bool clip = false;
    // DEBUG("ruler damage " << show_damage(damage()));
    if (damage() == OverlayRuler::DAMAGE_RANGE) {
        IRect c = rect(this).intersect(this->damaged_area);
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
OverlayRuler::damage_range(ScoreTime start, ScoreTime end)
{
    IRect r = rect(this);
    if (start == ScoreTime(-1) && end == ScoreTime(-1)) {
        ; // leave it covering the whole widget
    } else {
        r.y += this->zoom.to_pixels(start - this->zoom.offset);
        r.h = this->zoom.to_pixels(end - start);
        // Since the selection point extends downwards a bit, always extend a
        // little to cover it if it was there.  This is so when the selection
        // is no longer a point the extra hanging bit will be cleared properly.
        // The problem doesn't occur above the selection because it hangs down,
        // not up.
        // +1, otherwise retina displays get a hanging pixel.
        r.h += selection_point_size + 1;
        if (start == end) {
            r.y -= selection_point_size;
            r.h += selection_point_size;
        }
    }

    // DEBUG(SHOW_RANGE(damaged_area) << " UNION " << SHOW_RANGE(r)
    //         << " = " << SHOW_RANGE(damaged_area.union_(r)));
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
    IRect clip = clip_rect(rect(this));
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

    // Show updated range, for debugging.
    // fl_color(color_cycle());
    // fl_rectf(clip.x + 10, clip.y, 2, clip.h);

    fl_font(Config::font, Config::font_size::ruler);
    // Later marklists will draw over earlier ones.
    for (Marklists::const_iterator it = config.marklists.begin();
            it != config.marklists.end(); ++it)
    {
        const Marklist *mlist = *it;
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
        // Extra pixels to keep from bumping into the bezel.
        int text_width = fl_width(mark.name) + 2;
        int xmin = x() + 2;
        int xmax = x() + w() - text_width;
        // Try to be right to the left of the mark, but align with the left
        // side if I must.
        int xpos = ::clamp(xmin, xmax, x() + w() - int(width) - text_width);
        // Unless there really isn't enough room, then clip on the left.
        if (xpos + text_width > x() + w()) {
            fl_color(color_to_fl(Config::abbreviation_color));
            fl_line_style(FL_SOLID, 2);
            fl_line(x() + 2, offset - fl_height(), x() + 2, offset);
            fl_line_style(0);
            xpos = xmax;
        }

        int text_at = at_zero ? offset + fl_height()  : offset - 1;
        fl_color(FL_BLACK);
        fl_draw(mark.name, xpos, text_at);
        drew_text = true;
    }
    return drew_text;
}


void
OverlayRuler::draw_selections()
{
    IRect sel_rect;
    int y = this->track_start();
    for (int i = 0; i < Config::max_selections; i++) {
        const TrackSelection &sel = this->selections[i];
        if (sel.empty())
            continue;
        int start = y + this->zoom.to_pixels(sel.low() - this->zoom.offset);
        int height = std::max(selection_min_size,
                this->zoom.to_pixels(sel.high() - sel.low()));
        // IRect intersection is half-open ranges, but rect drawing is inclusive
        // pixel ranges.  So add one to ensure that if I share a pixel border
        // with the clip rect, I'll still draw that pixel line.
        sel_rect = clip_rect(IRect(x(), start, w(), height + 1));
        fl_line_style(FL_SOLID, 0);
        alpha_rectf(sel_rect, sel.color);

        // Darken the the cur pos a bit, and make it non-transparent.
        fl_color(color_to_fl(sel.color.brightness(0.5)));
        int cur = y + this->zoom.to_pixels(sel.cur - this->zoom.offset);
        fl_line(x() + 2, cur, x() + w() - 2, cur);
        if (sel.is_point() && sel.is_cur_track) {
            // Draw a little bevel thingy.
            const int sz = selection_point_size;
            fl_polygon(x(), cur - sz, x() + 4, cur, x(), cur + sz);
        }
    }
}


// RulerTrackView

RulerTrackView::RulerTrackView(const RulerConfig &config) :
    TrackView("ruler"),
    title_box(0),
    bg_box(0, 0, 1, 1),
    ruler(config, true)
{
    this->add(bg_box);
    this->add(ruler);
    end();

    bg_box.box(FL_THIN_DOWN_BOX);
    bg_box.color(color_to_fl(config.bg));
}


// Don't create the title widget until it is actually requested.  This avoids
// creating a title box for Rulers that don't have one.
Fl_Box &
RulerTrackView::title_widget()
{
    if (!this->title_box) {
        this->title_box = new Fl_Box(0, 0, 1, 1);
        title_box->box(FL_FLAT_BOX);
        title_box->color(color_to_fl(this->ruler.config.bg));
    }
    return *this->title_box;
}


void
RulerTrackView::set_zoom(const ZoomInfo &new_zoom)
{
    // duplicated, like the scroll in ::draw, with EventTrackView
    if (new_zoom == ruler.zoom)
        return;
    if (ruler.zoom.factor == new_zoom.factor)
        this->damage(FL_DAMAGE_SCROLL);
    else
        this->damage(FL_DAMAGE_ALL);
    ruler.set_zoom(new_zoom);
}


void
RulerTrackView::update(const Tracklike &track, ScoreTime start, ScoreTime end)
{
    ASSERT_MSG(track.ruler && !track.track,
        "updated a ruler track with an event track config");
    this->ruler.set_config(true, *track.ruler, start, end);
    if (color_to_fl(track.ruler->bg) != bg_box.color()) {
        bg_box.color(color_to_fl(track.ruler->bg));
        bg_box.redraw();
        if (title_box) {
            title_box->color(color_to_fl(track.ruler->bg));
            title_box->redraw();
        }
    }
}

void
RulerTrackView::finalize_callbacks()
{
    ruler.delete_config();
}

void
RulerTrackView::draw()
{
    IRect draw_area = rect(this);

    // TODO this is mostly a copy from EventTrackView
    // factor this stuff into a "ScrollableTrack" base class?
    // DEBUG("ruler track damage " << show_damage(damage()));

    // Fast scrolling is disabled, because it's hard to get right, and it
    // doesn't actually seem to improve performance.
    // TODO either fix or remove
    if (false && damage() == FL_DAMAGE_SCROLL) {
        // Avoid the one pixel upper and lower bezels;
        draw_area.x++; draw_area.w -= 2;
        draw_area.y++; draw_area.h -= 2;
        draw_area = clip_rect(draw_area);

        // I'm sure there's a better way to scroll than this copy and pastage,
        // but there are only two cases, so whatever.
        int scroll = ruler.zoom.to_pixels(ruler.zoom.offset)
            - ruler.zoom.to_pixels(ruler.last_offset);
        fl_scroll(draw_area.x, draw_area.y, draw_area.w, draw_area.h,
            0, -scroll, dummy_scroll_draw, NULL);
        ScoreTime shift_pos = std::max(
            ruler.zoom.offset - ruler.last_offset,
            ruler.last_offset - ruler.zoom.offset);
        if (scroll > 0) { // Contents moved up, bottom is damaged.
            ScoreTime bottom = ruler.zoom.offset
                + ruler.zoom.to_time(draw_area.h);
            this->ruler.damage_range(bottom - shift_pos, bottom);
            draw_area.y = draw_area.b() - scroll;
            draw_area.h = scroll;
        } else if (scroll < 0) { // Contents moved down, top is damaged.
            this->ruler.damage_range(
                ruler.zoom.offset, ruler.zoom.offset + shift_pos);
            draw_area.h = -scroll;
        } else {
            draw_area.h = 0;
        }
    } else if (damage() == FL_DAMAGE_CHILD) {
        // Only CHILD damage means a selection was set.  But since I overlap
        // with the child, I have to draw too.
        // DEBUG("pre intersect " << SHOW_RANGE(draw_area));
        draw_area = draw_area.intersect(this->ruler.damaged_area);
        // DEBUG("post intersect " << SHOW_RANGE(draw_area));
    } else {
        this->damage(FL_DAMAGE_ALL);
    }
    // Prevent marks at the top and bottom from drawing outside the ruler.
    ClipArea clip_area(draw_area);
    this->draw_child(this->bg_box);

    // This is more than one pixel, but otherwise I draw on top of the bevel on
    // retina displays.
    IRect inside_bevel = rect(this);
    inside_bevel.x += 2; inside_bevel.w -= 3;
    inside_bevel.y += 2; inside_bevel.h -= 3;
    ClipArea clip_area2(inside_bevel);

    if (damage() & FL_DAMAGE_ALL)
        this->draw_child(this->ruler);
    else
        this->update_child(this->ruler);
}

std::string
RulerTrackView::dump() const
{
    return "type ruler";
}
