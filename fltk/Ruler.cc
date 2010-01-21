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
            TrackPos start0 = std::min(olds.low(), news.low());
            TrackPos end0 = std::max(olds.low(), news.low());
            TrackPos start1 = std::min(olds.high(), news.high());
            TrackPos end1 = std::max(olds.high(), news.high());
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


TrackPos
OverlayRuler::time_end() const
{
    return this->config.last_mark_pos;
    // Now that I go to the end of the ruler, I probably don't need to check
    // the selections any more.
    /*
    TrackPos end(0);
    for (int i = 0; i < Config::max_selections; i++) {
        if (!selections[i].empty())
            end = std::max(end, selections[i].end);
    }
    return end;
    */
}


void
OverlayRuler::set_config(const RulerConfig &config, FinalizeCallback finalizer,
        TrackPos start, TrackPos end)
{
    this->finalize_callbacks(finalizer);
    this->config = config;
    this->damage_range(start, end);
}


void
OverlayRuler::finalize_callbacks(FinalizeCallback finalizer)
{
    for (size_t i = 0; i < this->config.marklists.size(); i++)
        finalizer((void *) this->config.marklists[i].find_marks);
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
        Rect c = rect(this).intersect(this->damaged_area);
        fl_push_clip(c.x, c.y, c.w, c.h);
        // DEBUG("draw range " << c << c.height_range());
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
OverlayRuler::damage_range(TrackPos start, TrackPos end)
{
    Rect r = rect(this);
    if (start == TrackPos(-1) && end == TrackPos(-1)) {
        ; // leave it covering the whole widget
    } else {
        r.y += this->zoom.to_pixels(start - this->zoom.offset);
        r.h = this->zoom.to_pixels(end - start);
        // Since the selection point extends downwards a bit, always extend a
        // little to cover it if it was there.  This is so when the selection
        // is no longer a point the extra hanging bit will be cleared properly.
        // The problem doesn't occur above the selection because it hangs down,
        // not up.
        r.h += selection_point_size;
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


void
OverlayRuler::draw_marklists()
{
    Rect clip = clip_rect(rect(this));
    // DEBUG("clip: " << clip);
    if (clip.w == 0 || clip.h == 0)
        return;
    int y = this->y() + 1; // avoid bevel

    TrackPos start = this->zoom.to_trackpos(clip.y - y);
    TrackPos end = start + this->zoom.to_trackpos(clip.h);
    start = start + this->zoom.offset;
    end = end + this->zoom.offset;
    // DEBUG("RULER CLIP: " << start << "--" << end << ", "
    //         << SHOW_RANGE(clip));

    TrackPos *mark_tps;
    Mark *marks;

    // Show updated range, for debugging.
    // Fl_Color colors[] =
    //     { FL_RED, FL_GREEN, FL_YELLOW, FL_BLUE, FL_MAGENTA, FL_CYAN };
    // static int colori;
    // fl_color(colors[colori]);
    // colori = (colori+1) % sizeof colors;
    //  fl_rectf(clip.x + 10, clip.y, 2, clip.h);

    fl_font(Config::font, Config::font_size::ruler);
    // Later marklists will draw over earlier ones.
    for (Marklists::const_iterator mlist = config.marklists.begin();
            mlist != config.marklists.end(); ++mlist)
    {
        // DEBUG("FIND " << &mlist->find_marks);
        // This is unnecessary but may help debugging.
        Marklist::FindMarks find = mlist->find_marks;
        int count = find(&start, &end, &mark_tps, &marks);
        for (int i = 0; i < count; i++) {
            int offset = y + zoom.to_pixels(mark_tps[i] - zoom.offset);
            draw_mark(offset, marks[i]);
        }
        if (count) {
            for (int i = 0; i < count; i++) {
                if (marks[i].name)
                    free(marks[i].name);
            }
            free(marks);
            free(mark_tps);
        }
    }
}


void
OverlayRuler::draw_mark(int offset, const Mark &mark)
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
        alpha_rectf(Rect(x()+w() - width - 1, offset, width, mark.width), c);

    if (this->zoom.factor >= mark.name_zoom_level && this->config.show_names
            && mark.name)
    {
        int text_width = fl_width(mark.name);
        int xmin = x() + 2;
        int xmax = x() + w() - text_width;
        // Try to be right to the left of the mark, but align with the left
        // side if I must.
        int xpos = ::clamp(xmin, xmax, x() + w() - int(width) - text_width);
        // Unless there really isn't enough room, then clip on the left.
        if (xpos + text_width > x() + w()) {
            fl_color(color_to_fl(Config::abbreviation_color));
            fl_line_style(FL_SOLID, 2);
            fl_line(x() + 1, offset - fl_height(), x() + 1, offset);
            xpos = xmax;
        }

        fl_color(FL_BLACK);
        fl_draw(mark.name, xpos, offset);
    }
}


void
OverlayRuler::draw_selections()
{
    Rect sel_rect;
    int y = this->y() + 1; // avoid bevel
    for (int i = 0; i < Config::max_selections; i++) {
        const TrackSelection &sel = this->selections[i];
        if (sel.empty())
            continue;
        int start = y + this->zoom.to_pixels(sel.low() - this->zoom.offset);
        int height = std::max(selection_min_size,
                this->zoom.to_pixels(sel.high() - sel.low()));
        // Rect intersection is half-open ranges, but rect drawing is inclusive
        // pixel ranges.  So add one to ensure that if I share a pixel border
        // with the clip rect, I'll still draw that pixel line.
        sel_rect = clip_rect(Rect(x(), start, w(), height + 1));
        alpha_rectf(sel_rect, sel.color);

        // Darken the the cur pos a bit, and make it non-transparent.
        fl_color(color_to_fl(sel.color.brightness(0.5)));
        fl_line_style(FL_SOLID, 1);
        int cur = y + this->zoom.to_pixels(sel.cur - this->zoom.offset);
        fl_line(x() + 2, cur, x() + w() - 2, cur);
        if (sel.is_point() && sel.is_cur_track) {
            // Draw a little bevel thingy.
            const int sz = selection_point_size;
            fl_polygon(x(), cur - sz, x() + 4, cur, x(), cur + sz);
        }
    }
}


RulerTrackView::RulerTrackView(const RulerConfig &config) :
    TrackView("ruler"),
    title_box(0),
    bg_box(0, 0, 1, 1),
    ruler(config)
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
RulerTrackView::update(const Tracklike &track, FinalizeCallback finalizer,
        TrackPos start, TrackPos end)
{
    ASSERT(track.ruler && !track.track);
    this->ruler.set_config(*track.ruler, finalizer, start, end);
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
RulerTrackView::draw()
{
    Rect draw_area = rect(this);

    // TODO this is mostly a copy from EventTrackView
    // factor this stuff into a "ScrollableTrack" base class?
    // DEBUG("ruler track damage " << show_damage(damage()));
    if (damage() == FL_DAMAGE_SCROLL) {
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
        TrackPos shift_pos = std::max(
                ruler.zoom.offset - ruler.last_offset,
                ruler.last_offset - ruler.zoom.offset);
        if (scroll > 0) { // Contents moved up, bottom is damaged.
            TrackPos bottom = ruler.zoom.offset
                + ruler.zoom.to_trackpos(draw_area.h);
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
    }
    // Prevent marks at the top and bottom from drawing outside the ruler.
    ClipArea clip_area(draw_area);
    this->draw_child(this->bg_box);

    Rect inside_bevel = rect(this);
    inside_bevel.x++; inside_bevel.w -= 2;
    inside_bevel.y++; inside_bevel.h -= 2;
    ClipArea clip_area2(inside_bevel);
    if (damage() & FL_DAMAGE_ALL)
        this->draw_child(this->ruler);
    else
        this->update_child(this->ruler);
}
