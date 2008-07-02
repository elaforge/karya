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
const static int selection_min_size = 3;

// Hack for debugging.
#define SHOW_RANGE(r) (r).y << "--" << (r).b()

void
OverlayRuler::set_zoom(const ZoomInfo &zoom)
{
    if (this->zoom == zoom)
        return;
    if (this->zoom.factor == zoom.factor) {
        // TODO shift is for scrolling, but it's not implemented yet
        // this->shift = this->zoom.to_pixels(zoom.offset - this->zoom.offset);
        this->damage(FL_DAMAGE_SCROLL);
    } else {
        this->damage(FL_DAMAGE_ALL);
    }
    this->zoom = zoom;
}


void
OverlayRuler::set_selection(int selnum, int tracknum, const Selection &sel)
{
    ASSERT(0 <= selnum && selnum < Config::max_selections);
    // DEBUG("set selection " << selnum << " - " << sel.start_track << " "
    //         << sel.tracks);
    TrackSelection news(sel, tracknum);
    const TrackSelection &olds = this->selections[selnum];

    if (olds.empty() && !news.empty()) {
        // DEBUG("add new " << news.start << "--" << news.end);
        damage_range(news.start, news.end);
    } else if (!olds.empty() && news.empty()) {
        // DEBUG("clear old " << olds.start << "--" << olds.end);
        damage_range(olds.start, olds.end);
    } else if (!olds.empty() && !news.empty()) {
        if (olds.end <= news.start || news.end <= olds.start) {
            // Not overlapping
            // DEBUG("not overlapping");
            damage_range(olds.start, olds.end);
            damage_range(news.start, news.end);
        } else {
            TrackPos start0 = std::min(olds.start, news.start);
            TrackPos end0 = std::max(olds.start, news.start);
            TrackPos start1 = std::min(olds.end, news.end);
            TrackPos end1 = std::max(olds.end, news.end);
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
    for (int i = 0; i < this->config.marklists.size(); i++)
        finalizer((void *) this->config.marklists[i].find_marks);
}


void
OverlayRuler::draw()
{
    Rect draw_area = rect(this);
    draw_area.h--; // tiles make a 1 pixel left/lower border
    draw_area.w--;
    draw_area.x++;

    // DEBUG("RULER DAMAGE " << show_damage(damage()));
    if (this->damage() & ~OverlayRuler::DAMAGE_RANGE) {
    } else {
        // DEBUG("INTERSECT: " << SHOW_RANGE(draw_area) << " with "
        //     << SHOW_RANGE(damaged_area) << " = "
        //     << SHOW_RANGE(draw_area.intersect(this->damaged_area)));
        draw_area = draw_area.intersect(this->damaged_area);
    }
    // Prevent marks at the top and bottom from drawing outside the ruler.
    ClipArea clip_area(draw_area);

    // DEBUG("draw group");
    Fl_Group::draw();
    // DEBUG("draw marklists");
    this->draw_marklists();
    // DEBUG("draw selections " << this);
    this->draw_selections();
    // DEBUG("done");
    this->damaged_area.w = this->damaged_area.h = 0;
    this->shift = 0;
}


void
OverlayRuler::damage_range(TrackPos start, TrackPos end)
{
    Rect r = rect(this);
    if (start == TrackPos(-1) and end == TrackPos(-1)) {
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

    TrackPos start = this->zoom.to_trackpos(clip.y - this->y());
    TrackPos end = start + this->zoom.to_trackpos(clip.h);
    start = start + this->zoom.offset;
    end = end + this->zoom.offset;
    // DEBUG("RULER CLIP: " << start << "--" << end << ", "
    //         << SHOW_RANGE(clip));

    TrackPos *mark_tps;
    Mark *marks;
    int count = 0;

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
            int offset = y() + zoom.to_pixels(mark_tps[i] - zoom.offset);
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
        // site if I must.
        int xpos = ::clamp(xmin, xmax, x() + w() - int(width) - text_width);

        fl_color(FL_BLACK);
        fl_draw(mark.name, xpos, offset);
    }
}


void
OverlayRuler::draw_selections()
{
    Rect sel_rect;
    for (int i = 0; i < Config::max_selections; i++) {
        const TrackSelection &sel = this->selections[i];
        if (sel.empty())
            continue;
        int start = y() + this->zoom.to_pixels(sel.start - this->zoom.offset);
        int height = std::max(selection_min_size,
                this->zoom.to_pixels(sel.end - sel.start));
        sel_rect = clip_rect(Rect(x(), start, w(), height));
        // DEBUG("SEL rectf " << sel_rect.y << "--" << sel_rect.b());
        alpha_rectf(sel_rect, sel.color);
        if (sel.start == sel.end) {
            // Darken the select color a bit, and make it non-transparent.
            fl_color(color_to_fl(sel.color.scale(0.5)));
            fl_line_style(FL_SOLID, 1);
            fl_line(x() + 2, start, x() + w() - 2, start);
            // Draw a little bevel thingy.
            const int sz = selection_point_size;
            fl_polygon(x(), start - sz, x() + 4, start, x(), start + sz);
        }
    }
}


RulerTrackView::RulerTrackView(const RulerConfig &config) :
    TrackView("ruler"),
    title_box(0),
    ruler(config),
        bg_box(0, 0, 1, 1)
{
    this->add(ruler);
    ruler.add(bg_box);
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
