/*
*/
#include <math.h>

#include "util.h"
#include "f_util.h"
#include "alpha_draw.h"

#include "Ruler.h"

// Height in pixels both above and below  of the special indicator that is
// drawn on a 0 size selection.
const static int selection_point_size = 4;
// Selections are always at least this many pixels.
const static int selection_min_size = 4;

void
OverlayRuler::set_zoom(const ZoomInfo &zoom)
{
    if (this->zoom == zoom)
        return;
    if (this->zoom.factor == zoom.factor) {
        this->shift = this->zoom.to_pixels(zoom.offset - this->zoom.offset);
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
    // DEBUG("set selection " << selnum << " " << sel.tracks);
    const Selection &old = this->selections[selnum];
    if (old == sel)
        return;
    // Clear old selection, set new selection if there is one.
    if (old.start_track <= tracknum && tracknum < old.start_track + old.tracks)
        this->damage_range(old.start_pos, old.start_pos + old.duration);
    if (sel.start_track <= tracknum && tracknum < sel.start_track + sel.tracks)
        this->damage_range(sel.start_pos, sel.start_pos + sel.duration);
    this->selections[selnum] = sel;
}


TrackPos
OverlayRuler::time_end() const
{
    TrackPos end(0);
    for (int i = 0; i < Config::max_selections; i++) {
        const Selection &sel = selections[i];
        if (sel.tracks != 0)
            end = std::max(end, sel.start_pos + sel.duration);
    }
    return end;
}


void
OverlayRuler::set_config(const RulerConfig &config, FinalizeCallback finalizer,
        TrackPos start, TrackPos end)
{
    this->finalize_callbacks(finalizer);
    this->config = config;
    this->damage_range(start, end); // TODO what about damage everything?
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
    // Prevent marks at the top and bottom from drawing outside the ruler.
    ClipArea clip_area(draw_area);

    /*
    uchar d = this->damage();

    if (d & FL_DAMAGE_ALL) {
        // draw_area(rect(this));
    } else {
        if (d & FL_DAMAGE_SCROLL) {
            // fl_scroll(...)
            // draw_area(...) // revealed areas
        }
    }
    */

    Fl_Group::draw();
    this->draw_marklists();
    this->draw_selections();
    this->damaged_area.w = this->damaged_area.h = 0;
    this->shift = 0;
}


// Intersect 'r' with the clip area.
static Rect
clip_rect(Rect r)
{
    int x, y, w, h;
    fl_clip_box(r.x, r.y, r.w, r.h, x, y, w, h);
    return Rect(x, y, w, h);
}


void
OverlayRuler::damage_range(TrackPos start, TrackPos end)
{
    Rect r = rect(this);
    r.y = this->zoom.to_pixels(start - this->zoom.offset);
    r.h = std::max(selection_min_size, this->zoom.to_pixels(end));
    this->damaged_area.union_(r);
    this->damage(FL_DAMAGE_USER1);
}


void
OverlayRuler::draw_marklists()
{
    Rect clip = clip_rect(rect(this));
    // DEBUG("clip: " << clip);
    if (clip.w == 0 || clip.h == 0)
        return;

    TrackPos *mark_tps;
    Mark *marks;
    int count = 0;
    TrackPos start = this->zoom.offset;
    TrackPos end = this->zoom.to_trackpos(clip.h) + this->zoom.offset;

    fl_font(FL_HELVETICA, 9);
    // Later marklists will draw over earlier ones.
    for (Marklists::const_iterator mlist = config.marklists.begin();
            mlist != config.marklists.end(); ++mlist)
    {
        int count = mlist->find_marks(&start, &end, &mark_tps, &marks);
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
    for (int i = 0; i < Config::max_selections; i++) {
        const Selection &sel = this->selections[i];
        if (sel.tracks == 0)
            continue;
        int start = y() + this->zoom.to_pixels(
                sel.start_pos - this->zoom.offset);
        int height = std::max(selection_min_size,
                this->zoom.to_pixels(sel.duration));
        alpha_rectf(Rect(x(), start, w(), height), sel.color);
        if (sel.duration == TrackPos(0)) {
            // Darken the select color a bit, and make it non-transparent.
            fl_color(color_to_fl(sel.color.scale(0.5)));
            fl_line_style(FL_SOLID, 1);
            fl_line(x() + 2, start, x() + w() - 2, start);
            // Draw little bevel thingy.
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
    ruler.set_config(*track.ruler, finalizer, start, end);
    if (color_to_fl(track.ruler->bg) != bg_box.color()) {
        bg_box.color(color_to_fl(track.ruler->bg));
        bg_box.redraw();
        if (title_box) {
            title_box->color(color_to_fl(track.ruler->bg));
            title_box->redraw();
        }
    }
}
