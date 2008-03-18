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
OverlayRuler::set_selection(int selnum, Color c, const Selection &sel)
{
    DEBUG("set sel " << selnum << " " << c << ": "
            << sel.start_pos << "--" << sel.duration);
    const Selection &old = this->selections[selnum].second;
    if (old.tracks != 0) {
        this->damage_range(old.start_pos, old.start_pos + old.duration);
    }
    this->damage_range(sel.start_pos, sel.start_pos + sel.duration);
    this->selections[selnum] = std::make_pair(c, sel);
}


TrackPos
OverlayRuler::time_end() const
{
    TrackPos end(0);
    for (int i = 0; i < this->selections.size(); i++) {
        const Selection &sel = selections[i].second;
        if (sel.tracks != 0)
            end = std::max(end, sel.start_pos + sel.duration);
    }
    return end;
}


void
OverlayRuler::draw()
{
    Rect draw_area = rect(this);
    draw_area.h--; // tiles make a 1 pixel lower border
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
    r.y = this->zoom.to_pixels(start);
    r.h = std::max(selection_min_size,
            this->zoom.to_pixels(this->zoom.offset + end));
    this->damaged_area.union_(r);
    this->damage(FL_DAMAGE_USER1);
}


void
OverlayRuler::draw_mark(int offset, const Mark &mark)
{
    Color c = mark.color;

    // DEBUG("mark: @" << offset << " r" << mark.rank << " c: " << mark.color);
    if (!this->model->use_alpha)
        c.a = 0xff;

    double width = w() - 2; // 2 pixels to keep away from the box edges
    // The rank->width sequence goes [1/1, 3/4, 1/2, 1/3, ...]
    if (this->model->full_width || mark.rank == 0)
        ;
    else if (mark.rank == 1)
        width *= 3.0/4.0;
    else
        width *= 1.0/mark.rank;
    width = floor(width);

    if (this->zoom.factor >= mark.zoom_level)
        alpha_rectf(Rect(x()+w() - width - 1, offset, width, mark.width), c);

    if (this->zoom.factor >= mark.name_zoom_level && this->model->show_names
            && mark.name.size() > 0)
    {
        int text_width = fl_width(mark.name.c_str());
        int xmin = x() + 2;
        int xmax = x() + w() - text_width;
        // Try to be right to the left of the mark, but align with the left
        // site if I must.
        int xpos = ::clamp(xmin, xmax, x() + w() - int(width) - text_width);

        fl_font(fl_font(), 9);
        fl_color(FL_BLACK);
        fl_draw(mark.name.c_str(), xpos, offset);
    }
}


void
OverlayRuler::draw_marklists()
{
    Rect clip = clip_rect(rect(this));
    // DEBUG("clip: " << clip);
    if (clip.w == 0 || clip.h == 0)
        return;
    // Later marklists will draw over earlier ones.
    for (Marklists::const_iterator mlist = model->marklists.begin();
            mlist != model->marklists.end(); ++mlist)
    {
        // TODO binary search?
        for (Marklist::const_iterator mark = (*mlist)->begin();
                mark != (*mlist)->end(); ++mark)
        {
            // mark is pair(trackpos, mark)
            int offset = y() + zoom.to_pixels(mark->first);
            // mlist should be sorted, so I can break after I pass the bottom.
            if (offset < clip.y)
                continue;
            else if (offset >= clip.b())
                break;
            else
                draw_mark(offset, mark->second);
        }
    }
}


void
OverlayRuler::draw_selections()
{
    for (int i = 0; i < this->selections.size(); i++) {
        const Selection &sel = this->selections[i].second;
        if (sel.tracks == 0)
            continue;
        int start = y() + this->zoom.to_pixels(sel.start_pos);
        int height = std::max(selection_min_size,
                this->zoom.to_pixels(zoom.offset + sel.duration));
        alpha_rectf(Rect(x(), start, w(), height), this->selections[i].first);
        if (sel.duration == TrackPos(0)) {
            // Darken the select color a bit, and make it non-transparent.
            fl_color(color_to_fl(this->selections[i].first.scale(0.5)));
            fl_line_style(FL_SOLID, 1);
            fl_line(x() + 2, start, x() + w() - 2, start);
            // Draw little bevel thingy.
            const int sz = selection_point_size;
            fl_polygon(x(), start - sz, x() + 4, start, x(), start + sz);
        }
    }
}


RulerTrackView::RulerTrackView(boost::shared_ptr<const RulerTrackModel> model) :
    TrackView("ruler"),
    title_box(0),
    ruler(model),
        bg_box(0, 0, 1, 1)
{
    this->add(ruler);
    ruler.add(bg_box);
    end();

    bg_box.box(FL_THIN_DOWN_BOX);
    bg_box.color(color_to_fl(model->bg));
}


// Don't create the title widget until it is actually requested.  This avoids
// creating a title box for Rulers that don't have one.
Fl_Box &
RulerTrackView::title_widget()
{
    if (!this->title_box) {
        this->title_box = new Fl_Box(0, 0, 1, 1);
        title_box->box(FL_FLAT_BOX);
        title_box->color(color_to_fl(this->ruler.model->bg));
    }
    return *this->title_box;
}
