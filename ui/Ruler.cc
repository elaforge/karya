/*
*/
#include <math.h>

#include "util.h"
#include "f_util.h"
#include "alpha_draw.h"

#include "Ruler.h"


void
OverlayRuler::draw()
{
    Fl_Group::draw();
    this->draw_marklists();
}

static Rect
clip_rect(Rect r)
{
    int x, y, w, h;
    fl_clip_box(r.x, r.y, r.w, r.h, x, y, w, h);
    return Rect(x, y, w, h);
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
        // TODO draw name
    }
}


void
OverlayRuler::draw_marklists()
{
    Rect draw_area = rect(this);
    draw_area.h--; // tiles make a 1 pixel lower border
    Rect clip = clip_rect(draw_area);
    // DEBUG("clip: " << clip);
    if (clip.w == 0 || clip.h == 0)
        return;
    // Prevent marks at the top and bottom from drawing outside the ruler.
    ClipArea clip_area(draw_area);
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
