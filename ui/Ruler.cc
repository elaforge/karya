/*
*/
#include <math.h>

#include "util.h"
#include "f_util.h"

#include "Ruler.h"


void
OverlayRuler::draw()
{
    TrackView::draw();
    draw_marklists();
    // fl_rectf(x(), y(), 10, 20);
}

static Rect
clip_rect(Rect r)
{
    int x, y, w, h;
    fl_clip_box(r.x, r.y, r.w, r.h, x, y, w, h);
    return Rect(x, y, w, h);
}


static void
rectf(Rect r, Color c)
{
    DEBUG(r);
    // like fl_rectf, but supports alpha
    fl_color(color_to_fl(c));
    // fl_color(FL_BLACK);
    fl_rectf(r.x, r.y, r.w, r.h);
}


void
OverlayRuler::draw_mark(int offset, const Mark &mark)
{
    Color c = mark.color;

    DEBUG("mark: @" << offset << " r" << mark.rank << " c: " << mark.color);
    if (!this->use_alpha)
        c.a = 0;

    double width = w() - 2; // 2 pixels to keep away from the box edges
    // The rank->width sequence goes [1/1, 3/4, 1/2, 1/3, ...]
    if (mark.rank == 0)
        ;
    else if (mark.rank == 1)
        width *= 3.0/4.0;
    else
        width *= 1.0/mark.rank;
    width = floor(width);

    if (this->zoom.factor >= mark.zoom_level)
        rectf(Rect(x()+w() - width - 1, offset, width, mark.width), c);

    if (this->zoom.factor >= mark.name_zoom_level && this->show_names
            && mark.name)
    {
        // TODO draw name
    }
}


void
OverlayRuler::draw_marklists()
{
    Rect clip = clip_rect(rect(this));
    DEBUG("clip: " << clip);
    if (clip.w == 0 || clip.h == 0)
        return;
    // Later marklists will draw over earlier ones.
    for (Marklists::const_iterator mlist = model.marklists.begin();
            mlist != model.marklists.end(); ++mlist)
    {
        for (Marklist::const_iterator mark = (*mlist)->begin();
                mark != (*mlist)->end(); ++mark)
        {
            // mark is pair(trackpos, mark)
            int offset = y() + floor(mark->first * zoom.factor - zoom.offset);
            // mlist should be sorted
            // actually, check if it's in the clip range
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
RulerTrackView::draw()
{
    OverlayRuler::draw();
}
