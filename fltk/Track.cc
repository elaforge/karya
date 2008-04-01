#include "Track.h"
#include "EventTrack.h"
#include "Ruler.h"


DividerView::DividerView(const DividerConfig &config) :
    TrackView("divider"), box(0, 0, 1, 1)
{
    box.box(FL_FLAT_BOX);
    box.color(color_to_fl(config.color));
    add(box);

    this->title_box = new Fl_Box(0, 0, 1, 1);
    title_box->box(FL_FLAT_BOX);
    title_box->color(color_to_fl(config.color));
}


void
DividerView::draw()
{
    Rect r = rect(this);
    r.h--;
    ClipArea c(r);
    TrackView::draw();
}
