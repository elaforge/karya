#include "Track.h"
#include "EventTrack.h"
#include "Ruler.h"


static const boost::shared_ptr<EventTrackModel> null_track;
static const boost::shared_ptr<RulerTrackModel> null_ruler;
static const boost::shared_ptr<DividerModel> null_divider;

TrackModel::TrackModel(boost::shared_ptr<EventTrackModel> t,
        boost::shared_ptr<RulerTrackModel> r) :
    track(t), ruler(r), divider(null_divider) {}

TrackModel::TrackModel(boost::shared_ptr<RulerTrackModel> r) :
    track(null_track), ruler(r), divider(null_divider) {}

TrackModel::TrackModel(boost::shared_ptr<DividerModel> d) :
    track(null_track), ruler(null_ruler), divider(d) {}



DividerView::DividerView(boost::shared_ptr<DividerModel> model) :
    TrackView("divider"), box(0, 0, 1, 1)
{
    box.box(FL_FLAT_BOX);
    box.color(color_to_fl(model->color));
    add(box);

    this->title_box = new Fl_Box(0, 0, 1, 1);
    title_box->box(FL_FLAT_BOX);
    title_box->color(color_to_fl(model->color));
}


void
DividerView::draw()
{
    Rect r = rect(this);
    r.h--;
    ClipArea c(r);
    TrackView::draw();
}
