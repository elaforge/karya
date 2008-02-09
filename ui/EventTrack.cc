#include "util.h"
#include "SeqInput.h"
#include "EventTrack.h"


EventTrackModel::~EventTrackModel()
{
    // Any remaining views should have kept this model alive.
    ASSERT(this->views.size() == 0);
}


EventTrackView::EventTrackView(boost::shared_ptr<EventTrackModel> model) :
     bg_box(0, 0, 1, 1), model(model)
{
    end(); // make sure no one else falls in
    add(bg_box);
    // create event widgets
    bg_box.box(FL_THIN_DOWN_BOX);
    bg_box.color(FL_WHITE);

    this->title_input = new SeqInput(0, 0, 1, 1);
    model->add_view(this);
}


EventTrackView::~EventTrackView()
{
    model->remove_view(this);
}
