#include "util.h"
#include "SeqInput.h"
#include "EventTrack.h"


EventTrackModel::~EventTrackModel()
{
    // Clients should remove all these views from their tracks before
    // deleting the model.  Otherwise, this method would have to track down
    // the BlockViews and remove the tracks itself.
    ASSERT(this->views.size() == 0);
}


EventTrackView::EventTrackView(EventTrackModel &track) :
     bg_box(0, 0, 1, 1), model(track)
{
    end(); // make sure no one else falls in
    add(bg_box);
    // create event widgets
    bg_box.box(FL_THIN_DOWN_BOX);
    bg_box.color(FL_WHITE);

    this->title_input = new SeqInput(0, 0, 1, 1);
}
