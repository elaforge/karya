#include "SeqInput.h"
#include "EventTrack.h"


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
