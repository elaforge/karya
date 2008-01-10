#include "Track.h"

TrackView::TrackView(TrackModel &track) :
     bg_box(0, 0, 1, 1), model(track)
{
    end(); // make sure no one else falls in?
    add(bg_box);
    // create event widgets
    bg_box.box(FL_THIN_DOWN_BOX);
    bg_box.color(FL_WHITE);
}
