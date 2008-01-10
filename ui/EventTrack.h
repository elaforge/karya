/*
*/

#ifndef __EVENT_TRACK_H
#define __EVENT_TRACK_H

#include <vector>

#include "Track.h"

// Models and views refer to each other.
class EventTrackView;

class EventTrackModel {
private:
    // events
    // std::vector<std::pair<Trackpos, EventModel> > events;
    // Views of this track, to update when it changes.
    std::vector<EventTrackView *> views;
};



class EventTrackView : public TrackView {
public:
    EventTrackView(EventTrackModel &track);

private:
    EventTrackModel &model;
    Fl_Box bg_box;
};

#endif
