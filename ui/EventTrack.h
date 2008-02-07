/*
*/

#ifndef __EVENT_TRACK_H
#define __EVENT_TRACK_H

#include <vector>

#include "types.h"

#include "SeqInput.h"
#include "Track.h"
#include "Event.h"

// Models and views refer to each other.
class EventTrackView;

class EventTrackModel {
public:
    EventTrackModel() {}
    ~EventTrackModel();
    // void register(EventTrackView *view) { views.push_back(view); }
    void deregister(EventTrackView *view) { } // views.erase(view); }
private:
    std::vector<std::pair<TrackPos, EventModel> > events;
    // Views of this track, to update when it changes.
    std::vector<EventTrackView *> views;
};



class EventTrackView : public TrackView {
public:
    EventTrackView(EventTrackModel *model);
    ~EventTrackView();
    virtual SeqInput &title_widget() { return *this->title_input; }

private:
    EventTrackModel *model;
    Fl_Box bg_box;
    SeqInput *title_input;
};

#endif
