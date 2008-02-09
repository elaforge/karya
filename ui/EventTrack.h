/*
*/

#ifndef __EVENT_TRACK_H
#define __EVENT_TRACK_H

#include <vector>
#include <boost/shared_ptr.hpp>

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
    void add_view(EventTrackView *view) { views.push_back(view); }
    void remove_view(EventTrackView *view) {
        views.erase(std::remove(views.begin(), views.end(), view), views.end());
    }
private:
    std::vector<std::pair<TrackPos, EventModel> > events;
    // Views of this track, to update when it changes.
    std::vector<EventTrackView *> views;
};



class EventTrackView : public TrackView {
public:
    EventTrackView(boost::shared_ptr<EventTrackModel> model);
    ~EventTrackView();
    virtual SeqInput &title_widget() { return *this->title_input; }

private:
    boost::shared_ptr<EventTrackModel> model;
    Fl_Box bg_box;
    SeqInput *title_input;
};

#endif
