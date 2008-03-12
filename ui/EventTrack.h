/*
Display a number of events and an overlay ruler.

Events don't overlap.

To save from having to store a potentially large number of events, the track
only creates widgets for events that are currently visible.  When they are
scrolled into view, they are created, and when they are scrolled out of view,
destroyed.
*/

#ifndef __EVENT_TRACK_H
#define __EVENT_TRACK_H

#include <vector>
#include <boost/shared_ptr.hpp>

#include "types.h"

#include "SeqInput.h"
#include "Ruler.h"
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

    typedef std::vector<std::pair<TrackPos, EventModel> > EventList;
private:
    EventList events;
    // Views of this track, to update when it changes.
    std::vector<EventTrackView *> views;
};



class EventTrackView : public TrackView {
public:
    EventTrackView(boost::shared_ptr<EventTrackModel> model,
            boost::shared_ptr<RulerTrackModel> ruler_model);
    ~EventTrackView();
    virtual SeqInput &title_widget() { return *this->title_input; }

private:
    boost::shared_ptr<EventTrackModel> model;
    ZoomInfo zoom;
    SeqInput *title_input;
    OverlayRuler overlay_ruler;
        Fl_Box bg_box;
};

#endif
