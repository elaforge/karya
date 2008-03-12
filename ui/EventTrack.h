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

#include <map>
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
    EventTrackModel(Color bg_color) : bg_color(bg_color) {}
    ~EventTrackModel();
    void insert_event(TrackPos pos, const EventModel &event);
    void remove_event(TrackPos pos);

    // Should only be called by EventTrackView and ~EventTrackView.
    void add_view(EventTrackView *view) { views.push_back(view); }
    void remove_view(EventTrackView *view) {
        views.erase(std::remove(views.begin(), views.end(), view), views.end());
    }

    typedef std::map<TrackPos, EventModel> Events;
    Color bg_color;
    Events events;
private:
    // Views of this track, to update when it changes.
    std::vector<EventTrackView *> views;
};



class EventTrackView : public TrackView {
public:
    EventTrackView(boost::shared_ptr<EventTrackModel> model,
            boost::shared_ptr<RulerTrackModel> ruler_model);
    ~EventTrackView();
    void resize(int x, int y, int w, int h);
    virtual SeqInput &title_widget() { return *this->title_input; }
    void set_zoom(const ZoomInfo &zoom);

    // Only called by EventTrackModel
    void insert_event(TrackPos pos, const EventModel &event);
    void remove_event(TrackPos pos);

protected:
    void draw();

private:
    // Fl_Group draws from child 0 up.  bg_box should be drawn first, and
    // overlay_ruler last, so use accessors for the events to preserve that.
    int events() const { return children()-2; }
    EventView *event_at(int i) {
        return static_cast<EventView *>(child(i+1));
    }
    void create_widgets() { create_widgets(TrackPos(0), TrackPos::max_pos); }
    void create_widgets(TrackPos start, TrackPos duration);

    // A cache from model->view, so I can see which models have views quickly.
    std::map<EventModel *, EventView *> displayed_events;
    boost::shared_ptr<EventTrackModel> model;
    ZoomInfo zoom;
    SeqInput *title_input;
    Fl_Box bg_box;
    OverlayRuler overlay_ruler;
};

#endif
