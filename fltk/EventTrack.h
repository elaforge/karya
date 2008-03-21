/*
Display a number of events and an overlay ruler.

Events don't overlap.

To save from having to store a potentially large number of events, the track
only creates widgets for events that are currently visible.  When they are
scrolled into view, they are created, and when they are scrolled out of view,
destroyed.

Events should be stored so that it's pretty fast to insert new ones in any
place, though most insertion will probably be at the end, and is fast to get
them out in sorted order, starting at a certain TrackPos.  I'm using std::map
for now, I should find out if it behaves well if it's usually appended to.

It might be worthwhile to have a specialized version for controller tracks that
stores events as a list of (TrackPos, Double).  This is because controller
events can be very numerous (from recording continuous data), but don't have
any data other than the value.
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
    // Insert fails if the previous event overlaps 'pos', or if 'event's
    // duration extends past the beginning of the next event.
    // If an event is already at 'pos, it will be replaced.
    bool insert_event(TrackPos pos, const EventModel &event);
    // Remove fails if there is no event at 'pos'.
    bool remove_event(TrackPos pos);

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


// Calls the callback when events are inserted or removed, so parents can
// e.g. update scrollbars based on the new boundaries.
class EventTrackView : public TrackView {
public:
    EventTrackView(boost::shared_ptr<EventTrackModel> model,
            boost::shared_ptr<RulerTrackModel> ruler_model);
    ~EventTrackView();
    void resize(int x, int y, int w, int h);
    virtual SeqInput &title_widget() { return *this->title_input; }
    void set_zoom(const ZoomInfo &zoom);
    virtual void set_selection(int selnum, Color c, const Selection &sel) {
        overlay_ruler.set_selection(selnum, c, sel);
    }
    virtual TrackPos time_end() const;

    // Only called by EventTrackModel
    void insert_event(TrackPos pos, const EventModel &event);
    void remove_event(TrackPos pos);

protected:
    void draw();

private:
    void draw_area(Rect area);
    void draw_upper_layer(int offset, const EventModel &event);

    boost::shared_ptr<EventTrackModel> model;
    ZoomInfo zoom;
    SeqInput *title_input;
    Fl_Box bg_box;
    OverlayRuler overlay_ruler;
};

#endif
