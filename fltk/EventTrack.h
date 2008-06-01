/*
Display a number of events and an overlay ruler.

Events don't overlap.
*/

#ifndef __EVENT_TRACK_H
#define __EVENT_TRACK_H

#include "types.h"

#include "SeqInput.h"
#include "Ruler.h"
#include "Track.h"
#include "Event.h"


// TODO: as an optimization, I could cache the last set of found events plus
// one before start and one after end.  Then if the next draw_area is within
// that area (as it will be when scrolling smoothly) I can avoid the callback.
struct EventTrackConfig {
    // Get events from start to end.  Return the TrackPos in pos, the events in
    // 'events', and the count.
    typedef int (*FindEvents)(TrackPos *start_pos, TrackPos *end_pos,
            TrackPos **ret_tps, Event **ret_events);
    // Get last trackpos, return 0 if the track is empty and there is none.
    typedef int (*LastTrackPos)(TrackPos *last);

    EventTrackConfig(Color bg_color, FindEvents find_events,
            LastTrackPos last_track_pos) :
        bg_color(bg_color), find_events(find_events),
        last_track_pos(last_track_pos)
    {}
    Color bg_color;
    FindEvents find_events;
    LastTrackPos last_track_pos;
};


class EventTrackView : public TrackView {
public:
    EventTrackView(const EventTrackConfig &config,
            const RulerConfig &ruler_config);
    void resize(int x, int y, int w, int h);
    virtual SeqInput &title_widget() { return *this->title_input; }
    virtual const char *get_title() const { return this->title_input->value(); }
    virtual void set_title(const char *title) {
        this->title_input->set_text(title);
    }
    void set_zoom(const ZoomInfo &zoom);
    virtual void set_selection(int selnum, int tracknum, const Selection &sel) {
        overlay_ruler.set_selection(selnum, tracknum, sel);
    }
    virtual TrackPos time_end() const;
    virtual void update(const Tracklike &track, FinalizeCallback finalizer,
            TrackPos start, TrackPos end);
    virtual void finalize_callbacks(FinalizeCallback finalizer);

protected:
    void draw();

private:
    void draw_area(Rect area);
    void draw_upper_layer(int offset, const Event &event);

    EventTrackConfig config;
    ZoomInfo zoom;
    SeqInput *title_input;
    Fl_Box bg_box;
    OverlayRuler overlay_ruler;
};

#endif
