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


struct RenderConfig {
    // Get samples from the one before start_pos to the one at or after
    // end_pos.  'ret_tps' should be in ascending order.
    // TODO start and end are actually const, but it's too much bother to
    // convert them now.
    typedef int (*FindSamples)(TrackPos *start_pos, TrackPos *end_pos,
            TrackPos **ret_tps, double **ret_samples);
    enum RenderStyle {
        render_none,
        render_line,
        render_filled
    };

    RenderConfig(RenderStyle style, FindSamples find_samples, Color color) :
        style(style), color(color), find_samples(find_samples)
    {}
    RenderStyle style;
    Color color;
    // Samples should be within 0--1 inclusive.
    FindSamples find_samples;
};

// TODO: as an optimization, I could cache the last set of found events plus
// one before start and one after end.  Then if the next draw_area is within
// that area (as it will be when scrolling smoothly) I can avoid the callback.
struct EventTrackConfig {
    // Get events from start to end, ordered by pos.  Return the TrackPos in
    // pos, the events in 'events', and the count.
    typedef int (*FindEvents)(TrackPos *start_pos, TrackPos *end_pos,
            TrackPos **ret_tps, Event **ret_events, int **ret_ranks);

    EventTrackConfig(Color bg_color, FindEvents find_events,
            TrackPos time_end, RenderConfig render_config) :
        bg_color(bg_color), find_events(find_events), time_end(time_end),
        render(render_config)
    {}
    Color bg_color;
    FindEvents find_events;
    TrackPos time_end;

    RenderConfig render;
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
    void set_zoom(const ZoomInfo &new_zoom);
    virtual void set_selection(int selnum, int tracknum, const Selection &sel) {
        overlay_ruler.set_selection(selnum, tracknum, sel);
    }
    virtual void set_event_brightness(double d);
    virtual TrackPos time_end() const;
    virtual void update(const Tracklike &track, FinalizeCallback finalizer,
            TrackPos start, TrackPos end);
    virtual void finalize_callbacks(FinalizeCallback finalizer);

protected:
    void draw();

private:
    void draw_area();
    void draw_samples(TrackPos start, TrackPos end);
    void draw_upper_layer(int offset, const Event &event, int rank,
            Rect *previous, int *ranked_bottom);

    EventTrackConfig config;
    ZoomInfo zoom;
    // Remember how much I've scrolled, to do fl_scroll() optimization.
    TrackPos last_offset;
    double brightness;
    Color bg_color;

    SeqInput *title_input;
    Fl_Box bg_box;
    OverlayRuler overlay_ruler;
};

#endif
