/* Display a number of events and an overlay ruler.

    Events don't overlap.
*/

#ifndef __EVENT_TRACK_H
#define __EVENT_TRACK_H

#include "types.h"

#include "SeqInput.h"
#include "Ruler.h"
#include "Track.h"
#include "Event.h"


// TODO make these const, except test_block wants to initialize them...
struct ControlSample {
    RealTime time;
    double val;
    ControlSample(RealTime time, double val) : time(time), val(val) {}
};

struct TrackSignal {
    TrackSignal() : signal(NULL), length(0), is_pitch_signal(false) {}

    // The track containing the TrackSignal is responsible for the freeing of
    // the signal pointers.
    void free_signals();

    // This pointer could be null if the signal is empty.
    ControlSample *signal;
    // The max and min values in 'signal', to normalize the display.
    double val_min, val_max;
    // Length of above signal.
    int length;
    char is_pitch_signal;

    // These are to be applied to the signal's time values.
    ScoreTime shift;
    ScoreTime stretch;

    RealTime to_real(ScoreTime p) const {
        return (p.multiply(stretch) + shift).to_real();
    }
    ScoreTime from_real(RealTime p) const {
        return (ScoreTime::from_real(p) - shift).divide(stretch);
    }

    // Return the index of the sample before 'start', or 0.
    int find_sample(ScoreTime start) const;
    // Get the time at the given index, taking shift, stretch, and the given
    // zoom into account.
    int time_at(const ZoomInfo &zoom, int i) const;
    // Get the val at the given index, normalized between 0--1.
    double val_at(int i) const;

    // Set 'val_min' and 'val_max'.  Normally this would be called by the
    // constructor, but since I construct manually from haskell I don't have
    // one of those.
    void calculate_val_bounds();
};

std::ostream &operator<<(std::ostream &os, const TrackSignal &sig);

struct RenderConfig {
    enum RenderStyle {
        render_none,
        render_line,
        render_filled
    };
    RenderConfig(RenderStyle style, Color color) : style(style), color(color) {}

    RenderStyle style;
    Color color;
};

// TODO: as an optimization, I could cache the last set of found events plus
// one before start and one after end.  Then if the next draw_area is within
// that area (as it will be when scrolling smoothly) I can avoid the callback.
struct EventTrackConfig {
    // Get events from start to end, ordered by pos.  Return the ScoreTime in
    // pos, the events in 'events', and the count.
    typedef int (*FindEvents)(ScoreTime *start_pos, ScoreTime *end_pos,
            Event **ret_events, int **ret_ranks);

    // What to do about text that's too long.  If it's too long but there's
    // no room below, it's always clipped.
    enum text_wrap_style { clip, rotate, wrap };

    EventTrackConfig(Color bg_color, FindEvents find_events,
            ScoreTime time_end, RenderConfig render_config) :
        text_wrap(wrap), // hardcode for now
        bg_color(bg_color), find_events(find_events), time_end(time_end),
        render(render_config)
    {}
    // This should be a text_wrap_style, but it's easier to use from the
    // haskell FFI if it's an int.
    int text_wrap;
    Color bg_color;
    FindEvents find_events;
    ScoreTime time_end;

    RenderConfig render;
    TrackSignal track_signal;
};


class EventTrackView : public TrackView {
public:
    EventTrackView(const EventTrackConfig &config,
        const RulerConfig &ruler_config);
    void resize(int x, int y, int w, int h);
    // Y position of the track start.  Use this instead of y() to avoid
    // colliding with the track bevel.
    int track_start() { return overlay_ruler.track_start(); }
    virtual SeqInput &title_widget() { return *this->title_input; }
    virtual const char *get_title() const {
        return this->title_input->value();
    }
    virtual void set_title(const char *title);
    virtual void set_title_focus();
    void set_zoom(const ZoomInfo &new_zoom);
    virtual void set_selection(int selnum, int tracknum, const Selection &sel)
    {
        overlay_ruler.set_selection(selnum, tracknum, sel);
    }
    virtual void set_event_brightness(double d);
    virtual ScoreTime time_end() const;
    virtual void update(const Tracklike &track, ScoreTime start, ScoreTime end);
    // For the moment, only EventTracks can draw a signal.
    virtual void set_track_signal(const TrackSignal &tsig);
    virtual void finalize_callbacks();
    virtual std::string dump() const;

protected:
    void draw();

private:
    void draw_area();
    void draw_signal(int min_y, int max_y, ScoreTime start);
    IRect draw_upper_layer(int offset, const Event &event, int rank,
        int prev_offset, int next_offset, const IRect &prev_unranked_rect);

    EventTrackConfig config;
    ZoomInfo zoom;
    // Remember how much I've scrolled, to do fl_scroll() optimization.
    ScoreTime last_offset;
    double brightness;
    Color bg_color;

    SeqInput *title_input;
    // Created and destroyed when 'edit_open' is called.
    SeqInput *edit_input;
    Fl_Box bg_box;
    OverlayRuler overlay_ruler;
};

#endif
