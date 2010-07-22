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


struct ValName {
    // These are created by haskell and are read-only from here.
    // TODO should be constant but then it's hard to initialize an array
    double val;
    char *name;
    ValName(double val, char *name) : val(val), name(name) {}
    ValName() : val(0), name(NULL) {}
};

struct TrackSignal {
    struct ControlSample {
        ScoreTime time;
        double val;
        ControlSample(ScoreTime time, double val) : time(time), val(val) {}
    };
    struct PitchSample {
        ScoreTime time;
        float from, to, at;
        PitchSample(ScoreTime time, float from, float to, float at)
            : time(time), from(from), to(to), at(at)
        {}
    };
    TrackSignal() :
        signal(NULL), pitch_signal(NULL), length(0), val_names(NULL),
        val_names_length(0)
    {}

    // The track containing the TrackSignal is responsible for the freeing of
    // the signal pointers.
    void free_signals() {
        if (signal)
            free(signal);
        if (pitch_signal)
            free(pitch_signal);
        if (val_names) {
            // DEBUG("free valnames " << val_names << " " << val_names_length);
            for (int i = 0; i < val_names_length; i++)
                free(val_names[i].name);
            free(val_names);
        }
    }

    // One of these pointers should be null.
    ControlSample *signal;
    PitchSample *pitch_signal;
    // Length of above signal.
    int length;

    ValName *val_names;
    int val_names_length;

    // These are to be applied to the signal's time values.
    ScoreTime shift;
    ScoreTime stretch;

    // Return the index of the sample before 'start', or 0.
    int find_sample(ScoreTime start) const;
    // Get the time at the given index, taking shift, stretch, and the given
    // zoom into account.
    int time_at(const ZoomInfo &zoom, int i) const;
    bool has_labels() const { return val_names; }
    // Get the val at the given index, normalized between 0--1.
    double val_at(int i, const char **lower, const char **upper) const;
    const ValName *name_of(double val, bool lower) const;
};

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
            ScoreTime **ret_tps, Event **ret_events, int **ret_ranks);

    EventTrackConfig(Color bg_color, FindEvents find_events,
            ScoreTime time_end, RenderConfig render_config) :
        bg_color(bg_color), find_events(find_events), time_end(time_end),
        render(render_config)
    {}
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
    virtual ScoreTime time_end() const;
    virtual void update(const Tracklike &track, FinalizeCallback finalizer,
            ScoreTime start, ScoreTime end);
    // For the moment, only EventTracks can draw a signal.
    virtual void set_track_signal(const TrackSignal &tsig);
    virtual void finalize_callbacks(FinalizeCallback finalizer);

protected:
    void draw();

private:
    void draw_area();
    void draw_signal(int min_y, int max_y, ScoreTime start);
    void draw_upper_layer(int offset, const Event &event, int rank,
            IRect *previous, int *ranked_bottom, int prev_offset);

    EventTrackConfig config;
    ZoomInfo zoom;
    // Remember how much I've scrolled, to do fl_scroll() optimization.
    ScoreTime last_offset;
    double brightness;
    Color bg_color;

    SeqInput *title_input;
    Fl_Box bg_box;
    OverlayRuler overlay_ruler;
};

#endif
