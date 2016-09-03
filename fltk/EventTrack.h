// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

/* Display a number of events and an overlay ruler.

    Events don't overlap.
*/

#ifndef __EVENT_TRACK_H
#define __EVENT_TRACK_H

#include <vector>

#include "AbbreviatedInput.h"
#include "Event.h"
#include "FloatingInput.h"
#include "Ruler.h"
#include "Track.h"
#include "types.h"


// TODO make these const, except test_block wants to initialize them...
struct ControlSample {
    RealTime time;
    double val;
    ControlSample(RealTime time, double val) : time(time), val(val) {}
};

class TrackSignal {
public:
    TrackSignal() : signal(nullptr), val_min(0), val_max(0), length(0),
        shift(0), stretch(1)
    {}

    // The track containing the TrackSignal is responsible for the freeing of
    // the signal pointers.
    void free_signals();

    // This pointer could be null if the signal is empty.
    ControlSample *signal;
    // The max and min values in 'signal', to normalize the display.
    double val_min, val_max;
    // Length of above signal.
    int length;

    // These are to be applied to the signal's time values.
    ScoreTime shift;
    ScoreTime stretch;

    bool empty() const { return signal == nullptr; }
    RealTime to_real(ScoreTime p) const {
        return (p.multiply(stretch) + shift).to_real();
    }
    ScoreTime from_real(RealTime p) const {
        return (ScoreTime::from_real(p) - shift).divide(stretch);
    }

    // Return the index of the sample before 'start', or 0.
    int find_sample(ScoreTime start) const;
    // Get the val at the given index, normalized between 0--1.
    double val_at(int i) const;
    // RealTime at the given index.
    RealTime time_at(int i) const;
    // Get the time pixel at the given index, taking shift, stretch, and the
    // given zoom into account.
    int pixel_time_at(const ZoomInfo &zoom, int i) const;

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
class EventTrackConfig {
public:
    // Get events from start to end, ordered by pos.  Return the ScoreTime in
    // pos, the events in 'events', and the count.
    typedef int (*FindEvents)(ScoreTime *start_pos, ScoreTime *end_pos,
            Event **ret_events, int **ret_ranks);

    // What to do about text that's too long.  If it's too long but there's
    // no room below, it's always clipped.
    enum text_wrap_style { clip, wrap };

    EventTrackConfig(Color bg_color, FindEvents find_events,
            ScoreTime time_end, RenderConfig render_config) :
        text_wrap(wrap), // hardcode for now
        bg_color(bg_color), find_events(find_events), time_end(time_end),
        render(render_config), track_signal()
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


// A Track with events on it.  It will call its callback when the title has
// been edited.
class EventTrack : public Track {
public:
    EventTrack(const EventTrackConfig &config, const RulerConfig &ruler_config);
    void resize(int x, int y, int w, int h) override;
    // Y position of the track start.  Use this instead of y() to avoid
    // colliding with the track bevel.
    int track_start() { return overlay_ruler.track_start(); }
    virtual Fl_Widget &title_widget() override { return title_input; }
    virtual const char *get_title() const override {
        return title_input.value();
    }
    virtual void set_title(const char *title) override;
    virtual void set_title_focus() override;
    void set_zoom(const ZoomInfo &new_zoom) override;
    virtual void set_selection(
        int selnum, int tracknum, const std::vector<Selection> &sels) override {
        overlay_ruler.set_selection(selnum, tracknum, sels);
    }
    virtual void set_event_brightness(double d) override;
    virtual ScoreTime time_end() const override;
    virtual void update(const Tracklike &track, ScoreTime start, ScoreTime end)
        override;
    // For the moment, only EventTracks can draw a signal.
    virtual void set_track_signal(const TrackSignal &tsig) override;
    virtual void finalize_callbacks() override;
    virtual std::string dump() const override;

protected:
    void draw() override;

private:
    void draw_area();
    void draw_signal(int min_y, int max_y, ScoreTime start);
    void draw_event_boxes(
        const Event *events, const int *ranks, int count,
        const std::vector<int> &offsets);
    std::pair<IRect, IRect> draw_upper_layer(
        int offset,
        const Event &event, int rank, int prev_offset, int next_offset,
        const IRect &prev_unranked_rect, const IRect &prev_ranked_rect);
    void static title_input_focus_cb(Fl_Widget *_w, void *arg);
    void static floating_input_done_cb(Fl_Widget *_w, void *arg);
    void focus_title();
    void unfocus_title();

    EventTrackConfig config;
    ZoomInfo zoom;
    // Remember how much I've scrolled, to do fl_scroll() optimization.
    // TODO but not anymore
    ScoreTime last_offset;
    double brightness;
    Color bg_color;

    // This only ever displays the text, since as soon as you try to focus on
    // it, 'floating_input' will pop up in front.
    AbbreviatedInput title_input;
    Fl_Box bg_box;
    OverlayRuler overlay_ruler;
    FloatingInput *floating_input;
};

#endif
