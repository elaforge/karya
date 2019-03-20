// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

/* Display a number of events and an overlay ruler.

    Events don't overlap.
*/

#pragma once

#include <string>
#include <memory>
#include <vector>

#include "AbbreviatedInput.h"
#include "Event.h"
#include "FloatingInput.h"
#include "PeakCache.h"
#include "RulerOverlay.h"
#include "SelectionOverlay.h"
#include "TimeVector.h"
#include "Track.h"
#include "Zoom.h"
#include "global.h"


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
    int pixel_time_at(const Zoom &zoom, int i) const;

    // Set 'val_min' and 'val_max'.  Normally this would be called by the
    // constructor, but since I construct manually from haskell I don't have
    // one of those.
    void calculate_val_bounds(const char *track_name);
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
    typedef int (*FindEvents)(
        const ScoreTime *start_pos, const ScoreTime *end_pos,
        Event **ret_events, int **ret_ranks);

    EventTrackConfig(Color bg_color, FindEvents find_events,
            ScoreTime time_end, RenderConfig render_config) :
        bg_color(bg_color), find_events(find_events), time_end(time_end),
        render(render_config), track_signal()
    {}
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
    virtual Fl_Widget &title_widget() override { return title_input; }
    virtual const char *get_title() const override {
        return title_input.value();
    }
    virtual void set_title(const char *title) override;
    virtual void set_title_focus() override;
    virtual void set_selection(int selnum, const std::vector<Selection> &sels)
        override;
    virtual void set_event_brightness(double d) override;
    virtual ScoreTime time_end() const override;
    virtual void update(const Tracklike &track, ScoreTime start, ScoreTime end)
        override;
    // For the moment, only EventTracks can draw a signal.
    virtual void set_track_signal(const TrackSignal &tsig) override;
    // Set waveform for this chunknum.  Under the assumption that I only ever
    // set waveforms in increasing chunknum, each set will clear the ones above
    // it, to avoid being left with stale chunks at the end.
    virtual void set_waveform(int chunknum, const PeakCache::Params &params)
        override;
    virtual void finalize_callbacks() override;
    virtual std::string dump() const override;

    // Whether text is aligned to the left or right side of the track.  Left
    // aligned text has higher drawing priority.
    enum Align { Left, Right };
    struct TextBox {
        TextBox(std::vector<std::pair<std::string, IRect>> lines, Align align)
            : lines(lines), align(align)
        {}
        TextBox() : lines(), align(Left) {}
        std::vector<std::pair<std::string, IRect>> lines;
        Align align;
    };
protected:
    void draw() override;

private:
    void draw_area();
    void draw_waveforms(int min_y, int max_y, ScoreTime start);
    void draw_signal(int min_y, int max_y, ScoreTime start);
    void draw_event_boxes(
        const Event *events, const int *ranks, int count,
        const std::vector<int> &offsets);
    void draw_upper_layer(
        int index, const Event *events, Align align,
        const std::vector<TextBox> &boxes, const std::vector<int> &triggers);
    void static title_input_focus_cb(Fl_Widget *_w, void *arg);
    void static floating_input_done_cb(Fl_Widget *_w, void *arg);
    void focus_title();
    void unfocus_title();

    EventTrackConfig config;
    double brightness;
    Color bg_color;
    // Downsampled waveform peak cache for each chunk, indexed by chunknum.
    std::vector<std::unique_ptr<PeakCache::MixedEntry>> peak_entries;

    // Keep track of the maximum peak.  I scale peaks automatically because the
    // the track is narrow so I want to see as much detail as possible, but
    // I still want to see relative loudness differences.
    //
    // I used to have a global peak in PeakCache, but a single block with high
    // amplitude would make the rest become tiny.
    float max_peak;

    // This only ever displays the text, since as soon as you try to focus on
    // it, 'floating_input' will pop up in front.
    AbbreviatedInput title_input;
    Fl_Box bg_box;
    RulerOverlay ruler_overlay;
    SelectionOverlay selection_overlay;
    FloatingInput *floating_input;
};

std::ostream &operator<<(std::ostream &os, const EventTrack::TextBox &box);
