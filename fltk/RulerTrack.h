// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

/* Rulers appear in two places: on a dedicated ruler track, and as an overlay
    on an event track.  The former can be implemented as an overlay onto a
    plain box.  There are some differences though: I may want to disable names
    for the event track overlay, and alpha for the ruler track.
*/

#ifndef __RULER_H
#define __RULER_H

#include <utility>
#include <vector>
#include <string>

#include <FL/Fl_Group.H>
#include <FL/Fl_Box.H>

#include "Color.h"
#include "Selection.h"
#include "SelectionOverlay.h"
#include "Track.h"
#include "config.h"
#include "types.h"


struct Mark {
    Mark() {}
    Mark(int rank, int width, Color color, char *name,
            double name_zoom_level, double zoom_level) :
        rank(rank), width(width), color(color), name(name),
        name_zoom_level(name_zoom_level), zoom_level(zoom_level)
    {
        ASSERT(rank >= 0);
    }

    int rank;
    int width;
    Color color;
    // std::string would be much nicer here, but you can't serialize to one of
    // those from haskell.
    char *name;
    double name_zoom_level;
    double zoom_level;
};

struct PosMark {
    PosMark(ScoreTime pos, Mark mark) : pos(pos), mark(mark) {}
    ScoreTime pos;
    Mark mark;
};

// Marklists are reference-counted because they are frequently large, but
// change rarely.  Haskell uses a ForeignPtr to hold one reference.
// More documentation in 'Ui.BlockC'.
class Marklist {
public:
    Marklist(const PosMark *marks, int length)
        : references(1), length(length), marks(marks) {}
    void incref();
    void decref();

private:
    int references;
public:
    const int length;
    const PosMark *marks;
};

typedef std::vector<Marklist *> Marklists;

// Markslists will be drawn in the order they are given, so later marklists
// will draw over earlier ones.
class RulerConfig {
public:
    // Initializing marklists by assignment is less convenient from c++, but
    // more convenient to serialize from haskell.
    RulerConfig(Color bg, bool show_names, bool use_alpha, bool full_width,
            bool align_to_bottom, ScoreTime last_mark_pos) :
        marklists(), bg(bg), show_names(show_names), use_alpha(use_alpha),
        full_width(full_width), align_to_bottom(align_to_bottom),
        last_mark_pos(last_mark_pos)
    {}
    Marklists marklists;

    // RulerTrack uses this to set the bg_box, an EventTrack's OverlayRuler
    // doesn't use it.
    Color bg;

    // So I can share marklists but have different display styles.
    char show_names;
    // Use the alpha channel in the Mark colors.  Otherwise, they are opaque.
    char use_alpha;
    // Always draw marks across the full width of the track.
    char full_width;
    // Align bottoms of marks to beats, instead of the top.  Looks good used
    // with negative duration events (arrival beats).
    char align_to_bottom;

    ScoreTime last_mark_pos;
};


class OverlayRuler : public Fl_Widget {
public:
    explicit OverlayRuler(const RulerConfig &config, bool is_ruler_track);
    ScoreTime time_end() const { return config.last_mark_pos; }
    void set_config(bool is_ruler_track, const RulerConfig &config,
        ScoreTime start, ScoreTime end);
    // Deallocate marklist memory.
    void delete_config();
    void set_zoom(const Zoom &zoom) { this->zoom = zoom; }

    RulerConfig config;

protected:
    void draw() override;

private:
    void draw_marklists();
    bool draw_mark(bool at_zero, int offset, const Mark &mark);

    Zoom zoom;
};


class RulerTrack : public Track {
public:
    explicit RulerTrack(const RulerConfig &config);
    virtual Fl_Box &title_widget() override;
    virtual void set_selection(int selnum, const std::vector<Selection> &sels)
        override;
    virtual ScoreTime time_end() const override { return ruler.time_end(); }
    virtual void update(const Tracklike &track, ScoreTime start, ScoreTime end)
        override;
    virtual void set_track_signal(const TrackSignal &tsig) override {
        DEBUG("WARNING: got a track signal on a ruler track!");
    }
    virtual void set_zoom(const Zoom &zoom) override {
        Track::set_zoom(zoom);
        ruler.set_zoom(zoom);
    }
    virtual void finalize_callbacks() override;
    virtual std::string dump() const override;

protected:
    void draw() override;

private:
    // If created, this is owned by a Fl_Group, which deletes it.
    Fl_Box *title_box;
    Fl_Box bg_box;
    OverlayRuler ruler;
    SelectionOverlay selection_overlay;
};

#endif
