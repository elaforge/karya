/*
Rulers appear in two places: on a dedicated ruler track, and as an overlay on
an event track.  The former can be implemented as an overlay onto a plain box.
There are some differences though: I may want to disable names for the event
track overlay, and alpha for the ruler track.

*/

#ifndef __RULER_H
#define __RULER_H

#include <utility>
#include <vector>
#include <string>

#include <FL/Fl_Group.H>
#include <FL/Fl_Box.H>

#include "util.h"
#include "config.h"

#include "types.h"
#include "Track.h"


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

struct Marklist {
    // Get marks from start to end.  Return the ScoreTime in pos, the events in
    // 'marks', and the count.
    typedef int (*FindMarks)(ScoreTime *start_pos, ScoreTime *end_pos,
            ScoreTime **ret_tps, Mark **ret_marks);
    Marklist(FindMarks find_marks) : find_marks(find_marks) {}
    FindMarks find_marks;
};

typedef std::vector<Marklist> Marklists;

// Markslists will be drawn in the order they are given, so later marklists
// will draw over earlier ones.
struct RulerConfig {
    // Initializing marklists by assignment is less convenient from c++, but
    // more convenient to serialize from haskell.
    RulerConfig(Color bg, bool show_names, bool use_alpha, bool full_width,
            bool align_to_bottom, ScoreTime last_mark_pos) :
        bg(bg), show_names(show_names), use_alpha(use_alpha),
        full_width(full_width), align_to_bottom(align_to_bottom),
        last_mark_pos(last_mark_pos)
    {}
    Marklists marklists;

    // RulerTrackView uses this to set the bg_box, an EventTrack's OverlayRuler
    // doesn't use it.
    Color bg;

    // So I can share marklists but have different display styles.
    bool show_names;
    bool use_alpha;
    // Always draw marks across the full width of the track.
    bool full_width;
    // Align bottoms of marks to beats, instead of the top.  Looks good used
    // with negative duration events (arrival beats).
    bool align_to_bottom;

    ScoreTime last_mark_pos;
};


class OverlayRuler : public Fl_Widget {
public:
    explicit OverlayRuler(const RulerConfig &config) :
        Fl_Widget(0, 0, 1, 1), config(config)
    {}
    void set_zoom(const ZoomInfo &new_zoom) { this->zoom = new_zoom; }
    void set_selection(int selnum, int tracknum, const Selection &sel);
    ScoreTime time_end() const;
    void set_config(const RulerConfig &config, FinalizeCallback finalizer,
            ScoreTime start, ScoreTime end);
    void finalize_callbacks(FinalizeCallback finalizer);
    // Mark a segment of the track as needing to be redrawn.
    // Only public so that EventTrack::draw can call it.
    void damage_range(ScoreTime start, ScoreTime end);

    enum { DAMAGE_RANGE = FL_DAMAGE_USER1 };
    // This area needs to be redrawn.
    Rect damaged_area;

    RulerConfig config;

    // Remember how much I've scrolled, to do fl_scroll() optimization.
    ScoreTime last_offset;
    ZoomInfo zoom;
protected:
    void draw();

private:
    void draw_marklists();
    void draw_mark(int offset, const Mark &mark);
    void draw_selections();
    TrackSelection selections[Config::max_selections];
};


class RulerTrackView : public TrackView {
public:
    explicit RulerTrackView(const RulerConfig &config);
    virtual Fl_Box &title_widget();
    virtual void set_zoom(const ZoomInfo &zoom);
    virtual void set_selection(int selnum, int tracknum, const Selection &sel) {
        ruler.set_selection(selnum, tracknum, sel);
    }
    virtual ScoreTime time_end() const { return ruler.time_end(); }
    virtual void update(const Tracklike &track, FinalizeCallback finalizer,
            ScoreTime start, ScoreTime end);
    virtual void finalize_callbacks(FinalizeCallback finalizer) {
        ruler.finalize_callbacks(finalizer);
    }

protected:
    void draw();

private:
    // If created, this is owned by a Fl_Group, which deletes it.
    Fl_Box *title_box;
    Fl_Box bg_box;
    OverlayRuler ruler;
};

#endif
