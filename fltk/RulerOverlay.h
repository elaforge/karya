// Copyright 2016 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

#include "Color.h"
#include "Zoom.h"
#include "f_util.h"

#include "global.h"


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

    // RulerTrack uses this to set the bg_box, an EventTrack's RulerOverlay
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


class RulerOverlay {
public:
    explicit RulerOverlay(const RulerConfig &config, bool is_ruler_track);
    ScoreTime time_end() const { return config.last_mark_pos; }
    void set_config(bool is_ruler_track, const RulerConfig &config);
    // Deallocate marklist memory.
    void delete_config();
    void draw(const IRect &box, const Zoom &zoom, const IRect &clip);

    // TODO public to get at config.bg
    RulerConfig config;
private:
    bool draw_mark(
        const IRect &box, const Zoom &zoom, bool at_zero, int offset,
        const Mark &mark);
};
