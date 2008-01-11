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

#include <FL/Fl_Box.H>

#include "util.h"

#include "types.h"
#include "Track.h"

struct Mark {
    Mark(int rank, int width, Color color, char *name, double name_zoom_level,
            double zoom_level) :
        rank(rank), width(width), color(color), name(name),
        name_zoom_level(name_zoom_level), zoom_level(zoom_level)
    {
        ASSERT(rank >= 0);
    }

    int rank;
    int width;
    Color color;
    char *name;
    double name_zoom_level;
    double zoom_level;
};

typedef std::vector<std::pair<TrackPos, Mark> > Marklist;
typedef std::vector<const Marklist *> Marklists;

// Markslists will be drawn in the order they are given, so later marklists
// will draw over earlier ones.
struct RulerTrackModel {
    RulerTrackModel(const Marklists &lists, Color bg) :
        marklists(lists), bg(bg)
    {}
    const Marklists &marklists;
    const Color bg;
};


class OverlayRuler : public TrackView {
public:
    OverlayRuler(const RulerTrackModel &ruler) : model(ruler) {}

protected:
    void draw();
    const RulerTrackModel &model;

private:
    void draw_marklists();
    void draw_mark(int offset, const Mark &mark);

    bool show_names;
    bool use_alpha;
    ZoomInfo zoom;
};


class RulerTrackView : public OverlayRuler {
public:
    RulerTrackView(const RulerTrackModel &ruler) :
        OverlayRuler(ruler), bg_box(0, 0, 1, 1)
    {
        bg_box.box(FL_THIN_DOWN_BOX);
        bg_box.color(color_to_fl(model.bg));
        this->add(bg_box);
    }

protected:
    void draw();

private:
    Fl_Box bg_box;
};

#endif
