/*
Events have a start and duration ScoreTime.  The duration can be 0.
*/

#ifndef __EVENT_H
#define __EVENT_H

#include <string>
#include <vector>

#include <FL/Fl_Box.H>

#include "util.h"
#include "types.h"


// Events are immutable.  They are never modified in place, so they don't need
// pointers to their views.
// I can't actually make the members const since I assign them by value into
// STL containers.
struct Event {
    // This has a default contsructor so I can assign it by value into
    // the EventTrackModel::Events map.
    Event() : duration(0), color(0, 0, 0) {}
    Event(char *text, ScoreTime duration, Color color,
            const TextStyle &style, bool align_to_bottom = false) :
        text(text), duration(duration), color(color), style(style)
    {}
    bool is_negative() const {
        return duration < ScoreTime(0) || duration.negative_zero();
    }

    // std::string would be nicer but I can't serialize to that from haskell.
    char *text;
    ScoreTime duration;
    Color color;
    TextStyle style;
};

#endif
