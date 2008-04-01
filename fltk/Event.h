/*
Events have a start and duration TrackPos.  The duration can be 0.
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
    Event(const std::string &text, TrackPos duration, Color color,
            const TextStyle &style, bool align_to_bottom = false) :
        text(text), duration(duration), color(color), style(style),
        align_to_bottom(align_to_bottom)
    {}

    std::string text;
    TrackPos duration;
    Color color;
    TextStyle style;
    // Align the text with the beginning of the event for western style
    // notation or align it to the end, indonesian style.
    bool align_to_bottom;
    // float *signal;
    // RenderStyle render_style;
};

#endif
