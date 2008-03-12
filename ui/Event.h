/*
Events have a start and duration TrackPos.  The duration can be 0.

They have a number of Subs, which are (offset, text) pairs.

The Sub text tries to stay inside the Event if possible.
*/

#ifndef __EVENT_H
#define __EVENT_H

#include <vector>

#include "util.h"
#include "types.h"

struct SubEvent {
    TrackPos pos;
    char *title;
};

struct EventModel {
    // This has a default contsructor so I can assign it by value into
    // the EventTrackModel::Events map.
    EventModel() : duration(0), bg_color(255, 255, 255) {}
    EventModel(TrackPos duration, Color bg) :
        duration(duration), bg_color(bg)
    {}

    TextStyle style;
    TrackPos duration;
    Color bg_color;
    std::vector<SubEvent> subs;
    // void *signal;
    // RenderStyle render_style;
};


class EventView : public Fl_Box {
public:
    EventView(EventModel &event) :
        model(event),
        Fl_Box(0, 0, 1, 1, "e")
    {
        box(FL_FLAT_BOX);
        color(color_to_fl(model.bg_color));
    }

    EventModel &model;
private:
};

#endif
