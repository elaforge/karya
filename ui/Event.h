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
    char *title;
    TextStyle style;
    TrackPos duration;
    std::vector<SubEvent> subs;
    Color bg_color;
    // void *signal;
    // RenderStyle render_style;
};


class EventView : Fl_Box {
public:
    EventView(EventModel &event) :
        model(event),
        Fl_Box(0, 0, 1, 1, "e")
    {
        box(FL_FLAT_BOX);
        color(color_to_fl(model.bg_color));
    }

private:
    EventModel &model;
};

#endif
