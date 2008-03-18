/*
Events have a start and duration TrackPos.  The duration can be 0.

They have a number of Subs, which are (offset, text) pairs.

The Sub text tries to stay inside the Event if possible.
*/

#ifndef __EVENT_H
#define __EVENT_H

#include <string>
#include <vector>

#include <FL/Fl_Box.H>

#include "util.h"
#include "types.h"


// Events are immutable.  They are never modified in place, so they don't need
// pointers to their views.  However, I can't actually make the members const
// since I assign them by value into STL containers.
struct EventModel {
    // This has a default contsructor so I can assign it by value into
    // the EventTrackModel::Events map.
    EventModel() : duration(0), color(0, 0, 0) {}
    EventModel(const std::string &text, TrackPos duration, Color color,
            const TextStyle &style) :
        text(text), duration(duration), color(color), style(style),
        align_to_top(true)
    {}

    std::string text;
    TrackPos duration;
    Color color;
    TextStyle style;
    // If true, align the text with the beginning of the event for western
    // style notation.  Otherwise, align it to the end, indonesian style.
    bool align_to_top;
    // float *signal;
    // RenderStyle render_style;
};


// Using a widget for an event may be too heavyweight.
// I don't need the separate coordinates and event delivery.  If I draw
// EventModels directly in the EventTrackView, I can avoid this overhead and
// also handle event collapsing when they're too close.
class EventView : public Fl_Box {
public:
    EventView(EventModel *event) : Fl_Box(0, 0, 1, 1), model(event) {
        box(FL_FLAT_BOX);
        color(color_to_fl(model->color));
    }
    void set_zoom(const ZoomInfo &zoom) { this->zoom = zoom; redraw(); }

    // view:event is 1:1, but the model is owned and deallocated by
    // EventTrackModel.
    EventModel *model;
protected:
    void draw();
    // EventTrackView calls draw_upper_layer() to draw the text over the ruler.
    friend class EventTrackView;
    void draw_upper_layer();

private:
    ZoomInfo zoom;
};

#endif
