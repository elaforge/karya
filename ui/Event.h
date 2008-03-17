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

struct SubEvent {
    SubEvent() : pos(0) {}
    SubEvent(TrackPos pos, const std::string &text) : pos(pos), text(text) {}
    TrackPos pos;
    std::string text;
};

class EventView;

struct EventModel {
    // This has a default contsructor so I can assign it by value into
    // the EventTrackModel::Events map.
    EventModel() : duration(0), bg_color(255, 255, 255) {}
    EventModel(TrackPos duration, Color bg, const SubEvent &sub) :
        duration(duration), bg_color(bg)
    {
        this->subs.push_back(sub);
    }
    void insert_sub(TrackPos pos, const std::string &text);
    void remove_sub(TrackPos pos);
    void update();

    void set_view(EventView *view) { this->view = view; }

    TextStyle style;
    TrackPos duration;
    Color bg_color;
    typedef std::vector<SubEvent> SubEvents;
    SubEvents subs;
    // float *signal;
    // RenderStyle render_style;
private:
    // Events only ever belong to one view, so they just have a single pointer.
    EventView *view;
};


class EventView : public Fl_Box {
public:
    EventView(EventModel *event) : Fl_Box(0, 0, 1, 1), model(event) {
        box(FL_FLAT_BOX);
        color(color_to_fl(model->bg_color));
    }
    void set_zoom(const ZoomInfo &zoom) { this->zoom = zoom; redraw(); }
    void update() { redraw(); }

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
