#include "util.h"

#include "EventCollector.h"


static void
find_active(UiEvent &e)
{
    e.inside_block = 0;
    e.inside_track = 0;
    e.inside_event = 0;
    for (Fl_Window *win = Fl::first_window(); win; win = Fl::next_window(win)) {
        if (Fl::event_inside(win)) {
            e.inside_block = dynamic_cast<BlockViewWindow *>(win);
            break;
        }
    }
    if (!e.inside_block)
        return;
    for (int i = 0; i < e.inside_block->block.tracks(); i++) {
        TrackView *t = e.inside_block->block.track_at(i);
        if (Fl::event_inside(t)) {
            e.inside_track = t;
            break;
        }
    }
    if (!e.inside_track)
        return;
    // do events
}


void
EventCollector::collect(int evt)
{
    UiEvent e;
    e.event = evt;
    e.button = Fl::event_button();
    e.clicks = Fl::event_clicks();
    e.is_click = Fl::event_is_click();
    e.x = Fl::event_x();
    e.y = Fl::event_y();
    e.state = Fl::event_state();
    e.key = Fl::event_key();
    find_active(e);
    this->events.push_back(e);
    // DEBUG("pushing " << show_event(evt));
}


EventCollector *
global_event_collector()
{
    static EventCollector e;
    return &e;
}
