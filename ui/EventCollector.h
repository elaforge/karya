#include <vector>

#include "Block.h"
#include "Track.h"


struct UiEvent {
    UiEvent() {}

    int event;
    int button, clicks, is_click, x, y;
    int state;
    int key;

    BlockViewWindow *inside_block;
    TrackView *inside_track;
    // TODO: later this should be the TrackPos of the event it's in
    bool inside_event;
    // TrackPos track_pos;
};


class EventCollector {
public:
    EventCollector() {}
    void collect(int evt);
    std::vector<UiEvent> events;
};

EventCollector *global_event_collector();
