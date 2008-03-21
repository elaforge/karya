#include <vector>

#include "Block.h"
#include "Track.h"

/*
Msgs:

Keystrokes:
If you click on an input (block or track title), it gets visually highlighted.
Keystrokes go to that input until you click outside or hit return or tab.

When focus leaves an input, it emits (InputChanged, BlockView, Maybe TrackNum)

Otherwise, keystrokes are emitted as UiMsgs.  The mouse position shouldn't
matter for these.

Mouse:
Clicks, drags, and releases on tracks are reported as UiMsgs.  The app can
use this to set selections or whatever.  Scrolling or zooming via the GUI
widgets is reported as SetZoom or SetTrackScroll.

Dragging a track boundary or the ruler edge is not reported.

Trying to close the window is reported as CloseBlock.


This is much simpler than fltk's model since the only widgets that get events
are inputs, and there is no keyboard navigation.


click:
Find the innermost widget under the pointer.  Offer it focus, and clear
existing focus if it doesn't accept.  Offer it FL_PUSH, and set pushed() on it
if it accepts.
If the widget didn't accept FL_PUSH, call a global handler.

drag:
If widget accepted FL_PUSH (i.e. is pushed()), send directly to that widget.
Otherwise, call the global handler.

kbd:
If a widget has focus, send directly to that widget.
If the widget didn't accept it, call the global handler.

All other events are ignored.

I also want to disable all of that keyboard arrow key navigation stuff, so
I think groups would just be responsible for finding which of their children
gets focus and pushed on FL_PUSH.

So I thought if I overrode Fl_Group::handle to only handle FL_PUSH


todo this:
blur() function sets focus to the window
window has a handle that collects KBD

window accepts push and drag if no one else did, and collects

if a child accepted PUSH, window won't get it and won't collect

make MoveTile not accept PUSH if it has a modifier, so I can expand dividers
with ^click or something

*/


// This struct is simple so it can be serialized to haskell.
struct UiMsg {
    // It's always initialized manually, but STL needs a default constructor.
    UiMsg() : view(0), has_track(false), has_pos(false), pos(0) {}

    enum MsgType {
        msg_event,
        msg_input,

        // block changed msgs all have 'view' set
        msg_track_scroll, msg_zoom, msg_view_resize,
        msg_track_width,
        msg_close
    };

    MsgType type;

    // Fields from the various fltk event_*() functions.
    int event;
    int button, clicks, is_click, x, y;
    int state;
    int key;

    // context
    // Block that event occurred in.
    BlockViewWindow *view;
    // Mouse was over this track.
    char has_track; // actually a bool, but haskell FFI doesn't support bools
    int track;
    // If it was over a ruler or event track, has_pos=true and it was at this
    // pos.
    char has_pos; // as has_track
    TrackPos pos;
};

std::ostream &operator<<(std::ostream &os, const UiMsg &m);


class MsgCollector {
public:
    MsgCollector() {}
    void event(int evt);
    // these are all const, but adding it causes const trickle
    void block_changed(Fl_Widget *w, UiMsg::MsgType type, int track = -1);
    void window_changed(BlockViewWindow *view, UiMsg::MsgType type,
            int track = -1);
    std::vector<UiMsg> msgs;

private:
    void push(const UiMsg &m) {
        msgs.push_back(m);
        // DEBUG("collected " << m);
    }
};

MsgCollector *global_msg_collector();
