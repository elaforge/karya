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
    UiMsg();

    // This is like the destructor, but isn't since this structure owns
    // pointers and is stored in a vector.  MsgCollect::clear should call this.
    void free();

    enum MsgType {
        // Fltk events.
        msg_event,
        // Text input changed, has view and maybe tracknum set.
        msg_input,

        // Block changed msgs all have 'view' set.  All these except
        // 'msg_close' are update notifications and may also have args in the
        // "update msg args" section.
        msg_track_scroll, msg_zoom, msg_view_resize,
        msg_track_width, msg_close
    };

    MsgType type;

    // Fields from the various fltk event_*() functions, used for 'msg_event'.
    int event;
    int button, clicks, is_click, x, y;
    int key;

    // Update msg args.  They're pointers to make haskell happy, but that means
    // I need to delete them in the destructor.
    char *update_text;
    // This is both the new width for msg_track_width and the scroll amount for
    // msg_track_scroll.
    int update_width;
    ZoomInfo *update_zoom;
    Rect *update_rect;

    // Every msg may have context.
    // Block that event occurred in.
    BlockViewWindow *view;
    // Mouse was over this track.
    char has_tracknum; // actually a bool, but haskell FFI doesn't support bools
    int tracknum;
    // If it was over a ruler or event track, has_pos=true and it was at this
    // pos.
    char has_pos; // as has_tracknum
    TrackPos pos;
};

std::ostream &operator<<(std::ostream &os, const UiMsg &m);


class MsgCollector {
public:
    MsgCollector() {}
    // Normally 'view' will be intuited from the mouse position, but
    // you can override that by passing it explicitly.
    void event(int evt, BlockViewWindow *view = 0);

    // There are 'block' and 'window' variants, with and without a tracknum.
    // The 'window' variant is necessary because the Fl_Widget::window() of a
    // window is not itself, but the parent window or NULL, and sometimes it's
    // more convenient to pass the BlockViewWindow directly.

    // 'view' and 'w' are const, but adding it causes const trickle.
    void block_update(Fl_Widget *w, UiMsg::MsgType type);
    void block_update(Fl_Widget *w, UiMsg::MsgType type, int tracknum);

    void window_update(BlockViewWindow *view, UiMsg::MsgType type);
    void window_update(BlockViewWindow *view, UiMsg::MsgType type,
            int tracknum);
    void window_update_resize(BlockViewWindow *view, const Rect &rect);

    UiMsg *msgs_ptr() {
        // The C++ standard says vector is supposed to use a contiguous array:
        // http://www.open-std.org/jtc1/sc22/wg21/docs/lwg-defects.html#69
        return &*msgs.begin();
    }
    int msgs_size() const { return msgs.size(); }
    void clear();

private:
    void push(const UiMsg &m);
    std::vector<UiMsg> msgs;
};

MsgCollector *global_msg_collector();
