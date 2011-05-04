#include <vector>
#include <set>

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


TODO:
blur() function sets focus to the window
window has a handle that collects KBD

window accepts push and drag if no one else did, and collects

if a child accepted PUSH, window won't get it and won't collect

make MoveTile not accept PUSH if it has a modifier, so I can expand dividers
with ^click or something

*/


// This struct is simple so it can be serialized to haskell.
//
// It's actually a union and the meaning of the various fields change
// depending on the 'type' field.
//
// TODO it would be easier to understand if I made bits actual unions, but
// I'm not totally sure what the FFI would think of that, and it's not so
// bad if the only code that cares is fltk/MsgCollector.cc and
// UiMsg/UiMsgC.hsc.
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
        msg_track_width, msg_close,
        // One will be emitted for each screen on on startup and when screens
        // have been added or removed.
        // Will set 'update_rect' to the screen size,
        // 'width_scroll_visible_track' to screen number, and 'visible_time' to
        // the total screens.
        msg_screen_size
    };
    static const char **msg_type_names() {
        static const char *names[] = { "event", "input",
            "track_scroll", "zoom", "view_resize",
            "track_width", "close",
            "screen_size" };
        return names;
    }

    // Type tag for this union.
    MsgType type;

    // Fields from the various fltk event_*() functions, used for 'msg_event'.
    int event;
    int button, clicks, is_click, x, y;
    int key;
    int modifier_state;
    char is_repeat;

    // Update msg args.  They're pointers to make haskell happy, but that means
    // I need to delete them in the destructor.
    char *update_text;
    // Set by msg_track_width, msg_track_scroll, and msg_view_resize.  This
    // should be a union but it's easier on the haskell side if it's not.
    int width_scroll_visible_track;
    // Also set by msg_view_resize.
    int visible_time;
    ZoomInfo *update_zoom;
    // Set by msg_view_resize.
    IRect *update_rect;

    // Every msg may have context.
    // Block that event occurred in.
    BlockViewWindow *view;
    // Mouse was over this track.
    char has_tracknum; // actually a bool, but haskell FFI doesn't support bools
    int tracknum;
    // If it was over a ruler or event track, has_pos=true and it was at this
    // pos.
    char has_pos; // as has_tracknum
    ScoreTime pos;
};

std::ostream &operator<<(std::ostream &os, const UiMsg &m);


class MsgCollector {
public:
    MsgCollector() : log_collected(false) {}
    // Normally 'view' will be intuited from the mouse position, but
    // you can override that by passing it explicitly.
    // 'track_drag' will assume you are dragging from a track and always
    // set the tracknum and pos.
    void event(int evt, BlockViewWindow *view = 0, bool track_drag = false);

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

    // Send one msg_screen_size msg for each screen.
    //
    // It's called on startup from Ui/c_interface.cc:initialize
    //
    // TODO this should be attached to a callback that gets called whenever
    // a screen is added or removed, but fltk doesn't support this.  I could
    // cache the screen count and constantly poll for changes, but fltk doesn't
    // even notice when screens change.
    // filed STR 2600
    void screen_update();

    UiMsg *msgs_ptr() {
        // The C++ standard says vector is supposed to use a contiguous array:
        // http://www.open-std.org/jtc1/sc22/wg21/docs/lwg-defects.html#69
        return &*msgs.begin();
    }
    int msgs_size() const { return msgs.size(); }
    void clear();

    // If true, log all collected msgs for debugging.
    bool log_collected;

private:
    void push(UiMsg &m);
    std::vector<UiMsg> msgs;

    // Keep track of which keys are down, to suppress spurious key ups.
    std::set<int> keys_down;
};

MsgCollector *global_msg_collector();
