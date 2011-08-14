#include <vector>
#include <set>

#include "Block.h"
#include "Track.h"

/* UiMsgs are how the fltk layer communicates with the haskell layer.

   Some events are handled directly by fltk widgets, but the rest are
   sent as fltk events.  There is also a set of msgs that are not fltk
   events and carry their own event-specific data with them.

   Various fltk event handlers will call the various MsgCollector msgs
   to put a msg in the global queue.  Every time the haskell side of the UI
   event loop cycles, it will drain the queue and forward those to the Cmd
   event loop (responder).

    Msgs:

    Keystrokes:
    If you click on an input (block or track title), it gets visually
    highlighted.  Keystrokes go to that input until you click outside or hit
    return or tab.

    When focus leaves an input, it emits (InputChanged, BlockView, Maybe
    TrackNum)

    Otherwise, keystrokes are emitted as UiMsgs.  The mouse position shouldn't
    matter for these.

    Mouse:
    Clicks, drags, and releases on tracks are reported as UiMsgs.  The app can
    use this to set selections or whatever.  Scrolling or zooming via the GUI
    widgets is reported as SetZoom or SetTrackScroll.

    Dragging a track boundary or the ruler edge is not reported.

    Trying to close the window is reported as CloseBlock.

    This is much simpler than fltk's model since the only widgets that get
    events are inputs, and there is no keyboard navigation.
*/

// This struct is simple so it can be serialized to haskell.
struct UiMsg {
    UiMsg() {}

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
        msg_track_scroll, msg_zoom, msg_resize,
        msg_track_width, msg_close,
        // One will be emitted for each screen on on startup and when screens
        // have been added or removed.
        msg_screen_size
    };
    static const char **msg_type_names() {
        static const char *names[] = { "event", "input",
            "track_scroll", "zoom", "view_resize",
            "track_width", "close",
            "screen_size" };
        return names;
    }

    MsgType type; // Type tag.

    // Every msg has context.
    struct Context {
        Context() : focus(0), view(0), has_tracknum(0), tracknum(0),
            has_pos(0), pos(0)
        {}

        // View with focus.
        BlockViewWindow *focus;
        // View to which this event applies, if any.  In haskell this is
        // given to the UiUpdates, but it's more of a hassle to nest types
        // in c++, so make it an optional field here.
        BlockViewWindow *view;
        // Mouse was over this track.
        // Actually a bool, but haskell FFI doesn't support bools.
        char has_tracknum;
        int tracknum;
        // If it was over a ruler or event track, has_pos=true and it was at
        // this pos.
        char has_pos; // as has_tracknum
        ScoreTime pos;
    } context;

    // WARNING:
    // Union members can't have default constructors, so make extra sure all
    // fields are initialized.

    // Fields from the various fltk event_*() functions, used for 'msg_event'.
    struct Event {
        int event;
        int button, clicks, x, y;
        char is_click;
        int key;
        int modifier_state;
        char is_repeat;
    };

    struct Resize {
        IRect *rect;
        int visible_track, visible_time;
    };

    struct Zoom {
        ZoomInfo *zoom;
    };

    struct TrackWidth {
        int width;
    };

    struct TrackScroll {
        int scroll;
    };

    // If context.has_track, this is a track title update, otherwise it's
    // the block title.
    struct Input {
        char *text;
    };

    struct ScreenSize {
        IRect *rect;
        int screen, screens;
    };

    union {
        Event event;
        Input input;
        TrackScroll track_scroll;
        Zoom zoom;
        Resize resize;
        TrackWidth track_width;
        ScreenSize screen;
    };
};

std::ostream &operator<<(std::ostream &os, const UiMsg &m);
std::ostream &operator<<(std::ostream &os, const UiMsg::Context &m);
std::ostream &operator<<(std::ostream &os, const UiMsg::Event &m);

class MsgCollector {
public:
    MsgCollector() : log_collected(false) {}
    // 'track_drag' will assume you are dragging from a track and always
    // set the tracknum and pos.
    void event(int evt, bool track_drag = false);
    // FL_FOCUS events are special because they are send *before* the
    // focus is changed.
    void focus(BlockViewWindow *focus);

    // Updates.  For updates that carry Context, there are methods to provide
    // the relevant data.
    void update(UiMsg::MsgType type);
    void block(UiMsg::MsgType type, Fl_Widget *w);
    void track(UiMsg::MsgType type, Fl_Widget *w, int tracknum);
    void view(UiMsg::MsgType type, BlockViewWindow *view);

    // Send one msg_screen_size msg for each screen.
    //
    // It's called on startup from Ui/c_interface.cc:initialize
    //
    // TODO this should be attached to a callback that gets called whenever a
    // screen is added or removed, but fltk doesn't support this.  I could
    // cache the screen count and constantly poll for changes, but fltk
    // doesn't even notice when screens change.
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

    static MsgCollector *get();

private:
    void push_update(UiMsg::MsgType type, const UiMsg::Context &c);
    void push(UiMsg &m);
    std::vector<UiMsg> msgs;

    // Keep track of which keys are down, to suppress spurious key ups.
    std::set<int> keys_down;
};
