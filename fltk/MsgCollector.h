// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

#include <vector>
#include <map>

#include "Block.h"
#include "Track.h"
#include "Zoom.h"


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

    When focus leaves an input, it emits msg_input.

    Otherwise, keystrokes are emitted as msg_event.  The mouse position
    shouldn't matter for these.

    Mouse:
    Clicks, drags, and releases on tracks are reported as msg_event.  The app
    can use this to set selections or whatever.  Scrolling via the GUI
    widgets or mousewheel is reported as msg_track_scroll or msg_time_scroll.

    Dragging a track boundary or the ruler edge is not reported.

    Trying to close the window is reported as msg_close.

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
        msg_track_scroll, msg_time_scroll, msg_resize,
        msg_track_width, msg_close,
        // One will be emitted for each screen on on startup and when screens
        // have been added or removed.
        msg_screen_size
    };
    // This array has to line up with enum above.
    static const char *msg_type_name(MsgType type) {
        static const char *names[] =
            { "event", "input"
            , "track_scroll", "time_scroll", "view_resize"
            , "track_width", "close"
            , "screen_size"
            };
        return names[type];
    }

    MsgType type; // Type tag.

    // Every msg has context.
    struct Context {
        Context() : focus(0), view(0), track_type(track_none), tracknum(0),
            has_pos(0), pos(0)
        {}

        // Window with focus.
        BlockWindow *focus;
        // Window to which this event applies, if any.  In haskell this is
        // given to the UiUpdates, but it's more of a hassle to nest types in
        // c++, so make it an optional field here.
        BlockWindow *view;
        // Source of the event.
        // This is a TrackType, but I want to explicitly control the size, for
        // haskell FFI.
        char track_type;
        int tracknum;
        // If it was over a ruler or event track, has_pos=true and it was at
        // this pos.
        char has_pos; // haskell FFI doesn't have bool
        ScoreTime pos;
    } context;
    // This goes in track_type and doubles as a boolean.  Since only click
    // events can apply to a divider, this can be interpreted as a boolean for
    // non-click events and only differentiate dividers for clicks.
    enum TrackType {
        track_none = 0,
        track_normal,
        // Combined with msg_input, this indicates that the input text
        // was from the floating_input, not the track title.
        track_floating_input,
        track_divider
    };

    // WARNING:
    // Union members can't have constructors, so make extra sure all fields
    // are initialized.
    //
    // I'd rather pass IRect and Zoom by value, but once again, no
    // constructors.  And it's not worth making one-off structs.

    // Fields from the various fltk event_*() functions, used for 'msg_event'.
    struct Event {
        int event;
        int button, clicks, x, y;
        char is_click;
        // The keycap that went down, including return, delete, etc.
        int key;
        // The text that this keycap represents, if any.  ASCII only, so this
        // doesn't support unicode input.
        char text;
        int modifier_state;
        char is_repeat;
    };

    // If context.has_track, this is a track title update, otherwise it's
    // the block title.
    struct Input {
        char *text;
    };

    struct TrackScroll {
        int scroll;
    };

    struct TimeScroll {
        ScoreTime scroll;
    };

    struct Resize {
        IRect *rect;
        Padding padding;
    };

    struct TrackWidth {
        int width;
        // From EventTrack::get_suggested_width()
        int suggested_width;
    };

    struct ScreenSize {
        IRect *rect;
        int screen, screens;
    };

    union {
        Event event;
        Input input;
        TrackScroll track_scroll;
        TimeScroll time_scroll;
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
    void focus(BlockWindow *focus);

    // Record an update with no Context.
    void update(UiMsg::MsgType type);
    // Record a msg from a block.  The Fl_Widget is any widget within the
    // msg's window.
    void block(UiMsg::MsgType type, Fl_Widget *w);
    // Record a msg from the given tracknum, where 'w' is within the msg's
    // window.
    void track(UiMsg::MsgType type, const Track *track);
    // Record msg_input for text changes.
    void track_title(const EventTrack *track, const char *text);
    void floating_input(Fl_Widget *w, const char *floating_input);
    void view(
        UiMsg::MsgType type, BlockWindow *view, const char *text = nullptr);

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
    // Clear out the keydown map manually.
    void all_keys_up();
    void key_up(int key);
    void clear();

    // If true, log all collected msgs for debugging.
    bool log_collected;

    static MsgCollector *get();
    // Callback for Fl::add_handler() to record global events.
    static int event_handler(int evt);

private:
    void push_update(UiMsg::MsgType type, const UiMsg::Context &c,
        const char *text = nullptr);
    void push(UiMsg &m);
    std::vector<UiMsg> msgs;

    // Keep track of which keys are down, to suppress spurious key ups.
    // Map Fl::event_key() to Fl::event_text().
    std::map<int, int> keys_down;
};
