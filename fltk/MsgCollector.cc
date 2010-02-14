#include <string.h>
#include <ctype.h>

#include "util.h"

#include "MsgCollector.h"

// UiMsg /////////////////////

UiMsg::UiMsg() :
    // Update args
    update_text(0), width_scroll_visible_track(0), visible_time(0),
    update_zoom(0), update_rect(0),
    // Context
    view(0), has_tracknum(false), has_pos(false), pos(0)
{}

void
UiMsg::free()
{
    if (update_text)
        delete[] update_text;
    if (update_zoom)
        delete update_zoom;
    if (update_rect)
        delete update_rect;
}

inline std::ostream &
operator<<(std::ostream &os, const UiMsg &m)
{
    // Keep this up to date with UiMsg::MsgType
    static const char *msg_type_names[] = { "event", "input",
        "track_scroll", "zoom", "view_resize",
        "track_width", "close" };

    os << '<' << msg_type_names[m.type];
    if (m.type == UiMsg::msg_event) {
        os << "=" << show_event(m.event)
            << " key=" << show_key(m.key)
            << " button=" << m.button << " clicks=" << m.clicks
            << " is_click=" << m.is_click
            << " xy=(" << m.x << ", " << m.y << ")";
    }

    if (m.update_text)
        os << " text=\"" << m.update_text << '"';
    if (m.type == UiMsg::msg_view_resize) {
        os << " track=(" << m.width_scroll_visible_track
            << ", " << m.visible_time << ")";
    } else {
        os << " width=" << m.width_scroll_visible_track;
    }
    if (m.update_zoom)
        os << " zoom=" << *m.update_zoom;
    if (m.update_rect)
        os << " rect=" << *m.update_rect;

    if (m.view)
        os << " view=\"" << m.view->block.get_title() << '"';
    if (m.has_tracknum)
        os << " tracknum=" << m.tracknum;
    if (m.has_pos)
        os << " pos=" << m.pos;
    os << '>';
    return os;
}


static void
set_msg_context(BlockViewWindow *view, bool track_drag, UiMsg &m)
{
    if (view) {
        m.view = view;
    } else {
        for (Fl_Window *win = Fl::first_window(); win;
                win = Fl::next_window(win))
        {
            // Events are reported relative to the window.
            Rect r = rect(win);
            r.x = r.y = 0;
            if (Fl::event_inside(r.x, r.y, r.w, r.h)) {
                m.view = dynamic_cast<BlockViewWindow *>(win);
                break;
            }
        }
        if (!m.view)
            return;
    }

    TrackView *t = 0;
    if (track_drag) {
        m.has_tracknum = true;
        int xpos = 0;
        m.tracknum = 0;
        for (int i = 0; i < m.view->block.tracks(); i++) {
            t = m.view->block.track_at(i);
            if (t->x() <= Fl::event_x() && Fl::event_x() > xpos) {
                m.tracknum = i;
                xpos = t->x();
            }
        }
    } else {
        for (int i = 0; i < m.view->block.tracks(); i++) {
            t = m.view->block.track_at(i);
            if (Fl::event_inside(t) || Fl::event_inside(&t->title_widget())) {
                m.has_tracknum = true;
                m.tracknum = i;
                break;
            }
        }
        if (!m.has_tracknum)
            return;
    }

    if (t && (track_drag || Fl::event_inside(t))) {
        int y = Fl::event_y() - t->y();
        m.has_pos = true;
        const ZoomInfo &zoom = m.view->block.get_zoom();
        m.pos = zoom.to_trackpos(y) + zoom.offset;
    }
}


static void
set_msg_from_event(UiMsg &m, int evt)
{
    m.event = evt;
    m.button = Fl::event_button();
    m.clicks = Fl::event_clicks();
    m.is_click = Fl::event_is_click();
    m.x = Fl::event_x();
    m.y = Fl::event_y();
    // Modifiers go over separately, so I shouldn't send capital letters.
    // Don't bother with non-alphabetic characters.  The effect of shift on
    // those is dependent on keymap anyway.
    m.key = tolower(Fl::event_text()[0]);
    if (!isprint(m.key)) // shift or backspace or some such
        m.key = Fl::event_key();
}


static void
set_update_args(UiMsg &m, BlockView *view, UiMsg::MsgType type)
{
    switch (type) {
    case UiMsg::msg_input:
        {
            const char *s;
            if (m.has_tracknum)
                s = view->track_at(m.tracknum)->get_title();
            else
                s = view->get_title();
            if (s) {
                char *text = new char[strlen(s) + 1];
                strcpy(text, s);
                m.update_text = text;
            }
        }
        break;
    case UiMsg::msg_track_scroll:
        m.width_scroll_visible_track = view->get_track_scroll();
        break;
    case UiMsg::msg_zoom:
        m.update_zoom = new ZoomInfo(view->get_zoom());
        break;
    case UiMsg::msg_view_resize:
        m.update_rect = new Rect(rect(view->window()));
        {
            Point track_size = view->get_track_size();
            m.width_scroll_visible_track = track_size.x;
            m.visible_time = track_size.y;
        }
        break;
    case UiMsg::msg_track_width:
        ASSERT(m.has_tracknum);
        m.width_scroll_visible_track = view->get_track_width(m.tracknum);
        break;
    case UiMsg::msg_close:
        ASSERT(m.view); // should have been set by caller
        break;
    case UiMsg::msg_event:
        ASSERT(false); // it's not an update so this shouldn't have been called
        break;
    }
}


// MsgCollector //////////////

void
MsgCollector::event(int evt, BlockViewWindow *view, bool track_drag)
{
    UiMsg m;
    m.type = UiMsg::msg_event;
    set_msg_from_event(m, evt);
    set_msg_context(view, track_drag, m);
    this->push(m);
}


void
MsgCollector::block_update(Fl_Widget *w, UiMsg::MsgType type)
{
    BlockViewWindow *win = static_cast<BlockViewWindow *>(w->window());
    this->window_update(win, type);
}


void
MsgCollector::block_update(Fl_Widget *w, UiMsg::MsgType type, int tracknum)
{
    BlockViewWindow *win = static_cast<BlockViewWindow *>(w->window());
    this->window_update(win, type, tracknum);
}


void
MsgCollector::window_update(BlockViewWindow *view, UiMsg::MsgType type)
{
    UiMsg m;
    m.type = type;
    m.view = view;
    set_update_args(m, &view->block, type);
    this->push(m);
}


void
MsgCollector::window_update(BlockViewWindow *view, UiMsg::MsgType type,
        int tracknum)
{
    UiMsg m;
    m.type = type;
    m.view = view;
    m.has_tracknum = true;
    m.tracknum = tracknum;
    set_update_args(m, &view->block, type);
    this->push(m);
}

MsgCollector *
global_msg_collector()
{
    static MsgCollector m;
    return &m;
}

void
MsgCollector::clear()
{
    for (size_t i = 0; i < this->msgs.size(); i++)
        msgs[i].free(); // yay pointers
    msgs.clear();
}

void
MsgCollector::push(const UiMsg &m)
{
    if (m.type == UiMsg::msg_event) {
        // Supppress keyups that have no keydown.  This can happen when focus
        // switches: the focused widget will eat the keydown to switch focus,
        // and whoever gets the focus (Block) will get a lone keyup.
        if (m.event == FL_KEYDOWN)
            this->keys_down.insert(m.key);
        else if (m.event == FL_KEYUP) {
            if (this->keys_down.find(m.key) == this->keys_down.end())
                return;
            this->keys_down.erase(m.key);
        }
    }
    this->msgs.push_back(m);
    if (this->log_collected)
        DEBUG("collected " << m);
}
