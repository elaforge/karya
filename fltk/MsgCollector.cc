#include "util.h"

#include "MsgCollector.h"

// UiMsg /////////////////////

inline std::ostream &
operator<<(std::ostream &os, const UiMsg &m)
{
    // Keep this up to date with UiMsg::MsgType
    static const char *msg_type_names[] = { "event", "input",
        "track_scroll", "zoom", "view_resize",
        "track_width", "close" };

    char keybuf[12];
    os << '<' << msg_type_names[m.type];
    switch (m.type) {
    case UiMsg::msg_event:
        if (isprint(m.key))
            sprintf(keybuf, "%c", m.key);
        else
            sprintf(keybuf, "\\0x%x", m.key);
        os << "=" << show_event(m.event)
            << " button=" << m.button << " clicks=" << m.clicks
            << " is_click=" << m.is_click
            << " xy=(" << m.x << ", " << m.y
            << ") key='" << keybuf << "'";
        break;
    }
    if (m.view)
        os << " view=\"" << m.view->block.get_title() << '"';
    if (m.has_track)
        os << " track=" << m.track;
    if (m.has_pos)
        os << " pos=" << m.pos;
    os << '>';
}


static void
set_msg_context(UiMsg &m)
{
    for (Fl_Window *win = Fl::first_window(); win; win = Fl::next_window(win)) {
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

    TrackView *t = 0;
    for (int i = 0; i < m.view->block.tracks(); i++) {
        t = m.view->block.track_at(i);
        if (Fl::event_inside(t) || Fl::event_inside(&t->title_widget())) {
            m.has_track = true;
            m.track = i;
            break;
        }
    }
    if (!m.has_track)
        return;
    if (Fl::event_inside(t)) {
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
    m.key = Fl::event_key();
}


// MsgCollector //////////////

void
MsgCollector::event(int evt, BlockViewWindow *view)
{
    UiMsg m;
    m.type = UiMsg::msg_event;
    set_msg_from_event(m, evt);
    if (!view) {
        set_msg_context(m);
    } else {
        m.view = view;
    }
    this->push(m);
}


void
MsgCollector::block_changed(Fl_Widget *w, UiMsg::MsgType type, int track)
{
    BlockViewWindow *win = static_cast<BlockViewWindow *>(w->window());
    this->window_changed(win, type, track);
}


void
MsgCollector::window_changed(BlockViewWindow *view, UiMsg::MsgType type,
        int track)
{
    UiMsg m;
    m.type = type;
    m.view = view;
    if (track != -1) {
        m.has_track = true;
        m.track = track;
    }
    this->push(m);
}


MsgCollector *
global_msg_collector()
{
    static MsgCollector m;
    return &m;
}
