#include <string.h>
#include <ctype.h>

#include "util.h"

#include "MsgCollector.h"

// UiMsg /////////////////////

void
UiMsg::free()
{
    if (msg_input == type && input.text)
        ::free(input.text);
    else if (msg_zoom == type && zoom.zoom)
        delete zoom.zoom;
    else if (msg_resize == type && resize.rect)
        delete resize.rect;
    else if (msg_screen_size == type && screen.rect)
        delete screen.rect;
}


std::ostream &
operator<<(std::ostream &os, const UiMsg &m)
{
    os << '<' << UiMsg::msg_type_names()[m.type] << ' ' << m.context << ' ';
    switch (m.type) {
    case UiMsg::msg_event:
        os << m.event;
        break;
    case UiMsg::msg_input:
        os << "text=\"" << m.input.text << '"';
        break;
    case UiMsg::msg_track_scroll:
        os << m.track_scroll.scroll;
        break;
    case UiMsg::msg_zoom:
        os << *m.zoom.zoom;
        break;
    case UiMsg::msg_resize:
        os << *m.resize.rect << " track=(" << m.resize.visible_track
            << ", " << m.resize.visible_time << ")";
        break;
    case UiMsg::msg_track_width:
        os << m.track_width.width;
        break;
    case UiMsg::msg_close:
        break;
    case UiMsg::msg_screen_size:
        os << *m.screen.rect << " "
            << m.screen.screen << "/" << m.screen.screens;
        break;
    }
    return os << '>';
}


std::ostream &
operator<<(std::ostream &os, const UiMsg::Context &c)
{
    os << '{';
    if (c.focus)
        os << "f='" << c.focus->block.get_title() << "' ";
    if (c.view)
        os << "v='" << c.view->block.get_title() << "' ";
    if (c.has_tracknum)
        os << "t=" << c.tracknum << ' ';
    if (c.has_pos)
        os << "p=" << c.pos << ' ';
    return os << '}';
}


std::ostream &
operator<<(std::ostream &os, const UiMsg::Event &m)
{
    return os << show_event(m.event)
        << " key=" << show_key(m.key)
        << (m.is_repeat ? "[r]" : "")
        << " mods=" << show_event_state(m.modifier_state)
        << " button=" << m.button << " clicks=" << m.clicks
        << " is_click=" << m.is_click
        << " xy=(" << m.x << ", " << m.y << ")";
}


static void
set_context(UiMsg::Context &m, BlockViewWindow *view)
{
    m.view = view;
    Fl_Widget *focus = Fl::focus();
    if (focus) {
        while (focus && focus->window())
            focus = focus->window();
        m.focus = dynamic_cast<BlockViewWindow *>(focus);
        ASSERT(m.focus); // all windows should be BlockViewWindows
    }
}


static void
set_context(UiMsg::Context &m, BlockViewWindow *view, bool track_drag)
{
    set_context(m, view);
    if (!m.focus)
        return;
    TrackView *t = 0;
    if (track_drag) {
        m.has_tracknum = true;
        int xpos = 0;
        m.tracknum = 0;
        for (int i = 0; i < m.focus->block.tracks(); i++) {
            t = m.focus->block.track_at(i);
            if (t->x() <= Fl::event_x() && Fl::event_x() > xpos) {
                m.tracknum = i;
                xpos = t->x();
            }
        }
    } else {
        for (int i = 0; i < m.focus->block.tracks(); i++) {
            t = m.focus->block.track_at(i);
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
        const ZoomInfo &zoom = m.focus->block.get_zoom();
        m.pos = zoom.to_time(y) + zoom.offset;
    }
}


static void
set_event(UiMsg::Event &m, int evt)
{
    m.event = evt;
    m.button = Fl::event_button();
    m.clicks = Fl::event_clicks();
    m.is_click = Fl::event_is_click();
    m.x = Fl::event_x();
    m.y = Fl::event_y();
    // I originally called tolower() since haskell already knows shift is down,
    // but it's less error-prone to send characters in their correct form.
    m.key = Fl::event_text()[0];
    if (!isprint(m.key)) {
        // shift or backspace or some such
        m.key = Fl::event_key();
    }
    m.modifier_state = Fl::event_state();
    m.is_repeat = false; // this may be set to true by push()
}


static void
set_update(UiMsg &m, UiMsg::MsgType type)
{
    BlockView *block = &m.context.focus->block;
    switch (type) {
    case UiMsg::msg_input:
        {
            const char *s;
            if (m.context.has_tracknum)
                s = block->track_at(m.context.tracknum)->get_title();
            else
                s = block->get_title();
            if (s)
                m.input.text = strdup(s);
        }
        break;
    case UiMsg::msg_track_scroll:
        m.track_scroll.scroll = block->get_track_scroll();
        break;
    case UiMsg::msg_zoom:
        m.zoom.zoom = new ZoomInfo(block->get_zoom());
        break;
    case UiMsg::msg_resize:
        {
            m.resize.rect = new IRect(rect(block->window()));
            IPoint track_size = block->get_track_size();
            m.resize.visible_track = track_size.x;
            m.resize.visible_time = track_size.y;
        }
        break;
    case UiMsg::msg_track_width:
        ASSERT(m.context.has_tracknum);
        m.track_width.width = block->get_track_width(m.context.tracknum);
        break;
    case UiMsg::msg_close:
        ASSERT(m.context.view); // should have been set by caller
        break;
    case UiMsg::msg_event:
        ASSERT(false); // it's not an update so this shouldn't have been called
        break;
    case UiMsg::msg_screen_size:
        ASSERT(false); // you should have called screen_update()
        break;
    }
}


// MsgCollector //////////////

void
MsgCollector::event(int evt, BlockViewWindow *view, bool track_drag)
{
    UiMsg m;
    m.type = UiMsg::msg_event;
    set_context(m.context, view, track_drag);
    // Special hack for FOCUS: in this case the passed view is the focus, not
    // an associated view.  In fact, FL_FOCUS is the only thing that should
    // call event() with a non-NULL view.
    if (FL_FOCUS == evt) {
        m.context.focus = view;
        m.context.view = NULL;
    }
    set_event(m.event, evt);
    this->push(m);
}


void
MsgCollector::update(UiMsg::MsgType type)
{
    this->window_update(NULL, type);
}

void
MsgCollector::update(UiMsg::MsgType type, int tracknum)
{
    UiMsg m;
    m.type = type;
    set_context(m.context, NULL);
    m.context.has_tracknum = true;
    m.context.tracknum = tracknum;
    set_update(m, type);
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
    UiMsg m;
    m.type = type;
    set_context(m.context, NULL);
    m.context.view = win;
    m.context.has_tracknum = true;
    m.context.tracknum = tracknum;
    set_update(m, type);
    this->push(m);
}


void
MsgCollector::window_update(BlockViewWindow *view, UiMsg::MsgType type)
{
    UiMsg m;
    m.type = type;
    set_context(m.context, view);
    set_update(m, type);
    this->push(m);
}



void
MsgCollector::screen_update()
{
    int screens = Fl::screen_count();
    for (int screen = 0; screen < screens; screen++) {
        int x, y, w, h;
        UiMsg m;
        Fl::screen_xywh(x, y, w, h, screen);
        m.type = UiMsg::msg_screen_size;
        m.screen.screen = screen;
        m.screen.screens = screens;
        m.screen.rect = new IRect(x, y, w, h);
        this->push(m);
    }
}


MsgCollector *
MsgCollector::get()
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
MsgCollector::push(UiMsg &m)
{
    if (m.type == UiMsg::msg_event) {
        // Supppress keyups that have no keydown.  This can happen when focus
        // switches: the focused widget will eat the keydown to switch focus,
        // and whoever gets the focus (Block) will get a lone keyup.
        if (m.event.event == FL_KEYDOWN) {
            if (this->keys_down.find(m.event.key) != keys_down.end())
                m.event.is_repeat = true;
            else
                this->keys_down.insert(m.event.key);
        } else if (m.event.event == FL_KEYUP) {
            if (this->keys_down.find(m.event.key) == this->keys_down.end())
                return;
            this->keys_down.erase(m.event.key);
        }
    }
    this->msgs.push_back(m);
    if (this->log_collected)
        DEBUG("collected " << m);
}
