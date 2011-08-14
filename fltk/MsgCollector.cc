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
operator<<(std::ostream &os, const UiMsg::Event &e)
{
    return os << show_event(e.event)
        << " key=" << show_key(e.key)
        << (e.is_repeat ? "[r]" : "")
        << " mods=" << show_event_state(e.modifier_state)
        << " button=" << e.button << " clicks=" << e.clicks
        << " is_click=" << e.is_click
        << " xy=(" << e.x << ", " << e.y << ")";
}

// Context ///////////////////

static void
set_context(UiMsg::Context &c, BlockViewWindow *view)
{
    c.view = view;
    Fl_Widget *focus = Fl::focus();
    if (focus) {
        while (focus && focus->window())
            focus = focus->window();
        c.focus = dynamic_cast<BlockViewWindow *>(focus);
        ASSERT(c.focus); // all windows should be BlockViewWindows
    }
}


// track_drag means this is part of a drag that started on a track, so it
// should continue to provide tracknum context no matter where the cursor is.
static void
set_event_context(UiMsg::Context &c, BlockViewWindow *view, bool track_drag)
{
    set_context(c, view);
    if (!c.focus)
        return;
    TrackView *t = 0;
    int tracks = c.focus->block.tracks();
    if (track_drag) {
        c.has_tracknum = true;
        // If it's not to the left of any track it must be the rightmost one.
        c.tracknum = tracks - 1;
        for (int i = 0; i < tracks; i++) {
            t = c.focus->block.track_at(i);
            if (Fl::event_x() <= t->x() + t->w()) {
                c.tracknum = i;
                break;
            }
        }
    } else {
        for (int i = 0; i < tracks; i++) {
            t = c.focus->block.track_at(i);
            // Detect if an event is within a track, where a track extends up
            // to the block title, including the skeleton display.  This means
            // clicks on the track_box are considered on track 0, but this
            // seems potentially useful.
            if (c.focus->block.title_bottom() <= Fl::event_y()
                    && Fl::event_y() < c.focus->block.status_top()
                    && Fl::event_x() <= t->x() + t->w()) {
                c.has_tracknum = true;
                c.tracknum = i;
                break;
            }
        }
        if (!c.has_tracknum)
            return;
    }

    if (t && (track_drag || Fl::event_inside(t))) {
        int y = Fl::event_y() - t->y();
        c.has_pos = true;
        const ZoomInfo &zoom = c.focus->block.get_zoom();
        c.pos = zoom.to_time(y) + zoom.offset;
    }
}


// Context constructors.  These are not real constructors because I don't
// want Context() to do complicated stuff like figure out focus.

static UiMsg::Context
context(BlockViewWindow *view)
{
    UiMsg::Context c;
    set_context(c, view);
    return c;
}


static UiMsg::Context
context(BlockViewWindow *view, int tracknum)
{
    UiMsg::Context c;
    set_context(c, view);
    c.has_tracknum = true;
    c.tracknum = tracknum;
    return c;
}

// Other update data /////////

static void
set_event(UiMsg::Event &e, int evt)
{
    e.event = evt;
    e.button = Fl::event_button();
    e.clicks = Fl::event_clicks();
    e.is_click = Fl::event_is_click();
    e.x = Fl::event_x();
    e.y = Fl::event_y();
    // I originally called tolower() since haskell already knows shift is down,
    // but it's less error-prone to send characters in their correct form.
    e.key = Fl::event_text()[0];
    if (!isprint(e.key)) {
        // shift or backspace or some such
        e.key = Fl::event_key();
    }
    e.modifier_state = Fl::event_state();
    e.is_repeat = false; // this may be set to true by push()
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
MsgCollector::event(int evt, bool track_drag)
{
    UiMsg m;
    m.type = UiMsg::msg_event;
    set_event_context(m.context, NULL, track_drag);
    set_event(m.event, evt);
    this->push(m);
}

void
MsgCollector::focus(BlockViewWindow *focus)
{
    UiMsg m;
    m.type = UiMsg::msg_event;
    m.context.focus = focus;
    set_event(m.event, FL_FOCUS);
    this->push(m);
}

void
MsgCollector::update(UiMsg::MsgType type)
{
    push_update(type, context(NULL));
}


static BlockViewWindow *
window(Fl_Widget *w)
{
    if (w)
        return static_cast<BlockViewWindow *>(w->window());
    else
        return NULL;
}


void
MsgCollector::block(UiMsg::MsgType type, Fl_Widget *w)
{
    push_update(type, context(window(w)));
}


void
MsgCollector::track(UiMsg::MsgType type, Fl_Widget *w, int tracknum)
{
    push_update(type, context(window(w), tracknum));
}


void
MsgCollector::view(UiMsg::MsgType type, BlockViewWindow *view)
{
    push_update(type, context(view));
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


void
MsgCollector::push_update(UiMsg::MsgType type, const UiMsg::Context &c)
{
    UiMsg m;
    m.type = type;
    m.context = c;
    set_update(m, type);
    this->push(m);
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
