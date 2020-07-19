// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <string.h>
#include <ctype.h>

#include "FloatingInput.h"
#include "f_util.h"
#include "util.h"

#include "MsgCollector.h"


// UiMsg /////////////////////

void
UiMsg::free()
{
    switch (type) {
    case msg_input:
        if (input.text)
            ::free(input.text);
        break;
    case msg_resize:
        if (resize.rect)
            delete resize.rect;
        break;
    case msg_screen_size:
        if (screen.rect)
            delete screen.rect;
        break;
    default:
        break;
    }
}


std::ostream &
operator<<(std::ostream &os, const UiMsg &m)
{
    os << '<' << UiMsg::msg_type_name(m.type) << ' ' << m.context << ' ';
    switch (m.type) {
    case UiMsg::msg_event:
        os << m.event;
        break;
    case UiMsg::msg_input:
        if (m.input.text)
            os << "text=\"" << m.input.text << '"';
        else
            os << "text=null";
        break;
    case UiMsg::msg_track_scroll:
        os << m.track_scroll.scroll;
        break;
    case UiMsg::msg_time_scroll:
        os << m.time_scroll.scroll;
        break;
    case UiMsg::msg_resize:
        os << *m.resize.rect << " padding=" << m.resize.padding;
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
    if (c.track_type) {
        switch (c.track_type) {
        case UiMsg::track_normal:
            os << "normal tracknum=" << c.tracknum;
            break;
        case UiMsg::track_floating_input:
            os << "floating_input";
            break;
        case UiMsg::track_divider:
            os << "div tracknum=" << c.tracknum;
            break;
        }
        os << ' ';
    }
    if (c.has_pos)
        os << "p=" << c.pos << ' ';
    return os << '}';
}


std::ostream &
operator<<(std::ostream &os, const UiMsg::Event &e)
{
    os << f_util::show_event(e.event);
    if (FL_KEYDOWN == e.event || FL_KEYUP == e.event) {
        os << " key=" << f_util::show_key(e.key)
            << (e.is_repeat ? "[r]" : "");
        if (e.text) {
            os << " text=" << e.text;
        }
    }
    os << " mods=" << f_util::show_event_state(e.modifier_state);
    if (FL_PUSH == e.event || FL_DRAG == e.event || FL_RELEASE == e.event) {
        os << " button=" << e.button << " clicks=" << e.clicks
            << " is_click=" << (e.is_click ? "t" : "f")
            << " xy=(" << e.x << ", " << e.y << ")";
    }
    return os;
}

// Context ///////////////////

static void
set_context(UiMsg::Context &c, BlockWindow *view)
{
    c.view = view;
    Fl_Widget *focus = Fl::focus();
    if (focus) {
        while (focus && focus->window())
            focus = focus->window();
        // All windows should be either FloatingInput or BlockWindow.
        FloatingInput *input = dynamic_cast<FloatingInput *>(focus);
        if (input != nullptr) {
            c.focus = dynamic_cast<BlockWindow *>(input->owner());
        } else {
            c.focus = dynamic_cast<BlockWindow *>(focus);
        }
        // c.focus could be nullptr if it's the keymap window.
        // ASSERT(c.focus);
    }
}


// track_drag means this is part of a drag that started on a track, so it
// should continue to provide tracknum context no matter where the cursor is.
static void
set_event_context(UiMsg::Context &c, BlockWindow *view, bool track_drag)
{
    static const int divider_pad = 3;

    set_context(c, view);
    if (!c.focus)
        return;
    Track *t = 0;
    int tracks = c.focus->block.tracks();

    // This implementation means that dragging upward from the status bar
    // will start to select tracks, which ok I think.
    if (track_drag || Fl::event_y() < c.focus->block.status_top())
        c.track_type = UiMsg::track_normal;
    if (c.track_type) {
        int i = 0;
        for (; i < tracks; i++) {
            // This is a special hack that makes it easier to click dividers,
            // since typically they're narrow.  MoveTile will grab clicks
            // within a certain range to make them easier to drag, so do the
            // same thing for non-move clicks.
            if (i+1 < tracks) {
                t = c.focus->block.track_at(i+1);
                int x = Fl::event_x() + divider_pad;
                if (dynamic_cast<Divider *>(t)
                        && x >= t->x() && x <= t->x() + t->w())
                {
                    i++;
                    break;
                }
            }
            if (i > 0) {
                t = c.focus->block.track_at(i-1);
                int x = Fl::event_x() - divider_pad;
                if (dynamic_cast<Divider *>(t) && x <= t->x() + t->w()) {
                    i--;
                    break;
                }
            }
            t = c.focus->block.track_at(i);
            if (Fl::event_x() <= t->x() + t->w()) {
                break;
            }
        }
        // Count an event past the rightmost track as the rightmost track.
        c.tracknum = std::min(i, tracks-1);
        if (dynamic_cast<Divider *>(t))
            c.track_type = UiMsg::track_divider;
    }

    if (t && track_drag) {
        int y = Fl::event_y() - t->y();
        c.has_pos = true;
        const Zoom &zoom = c.focus->block.get_zoom();
        c.pos = zoom.to_time(y) + zoom.offset;
    }
}


// Context constructors.  These are not real constructors because I don't
// want Context() to do complicated stuff like figure out focus.

static UiMsg::Context
context(BlockWindow *view)
{
    UiMsg::Context c;
    set_context(c, view);
    return c;
}


static UiMsg::Context
context(BlockWindow *view, int tracknum)
{
    UiMsg::Context c;
    c.track_type = UiMsg::track_normal;
    c.tracknum = tracknum;
    set_context(c, view);
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
    // On OS X, shifted symbols emit the alternate symbol + shift, rather
    // than the unshifted symbol + shift, which is inconsistent with letters
    // and digits.  It's also inconsistent with X, which always emits the main
    // symbol.  I have to map the symbols back to their keycaps in Ui.UiMsg for
    // keyboard mapping to be consistent across platforms.
    //
    // Further notes in "Cmd.Keymap".
    e.key = Fl::event_key();
    if (evt == FL_KEYDOWN && isprint(Fl::event_text()[0])) {
        e.text = Fl::event_text()[0];
    } else {
        e.text = 0;
    }
    e.modifier_state = Fl::event_state();
    e.is_repeat = false; // this may be set to true by push()
}


static void
set_update(UiMsg &m, UiMsg::MsgType type, const char *text)
{
    ASSERT_MSG(m.context.view, "caller must explicitly set view for updates");
    Block *block = &m.context.view->block;
    switch (type) {
    case UiMsg::msg_input:
        {
            // If 'text' was given, it's either a track title, block title, or
            // floating input.  The floating input sets the text to null if
            // the input is closing with no change.
            const char *s;
            if (text)
                s = text;
            else if (m.context.track_type == UiMsg::track_floating_input)
                s = nullptr;
            else
                s = block->get_title();
            m.input.text = s ? strdup(s) : nullptr;
        }
        break;
    case UiMsg::msg_track_scroll:
        m.track_scroll.scroll = block->get_track_scroll();
        break;
    case UiMsg::msg_time_scroll:
        m.time_scroll.scroll = block->get_zoom().offset;
        break;
    case UiMsg::msg_resize:
        {
            m.resize.rect = new IRect(f_util::rect(block->window()));
            m.resize.padding = block->get_padding();
        }
        break;
    case UiMsg::msg_track_width:
        ASSERT(m.context.track_type);
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
    set_event_context(m.context, nullptr, track_drag);
    set_event(m.event, evt);
    this->push(m);
}

void
MsgCollector::focus(BlockWindow *focus)
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
    push_update(type, context(nullptr));
}


static BlockWindow *
window(Fl_Widget *w)
{
    if (w)
        return static_cast<BlockWindow *>(w->window());
    else
        return nullptr;
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
MsgCollector::track_title(Fl_Widget *w, int tracknum, const char *text)
{
    UiMsg::Context c(context(window(w), tracknum));
    c.track_type = UiMsg::track_normal;
    push_update(UiMsg::msg_input, c, text);
}


void
MsgCollector::floating_input(Fl_Widget *w, const char *floating_input)
{
    UiMsg::Context c(context(window(w)));
    c.track_type = UiMsg::track_floating_input;
    push_update(UiMsg::msg_input, c, floating_input);
}


void
MsgCollector::view(UiMsg::MsgType type, BlockWindow *view)
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
        Fl::screen_work_area(x, y, w, h, screen);
        m.type = UiMsg::msg_screen_size;
        m.screen.screen = screen;
        m.screen.screens = screens;
        m.screen.rect = new IRect(x, y, w, h);
        this->push(m);
    }
}


void
MsgCollector::push_update(UiMsg::MsgType type, const UiMsg::Context &c,
    const char *text)
{
    UiMsg m;
    m.type = type;
    m.context = c;
    set_update(m, type, text);
    this->push(m);
}


MsgCollector *
MsgCollector::get()
{
    static MsgCollector m;
    return &m;
}


int
MsgCollector::event_handler(int evt)
{
    switch (evt) {
    case FL_SCREEN_CONFIGURATION_CHANGED:
        get()->screen_update();
        return 1;
    case FL_NO_EVENT:
        // For some reason fltk sends these to the Fl::add_handler on linux.
        return 1;
    case FL_SHORTCUT:
        return 1;
    default:
        DEBUG("unknown event: " << f_util::show_event(evt));
    }
    return 0;
}


void
MsgCollector::all_keys_up()
{
    keys_down.clear();
}


void
MsgCollector::key_up(int key)
{
    keys_down.erase(key);
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
        int key = Fl::event_key();
        if (m.event.event == FL_KEYDOWN) {
            if (keys_down.find(key) != keys_down.end())
                m.event.is_repeat = true;
            else
                keys_down[key] = m.event.key;
        } else if (m.event.event == FL_KEYUP) {
            if (keys_down.find(key) == keys_down.end())
                return;
            else {
                // The contents of Fl::event_text() are unreliable for keyups.
                m.event.key = keys_down[key];
                keys_down.erase(key);
            }
        }
    }

    if (util::Timing::level >= 2) {
        static char name[64];
        name[0] = '\0';
        strcpy(name, "evt-");
        if (m.type == UiMsg::msg_event) {
            strcat(name, f_util::show_event(m.event.event));
        } else {
            strcat(name, UiMsg::msg_type_name(m.type));
        }
        util::timing(2, name);
    }

    this->msgs.push_back(m);
    if (this->log_collected)
        std::cout << m << '\n';
}
