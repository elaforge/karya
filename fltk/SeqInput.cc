// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <FL/Fl.H>
#include <FL/Fl_Window.H>

#include "f_util.h"
#include "config.h"

#include "MsgCollector.h"
#include "SeqInput.h"


SeqInput::SeqInput(int X, int Y, int W, int H, bool do_expansion,
        bool strip_text) :
    Fl_Input(X, Y, W, H), focus_color(255, 240, 220),
    proper_size(W, H), expanded(false), do_expansion(do_expansion),
    strip_text(strip_text), callback2(NULL), callback2_arg(NULL)
{
    this->color(FL_WHITE);
    this->textsize(Config::font_size::input);
    this->box(FL_THIN_DOWN_BOX);
    this->callback(SeqInput::changed_cb, static_cast<void *>(this));
    // FL_WHEN_RELEASE is documented as firing whenever focus leaves the input.
    // But that's not true, it doesn't fire if the text hasn't changed.
    // So I have to call 'do_callback' on FL_UNFOCUS myself.
    this->when(0);
}


void
SeqInput::resize(int x, int y, int w, int h)
{
    if (!this->expanded)
        this->proper_size = IPoint(w, h);
    Fl_Input::resize(x, y, w, h);
}


void
SeqInput::set_text(const char *text)
{
    this->value(text);
    // So inputs consistently display the same part of text.
    // this->position(9999);
}


void
SeqInput::insert_text(const char *text)
{
    this->insert(text, 0);
    this->expand();
}


// First skip trailing spaces, then skip a token.  A token is a parenthesized
// expression, from a ` to the next `, or until a space.
static const char *
backward_token(const char *start, const char *pos)
{
    const char *p = utf8::backward(pos, start);
    while (p > start && *p == ' ')
        p = utf8::backward(p, start);
    if (*p == ')') {
        int parens = 1;
        do {
            p = utf8::backward(p, start);
            if (*p == '(')
                parens--;
            else if (*p == ')')
                parens++;
            if (parens == 0)
                break;
        } while (p > start);
        // Unbalanced parens, so just go back one character.
        if (parens)
            p = utf8::backward(pos, start);
    } else if (*p == '`') {
        do {
            p = utf8::backward(p, start);
            if (*p == '`')
                break;
        } while (p > start);
        // Unbalanced backticks, so just go back one character.
        if (*p != '`')
            p = utf8::backward(pos, start);
    } else {
        while (p > start && *p != ' ')
            p = utf8::backward(p, start);
        if (*p == ' ')
            p = utf8::forward(p, pos);
    }
    return p;
}

// Skip a token, and then any trailing spaces.  A token has the same definition
// as in 'backward_token'.
static const char *
forward_token(const char *end, const char *pos)
{
    const char *p = pos;
    if (p == end)
        return p;
    if (*p == '(') {
        int parens = 1;
        do {
            p = utf8::forward(p, end);
            if (*p == '(')
                parens++;
            else if (*p == ')')
                parens--;
            if (parens == 0)
                break;
        } while (p < end);
        // Unbalanced parens, so just go forward one character.
        if (parens)
            p = utf8::forward(pos, end);
        else
            p = utf8::forward(p, end);
    } else if (*p == '`') {
        do {
            p = utf8::forward(p, end);
            if (*p == '`')
                break;
        } while (p < end);
        // Unbalanced backticks, so just go forward one character.
        if (*p != '`')
            p = utf8::forward(pos, end);
        else
            p = utf8::forward(p, end);
    } else {
        while (p < end && *p != ' ')
            p = utf8::forward(p, end);
    }
    while (p < end && *p == ' ')
        p = utf8::forward(p, end);
    return p;
}

static void
move_backward(SeqInput *w, bool shift)
{
    const char *text = w->value();
    const char *p = backward_token(text, text + w->position());
    if (shift)
        w->position(p - text, w->mark());
    else
        w->position(p - text);
}


static void
move_forward(SeqInput *w, bool shift)
{
    const char *text = w->value();
    const char *p = forward_token(text + w->size(), text + w->position());
    if (shift)
        w->position(p - text, w->mark());
    else
        w->position(p - text);
}


static void
backspace_token(SeqInput *w)
{
    const char *text = w->value();
    const char *p = backward_token(text, text + w->position());
    w->replace(p - text, w->position(), NULL);
}


int
SeqInput::handle(int evt)
{
    // This is a crazy delicate mess because I have to apply my own key
    // bindings but fall back on the Fl_Input ones otherwise.
    if (evt == FL_KEYUP) {
        // If this is an edit input created in response to a keystroke, it gets
        // focus immediately and the keyup will wind up here.  So I have to
        // make sure the MsgCollector gets it.
        MsgCollector::get()->key_up(Fl::event_key());
    }
    if (evt == FL_KEYDOWN || evt == FL_KEYUP) {
        switch (Fl::event_key()) {
        case FL_Shift_L: case FL_Shift_R: case FL_Enter: case FL_Escape:
        case FL_Right: case FL_Left: case FL_Up: case FL_Down:
        case FL_BackSpace: case FL_Tab:
            break; // some non-prinables are handled here
        default:
            // but control chars and the like should be passed out
            if (!isprint(Fl::event_key()))
                return 0;
        }
    }

    int state = Fl::event_state();
    bool handled = false;
    switch (evt) {
    case FL_KEYDOWN:
        switch (Fl::event_key()) {
        case FL_Tab: case FL_Enter: case FL_Escape:
            Fl::focus(this->window());
            handled = true;
            break;
        case FL_Up:
            this->position(0);
            handled = true;
            break;
        case FL_Down:
            this->position(this->size());
            handled = true;
            break;
        case 'h':
            if (state & (FL_SHIFT | FL_META | FL_CTRL)) {
                move_backward(this, state & FL_SHIFT);
                handled = true;
            }
            break;
        case 'l':
            if (state & (FL_SHIFT | FL_META | FL_CTRL)) {
                move_forward(this, state & FL_SHIFT);
                handled = true;
            }
            break;
        case FL_BackSpace:
            if (state & (FL_SHIFT | FL_META | FL_CTRL)) {
                backspace_token(this);
                handled = true;
            }
            break;
        }
        break;
    case FL_KEYUP:
        // Eat keyups if I have focus.
        handled = true;
        break;
    case FL_FOCUS:
        this->color(color_to_fl(this->focus_color));
        this->redraw();
        // Don't set handled, because Fl_Input still needs to get this.
        break;
    case FL_UNFOCUS:
        this->color(FL_WHITE);
        // So inputs consistently display the same part of text.
        // this->position(9999);
        this->redraw();
        break;
    }

    // Call Fl_Input::handle before expand(), so it has the updated value().
    // If I didn't handle the event above, hand it to Fl_Input so it can do its
    // keybindings.
    if (!handled) {
        handled = Fl_Input::handle(evt);
        // Only call expand if Fl_Input handled the key, which means it may
        // have added or removed a character.
        if (handled && evt == FL_KEYDOWN)
            this->expand();
    }
    switch (evt) {
    case FL_FOCUS:
        this->expand();
        break;
    case FL_UNFOCUS:
        this->contract();
        // edit_input needs to emit text even when the text hasn't changed.
        this->do_callback();
        break;
    }

    // Returning false is how Fl_Input lets the parent group do keynav so its
    // arrow key refocus thing works.  Returing 1 disables that.
    // I still get 14 keyups on arrow keys.  FLTK I LOVE YOU TOO!
    if (evt == FL_KEYDOWN)
        handled = true;

    return handled;
}


// This doesn't necessarily make the input larger, it could make it smaller
// if a character was deleted.
void
SeqInput::expand()
{
    if (!this->do_expansion)
        return;
    IPoint size(0, 0);
    fl_font(Config::font, Config::font_size::input);
    fl_measure(this->value(), size.x, size.y, false);

    // The text input needs a bit more space than just the text within, since
    // it also has an insert cursor.
    size.x += 7;
    // I intentionally allow it to grow off the edge of the parent window.
    // If this entry is scrolled off the edge then it would be clipped down to
    // nothing, but have no way of knowing when it is later scrolled into the
    // window.  The edge of the window will clip me anyway.
    size.x = std::max(size.x, this->proper_size.x);
    // SeqInput doesn't wrap so I don't use 'y', but who knows, maybe it will
    // someday.
    size.y = this->proper_size.y;
    this->expanded = true;

    if (size.x != this->w()) {
        bool contraction = size.x < this->w();
        this->resize(x(), y(), size.x, size.y);
        if (contraction)
            this->redraw_neighbors();
        this->redraw();
    }
}


void
SeqInput::contract()
{
    if (!do_expansion || !expanded)
        return;
    if (this->w() != proper_size.x || this->h() != proper_size.y) {
        this->resize(x(), y(), proper_size.x, proper_size.y);
        // Since I might have sized over my neighbors to the right, I'll go
        // redraw them.
        this->redraw_neighbors();
    }
    this->expanded = false;
}


void
SeqInput::redraw_neighbors()
{
    // Since expand() can inconsiderately walk all over its neighbors, I have
    // to redraw them when I contract() again.
    Fl_Group *parent = this->parent();
    ASSERT(parent);

    IRect me(rect(this));
    for (int i = 0; i < parent->children(); i++) {
        Fl_Widget &sibling = *parent->child(i);
        if (sibling.x() > x()
            && !(sibling.y() > me.b() || sibling.y() + sibling.h() < me.y))
        {
            sibling.redraw();
        }
    }
}


static void
strip_value(Fl_Input *w)
{
    // Fl_Input manages the storage.
    const char *s = w->value();

    // Why am I still writing functions like this?
    int start = 0, end = strlen(s);
    if (!(end > 0 && isspace(s[0]) || isspace(s[end-1]))) {
        return;
    }
    while (start < end && isspace(s[start]))
        start++;
    while (end > start && isspace(s[end-1]))
        end--;
    if (end - start <= 0) {
        w->value(NULL);
    } else {
        w->value(s + start, end - start);
    }
}


void
SeqInput::changed_cb(Fl_Widget *w, void *vp)
{
    SeqInput *self = static_cast<SeqInput *>(vp);
    if (self->strip_text)
        strip_value(self);

    // I only put SeqInputs in BlockViewWindows, so this should be safe.
    BlockViewWindow *view = dynamic_cast<BlockViewWindow *>(self->window());
    for (int i = 0; i < view->block.tracks(); i++) {
        if (&view->block.track_at(i)->title_widget() == self) {
            MsgCollector::get()->track(UiMsg::msg_input, self, i);
            break;
        }
    }
    if (self->callback2)
        self->callback2(w, self->callback2_arg);
}
