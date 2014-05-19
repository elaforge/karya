// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <FL/Fl.H>

#include "f_util.h"
#include "config.h"
#include "input_util.h"

#include "MsgCollector.h"
#include "ExpandInput.h"


ExpandInput::ExpandInput(int X, int Y, int W, int H, bool do_expansion,
        bool strip_text) :
    Fl_Input(X, Y, W, H),
    proper_size(W, H), expanded(false), do_expansion(do_expansion),
    strip_text(strip_text), callback2(NULL), callback2_arg(NULL)
{
    this->color(FL_WHITE);
    this->textsize(Config::font_size::input);
    this->box(FL_THIN_DOWN_BOX);
    this->callback(ExpandInput::changed_cb, static_cast<void *>(this));
    // FL_WHEN_RELEASE is documented as firing whenever focus leaves the input.
    // But that's not true, it doesn't fire if the text hasn't changed.
    // So I have to call 'do_callback' on FL_UNFOCUS myself.
    this->when(0);
}


void
ExpandInput::resize(int x, int y, int w, int h)
{
    if (!this->expanded)
        this->proper_size = IPoint(w, h);
    Fl_Input::resize(x, y, w, h);
}


void
ExpandInput::set_text(const char *text)
{
    this->value(text);
    // So inputs consistently display the same part of text.
    // this->position(9999);
}


void
ExpandInput::insert_text(const char *text)
{
    this->insert(text, 0);
    this->expand();
}


int
ExpandInput::handle(int evt)
{
    // This is a crazy delicate mess because I have to apply my own key
    // bindings but fall back on the Fl_Input ones otherwise.
    if (evt == FL_KEYUP) {
        // If this is an edit input created in response to a keystroke, it gets
        // focus immediately and the keyup will wind up here.  So I have to
        // make sure the MsgCollector gets it.
        MsgCollector::get()->key_up(Fl::event_key());
    }
    if (input_util::should_ignore(evt))
        return 0;
    bool handled = input_util::handle(this, evt, false);

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
ExpandInput::expand()
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
    // ExpandInput doesn't wrap so I don't use 'y', but who knows, maybe it will
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
ExpandInput::contract()
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
ExpandInput::redraw_neighbors()
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


void
ExpandInput::changed_cb(Fl_Widget *w, void *vp)
{
    ExpandInput *self = static_cast<ExpandInput *>(vp);
    if (self->strip_text)
        input_util::strip_value(self);

    // I only put ExpandInputs in BlockViewWindows, so this should be safe.
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
