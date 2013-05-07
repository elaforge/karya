#include <FL/Fl.H>
#include <FL/Fl_Window.H>

#include "f_util.h"
#include "config.h"

#include "MsgCollector.h"
#include "SeqInput.h"


SeqInput::SeqInput(int X, int Y, int W, int H, bool do_expansion) :
    Fl_Input(X, Y, W, H), focus_color(255, 240, 220),
    proper_size(W, H), expanded(false), do_expansion(do_expansion)
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


int
SeqInput::handle(int evt)
{
    int key = Fl::event_key();
    if (evt == FL_KEYUP) {
        // If this is an edit input created in response to a keystroke, it gets
        // focus immediately and the keyup will wind up here.  So I have to
        // make sure the MsgCollector gets it.
        MsgCollector::get()->key_up(Fl::event_key());
    }
    if (evt == FL_KEYDOWN || evt == FL_KEYUP) {
        switch (key) {
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

    // Call Fl_Input::handle before expand(), so it has the updated value().
    int val = Fl_Input::handle(evt);
    switch (evt) {
    case FL_FOCUS: case FL_KEYDOWN:
        this->expand();
        break;
    case FL_UNFOCUS:
        this->contract();
        break;
    }

    switch (evt) {
    case FL_KEYDOWN:
        switch (Fl::event_key()) {
        case FL_Tab: case FL_Enter: case FL_Escape:
            Fl::focus(this->window());
            return 1;
        case FL_Up:
            this->position(0);
            return 1;
        case FL_Down:
            this->position(this->size());
            return 1;
        }
        break;
    case FL_KEYUP:
        // Eat keyups if I have focus.
        return 1;
    case FL_FOCUS:
        this->color(color_to_fl(this->focus_color));
        this->redraw();
        break;
    case FL_UNFOCUS:
        this->color(FL_WHITE);
        // So inputs consistently display the same part of text.
        // this->position(9999);
        this->redraw();
        // edit_input needs to emit text even when the text hasn't changed.
        this->do_callback();
        break;
    }
    // Returning 0 is how Fl_Input lets the parent group do keynav so its
    // arrow key refocus thing works.  Returing 1 disables that.
    // I still get 14 keyups on arrow keys.  FLTK I LOVE YOU TOO!
    if (evt == FL_KEYDOWN)
        val = 1;

    return val;
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
        // Bypass this->size, which sets proper_size.
        Fl_Input::resize(x(), y(), size.x, size.y);
        if (contraction)
            this->redraw_neighbors();
        this->redraw();
    }
}


void
SeqInput::contract()
{
    if (!this->do_expansion)
        return;
    this->expanded = false;
    if (this->w() != proper_size.x || this->h() != proper_size.y) {
        Fl_Input::resize(x(), y(), proper_size.x, proper_size.y);
        // Since I might have sized over my neighbors to the right, I'll go
        // redraw them.
        this->redraw_neighbors();
    }
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
