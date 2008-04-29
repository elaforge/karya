#include <FL/Fl.H>
#include <FL/Fl_Window.H>

#include "f_util.h"
#include "config.h"

#include "MsgCollector.h"
#include "SeqInput.h"


SeqInput::SeqInput(int X, int Y, int W, int H) :
    Fl_Input(W, Y, W, H), focus_color(255, 200, 200)
{
    this->textsize(Config::font_size::input);
    this->box(FL_THIN_DOWN_BOX);
    this->callback(SeqInput::changed_cb, static_cast<void *>(this));
    this->when(FL_WHEN_RELEASE);
}


int
SeqInput::handle(int evt)
{
    switch (evt) {
    case FL_KEYBOARD:
        if (Fl::event_key() == FL_Tab || Fl::event_key() == FL_Enter) {
            Fl::focus(this->window());
            return 1;
        }
    case FL_FOCUS:
        this->color(color_to_fl(this->focus_color));
        this->redraw();
        break;
    case FL_UNFOCUS:
        this->color(FL_WHITE);
        this->redraw();
        break;
    }
    // TODO
    // change keymap:
    // ^A and cmd+a should select all
    // set callback to when(FL_WHEN_RELEASE)
    // or maybe some control keys unfocus but don't eat the event?
    return Fl_Input::handle(evt);
}


void
SeqInput::changed_cb(Fl_Widget *_w, void *vp)
{
    SeqInput *self = static_cast<SeqInput *>(vp);
    // I only put SeqInputs in BlockViewWindows, so this should be safe.
    BlockViewWindow *view = static_cast<BlockViewWindow *>(self->window());
    for (int i = 0; i < view->block.tracks(); i++) {
        if (&view->block.track_at(i)->title_widget() == self) {
            global_msg_collector()->window_update(view, UiMsg::msg_input, i);
            return;
        }
    }
    global_msg_collector()->window_update(view, UiMsg::msg_input);
}
