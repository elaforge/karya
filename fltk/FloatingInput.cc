// Copyright 2016 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <FL/Fl.H>
#include <FL/Fl_Widget.H>
#include <FL/Fl_Double_Window.H>

#include "WrappedInput.h"
#include "f_util.h"

#include "FloatingInput.h"


FloatingInput::FloatingInput(int x, int y, int w, int h,
        Fl_Window *owner, const char *text, bool strip, int max_width)
    : Fl_Double_Window(x, y, w, h),
        input(0, 0, w, h, strip, max_width), owner_(owner), ready(false)
{
    end();
    resizable(this);
    border(false);
    input.callback(wrapped_input_cb_dispatch, static_cast<void *>(this));
    when(0); // Only do the callback when I explicitly want it.
    if (text && *text)
        input.set_text(text);
    size(input.suggested_width(), input.text_height());
    show();
    // For some reason, the 'input' gets an extra focus / unfocus sequence
    // before the show().  Since I use unfocus to detect when editing is
    // complete and I can delete the FloatingInput, I need to explicitly
    // ignore the extra ones.
    // TODO figure this out
    this->ready = true;
}


void
FloatingInput::wrapped_input_cb_dispatch(Fl_Widget *_w, void *arg)
{
    FloatingInput *self = static_cast<FloatingInput *>(arg);
    self->wrapped_input_cb();
}


void
FloatingInput::wrapped_input_cb()
{
    int width = input.suggested_width();
    int height = input.text_height();
    if (width != w() || height != h()) {
        size(width, height);
    }
    if (ready && &input != Fl::focus()) {
        do_callback();
    }
}
