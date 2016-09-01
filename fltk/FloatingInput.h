// Copyright 2016 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#ifndef __FLOATING_INPUT_H
#define __FLOATING_INPUT_H

#include <FL/Fl_Double_Window.H>
#include "WrappedInput.h"

// A borderless window with just a WrappedInput in it.  It will expand on
// demand.  It calls its callback when it gets an enter or loses focus.
class FloatingInput : public Fl_Double_Window {
public:
    FloatingInput(int x, int y, int w, int h, Fl_Window *owner,
        const char *text);
    void cursor_position(int cursor, int mark) {
        input.position(cursor, mark);
    }
    Fl_Window *owner() const { return owner_; }

    void insert(const char *text) { input.insert(text); }
    const char *get_text() const { return input.get_text(); }
    int text_height() const { return input.text_height(); }
    bool text_changed() const { return input.text_changed(); }

    // void insert(const char *text) { }
    // const char *get_text() const { return ""; }
    // int text_height() const { return 0; }
    // bool text_changed() const { return false; }

private:
    static void wrapped_input_dispatch(Fl_Widget *_w, void *vp);
    void wrapped_input_cb();

    WrappedInput input;
    Fl_Window *owner_;
    bool ready;
};

#endif
