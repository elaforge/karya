// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#ifndef __WRAPPED_INPUT_H
#define __WRAPPED_INPUT_H

#include <string>
#include <FL/Fl_Multiline_Input.H>

#include "util.h"

// A customized Fl_Multiline_Input that wraps its text.
//
// Every time the text changes, figure out word wrapping, and insert newlines
// as apporpriate.  This only breaks on spaces, and newlines are not allowed
// in the input, so I can get unwrapped text back by converting newlines to
// spaces.
//
// Then call the callback.  If the number of lines has changed, the parent will
// resize it to fit.
class WrappedInput : public Fl_Multiline_Input {
public:
    WrappedInput(int X, int Y, int W, int H);
    void resize(int x, int y, int w, int h); // , bool no_wrap = false);
    void set_text(const char *text);
    const char *get_text() const;
    int text_height() const;
protected:
    int handle(int evt);
private:
    // void handle_keydown(int evt);
    bool wrap_text();
};

#endif

