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
// Then call the callback.  If the number of lines has changed, 'text_height'
// will return its new height and the parent should resize it appropriately.
class WrappedInput : public Fl_Multiline_Input {
public:
    WrappedInput(int x, int y, int w, int h, bool strip);
    void resize(int x, int y, int w, int h) override;
    // Use this to set newline-free unwrapped text.
    void set_text(const char *text);
    const char *get_text() const;
    // True if the text has changed since the last time this widget got focus.
    // This is used to detect if the edit's changes were reverted by the escape
    // key.
    bool text_changed() const {
        return last_text != value();
    }
    int text_height() const;
    int handle(int evt) override;
private:
    bool wrap_text();
    // Strip spaces from the text on unfocus.
    const bool strip;
    // Keep the previous text, to revert on escape.
    std::string last_text;
};

#endif
