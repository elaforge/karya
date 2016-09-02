// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#ifndef __INPUT_UTIL_H
#define __INPUT_UTIL_H

#include <FL/Fl_Input.H>


// Code common to text entry widgets.
namespace input_util {

bool should_ignore(int etv);
// Handle common keys, and return true if they were handled.
bool handle(Fl_Input *input, int evt, bool multiline);

// True if it stripped spaces from the value().
bool strip_value(Fl_Input *w);

}

#endif
