// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <FL/Fl.H>
#include "Util/fltk_interface.h"

extern "C" {

void initialize() { Fl::lock(); }
void ui_wait() { Fl::wait(100); }
void ui_awake() { Fl::awake((void*) 0); }
int has_windows() { return Fl::first_window() != NULL; }

}
