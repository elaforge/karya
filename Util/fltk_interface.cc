#include <FL/Fl.H>
#include <FL/x.H>
#include "Util/fltk_interface.h"

extern "C" {

void initialize() { Fl::lock(); }
void ui_wait() { Fl::wait(100); }
void ui_awake() { Fl::awake((void*) 0); }
int has_windows() { return Fl_X::first != NULL; }

}
