#include <FL/Fl_Double_Window.H>
#include <FL/Fl.H>
#include <FL/Fl_Box.H>
#include <FL/fl_draw.H>

#include "util.h"
#include "futil.h"

#include <iostream>

class W : public Fl_Box {
public:
	W(int X, int Y, int W, int H) :
		Fl_Box(X, Y, W, H)
	{ box(FL_FLAT_BOX); color(FL_BLUE); }
protected:
	int handle(int ev) {
		std::cout << show_event(ev) << '\n';
		return 1;
	}
};

int
main(int argc, char **argv)
{
	Fl_Double_Window win(200, 200);
	win.resizable(win);
	W w(0, 0, 200, 200);
	win.show(argc, argv);
	return Fl::run();
}
