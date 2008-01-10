#include <FL/Fl.H>
#include <FL/Fl_Double_Window.H>
#include "Block.h"
#include "types.h"

struct Defaults {
	int title_size, sb_size, ruler_size;
};

Defaults
defaults()
{
	Defaults d;
	d.title_size = 20;
	d.sb_size = 16;
	d.ruler_size = 24;
	return d;
}

int
main(int argc, char **argv)
{
	Fl_Double_Window win(300, 300);
	Defaults d = defaults();
	win.resizable(win);

	Block block(0, 0, 300, 300);
	Orientation o(Vertical_time);
	block.update_size(d.title_size, d.ruler_size, d.sb_size, o);
	win.show(argc, argv);
	Fl::run();
}
