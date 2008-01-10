#include <string.h>
#include <stdio.h>

#include <FL/Fl.H>
#include <FL/Fl_Double_Window.H>
#include <FL/Fl_Box.H>

#include "Track_tile.H"

// #include <FL/Fl_Tile.H>

int
main(int argc, char **argv)
{
	int w = 400, h = 300;
	int ntracks = 4;
	Fl_Double_Window win(w, h);
	win.resizable(win);

	Track_tile tile(0, 0, w, h);

	/*
	Fl_Box b(0, 0, w, h, "1");
		b.box(FL_DOWN_BOX);
		b.color(9);
	*/

	int tw = w / ntracks;
	for (int i=0; i < ntracks; i++) {
		char buf[32];
		sprintf(buf, "%d", i);
		Fl_Box *b = new Fl_Box(i*tw, 0, tw, h, strdup(buf));
		b->box(FL_DOWN_BOX);
		b->color(9);
	}

	win.show(argc, argv);
	Fl::run();
}
