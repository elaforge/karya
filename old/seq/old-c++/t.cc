#include <FL/Fl.H>
#include <FL/Enumerations.H>
#include <FL/fl_draw.H>
#include <FL/Fl_Window.H>
#include <FL/Fl_Scroll.H>
#include <FL/Fl_Button.H>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

class Zoom : public Fl_Scroll {
protected:
	void draw();
	// int handle(int ev);
public:
	Zoom(int,int,int,int, const char * = 0);
};

Zoom::Zoom(int x, int y, int w, int h, const char *label) :
	Fl_Scroll(x, y, w, h, label)
{
	// type(0);
}

/*
int
Zoom::handle(int ev)
{
	printf("%d\n", ev);
}
*/

void
Zoom::draw()
{
	if (damage() & (FL_DAMAGE_ALL | FL_DAMAGE_SCROLL)) {
		fl_color(FL_WHITE);
		fl_rectf(x(), y(), w(), h());
	}
	Fl_Scroll::draw();
}

int
main(int argc, char **argv)
{
	int w = 300, h = 200;
	Fl_Window *win = new Fl_Window(w, h);
	// Zoom z(0, 0, w, h, "track");
	Fl_Scroll *scroll = new Fl_Scroll(0, 0, w, h);
	scroll->type(Fl_Scroll::BOTH_ALWAYS);
	for (int i = 0; i < 10; i++) {
		char label[32];
		snprintf(label, 32, "btn %d", i);
		Fl_Button *b = new Fl_Button(0, i*75, 50, 40, strdup(label));
	}
	scroll->end();
	win->end();
	win->show(argc, argv);
	exit(Fl::run());
}
