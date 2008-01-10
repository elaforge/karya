// for test code
#include <FL/Fl_Window.H>
#include <FL/Fl_Button.H>
#include <string.h>

Zoom *
test_zoom(int w, int h)
{
	int bw = 50;
	Zoom *z = new Zoom(0, 0, w, h, "test zoom");
	for (int i = 0; i <10; i++) {
		char buf[64];
		sprintf(buf, "btn %d", i);
		z->add(new Fl_Button(i * bw, 0, bw, bw, strdup(buf)));
	}
	z->zoom_speed.x = 100;
	z->resize(0, 0, w, h); // just to fix stupid scrollbars
	z->set_lower_right();
	return z;
}

int
main(int argc, char **argv)
{
	Fl_Window w(200, 200);
	w.resizable(w);
	Zoom *z = test_zoom(w.w(), w.h());
	z->zoom(Drect(100, 0, 100, 200));
	// Zoom *z = new Zoom(0, 0, 200, 200);
	w.add(z);
	w.show(argc, argv);
	return Fl::run();
}

const seq::Marklist *
tmarklist(const double zoom_factor)
{
}

