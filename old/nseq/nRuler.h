#include <FL/Fl_Group.H>

#include "marklist.h"

/*

Overlay_ruler draws ruler lines over the objects inside it
Selection_ruler extends Overlay_ruler to allow selection
Ruler is a Selection_ruler that always has a solid colored box beneath it

*/


class Overlay_ruler : public Fl_Group {
public:
	Overlay_ruler(int X, int Y, int W, int H) :
		Fl_Group(X, Y, W, H),
		show_names(true)
	{}
	void resize(int X, int Y, int W, int H);
	void draw();
	void orientation(Orientation o) {
	}

	void zoom() {}
	void scroll() {}

	void insert_marklist(int i, const Marklist *m) {}
	void remove_marklist(int i) {}
protected:
	bool show_names;
};

class Selection_ruler : public Overlay_ruler {
};

class Ruler : public Selection_ruler {
};

