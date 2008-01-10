#ifndef _ZOOMER_H
#define _ZOOMER_H

#include <FL/Fl_Group.H>

class Zoomer : public Fl_Group {
public:
	Zoomer(int X, int Y, int W, int H) :
		Fl_Group(X, Y, W, H)
	{}
}


#endif
