#include <FL/Fl_Scrollbar.H>

class A_scrollbar : public Fl_Scrollbar {
public:
	A_scrollbar(int X, int Y, int W, int H, char *label = 0) :
		Fl_Scrollbar(X, Y, W, H, label)
	{}
};
