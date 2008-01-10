#include <FL/Fl_Scrollbar.H>

class S_scrollbar : public Fl_Scrollbar {
public:
	S_scrollbar(int X, int Y, int W, int H, char *label = 0) :
		Fl_Scrollbar(X, Y, W, H, label)
	{}
};
