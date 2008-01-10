#include <FL/Fl_Input.H>

// a text-input box that behaves like acme
// and calls a callback for each letter typed and is otherwise configurable
class A_input : public Fl_Input {
public:
	A_input(int X, int Y, int W, int H, char *label = 0)
		: Fl_Input(X, Y, W, H, label)
	{}
};
