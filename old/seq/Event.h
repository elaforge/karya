#include <FL/Fl_Widget.H>
#include <FL/Fl_Box.H>

/*
Ruler layer draws ruler marks.

Draw starting point, event text, and waveform.  Handle graceful collapsing of
text if there's no room.  Do special tab placement for sub-events.

Styles of waveform drawing:
half-waveform filled or outline
full (+ and -) filled or outline
decimal or hex numbers spaced out
*/

namespace widgets {

class Event : public Fl_Box {
public:
	Event(int X, int Y, int W, int H) :
		Fl_Box(X, Y, W, H), _text_color(FL_BLACK)
	{}
	void text_color(Fl_Color c) { _text_color = c; }
private:
	Fl_Color _text_color;
};

}
