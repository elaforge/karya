#include <Fl/Fl_Tile.H>

class Track_tile : public Fl_Tile {
public:
	Track_tile(int X, int Y, int W, int H) :
		Fl_Tile(X, Y, W, H)
  {}
	// { box(FL_THIN_DOWN_BOX); }
};
