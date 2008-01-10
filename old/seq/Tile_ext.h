#include "Fl_Tile2.H"

/*
- configurable drag_area
- min_size - children will not get smaller than this
	change position()
- after a resizing, init_sizes() is called.  this means if the tile is later resized
children retain their positions
	we can do this with a callback, since callbacks are called on every DRAG


Tracks stack vertically, with variable spacing (to indicate 'linked' and 'non-linked'
tracks).
Below the bottom track is a solid area (this can be either special cased or simply
a padding track handled by track_tile).
To the right of each track is a solid area.  This could once again simply be a padding
widget.  It could be handled by the Track, but then the track would have to deal
with dragging the right edge, instead of leaving that to the tile.

Dragging tracks horizontally or vertically sends dimension() msgs to the track.  This
is because track dimensions are a different type from screen pixels, so we can't
use resize().  Or it could send resize(), and tracks can change that into Trackpos
since they know their zoom.  But there's still the problem that dimensions() is not
size, because vsize varies by zoom, and hsize is limited to window size.

If tile sends resizes (this is nice because it lets tile work with things other than track,
and doesn't have to handle the padding widgets (Fl_Boxes) specially):
dh changes dimension.y to h().to_screen(zoom.y), which means tracks now have
to know their yzoom.  dw changes dimension.x to (offset+w()).to_screen(zoom.x).
dx and dy just moves x() and y().  dy will happen when this track gets pushed around
by others resizing.  dx shouldn't ever happen since all tracks start at 0.

When track_tile gets zoom msgs, it can't send 'resize' to the widgets because that
would change their actual dimensions.  For hzoom:
If the widget takes up the whole screen, no problem, just forward the zoom.
Otherwise, we may have to change the w() of the widget, but without changing its
dimensions.  Or, we could just forward the zoom and let the track change its size,
to 

It seems like the simplest thing is that resize() affects *only* the track's display size.
This means the tile has to send dimension() changes in addition to resizes when
the track is dragged.  This shouldn't be difficult if the tile calls a callback when things
are dragged, which is what Fl_Tile in fact does.  When the tile gets zoom()s, it
forwards the zoom to all tracks and then resizes appropriately.

Do I want a minimum track size?  It's useful to allow tracks to collapse down to 0,
but it would also be nice to resize a track and *move* tracks around it instead of
squishing or expanding them.  Dragging bottom of widget resizes widget and moves
ones below it.  Dragging the top of the widget is just dragging the bottom of the above
widget (if there is one).


So Tile_ext provides:
two modes for dragging children: resize neighbors and move neighbors
in fact, I don't see where resizing neighbors would ever be useful
callback

Track_tile:
sets callback to: set track dimensions() based on current widget sizes.
xdim is only modified if the current xdim is not what it "should" be according to
offset and zoom calculation.  Padding widgets are adjusted appropriately.


*/

namespace widgets {

class Tile_ext : public Fl_Tile2 {
public:
	Tile_ext(int X, int Y, int W, int H, char *label = 0) :
		Fl_Tile2(X, Y, W, H, label),
		min_size(0, 0), drag_area(100)
	{ }
	virtual int handle(int event);
	// void zoom_resize(int X, int Y, int W, int H);
	Point min_size;
	int drag_area;
private:
	enum Drag_state { No_drag = 0, Drag_x, Drag_y, Drag_both };
};

}
