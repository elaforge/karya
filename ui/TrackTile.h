/*
Manage the underlying MoveTile:
Fill rightmost track edge until the right edge of the window with a pad box of
the given color.

Accept zoom callbacks from parent Zoom and BlockView.

*/
#ifndef __TRACK_TILE_H
#define __TRACK_TILE_H

#include <FL/Fl_Box.H>

#include "util.h"
#include "f_util.h"

#include "MoveTile.h"
#include "Track.h"


class TrackTile : public MoveTile {
public:
    TrackTile(int X, int Y, int W, int H, Color bg_color);

    void set_bg_color(Color c) { track_pad.color(color_to_fl(c)); }

    int tracks() const { return children()-1; } // not counting track_pad
    TracklikeView *track_at(int at);
    void insert_track(int at, TracklikeView *track, int width);
    // Remove and return the TracklikeView, so the parent can delete it.
    TracklikeView *remove_track(int at);
private:
    Fl_Box track_pad; // box to take up space not covered by tracks

    void update_sizes();
};

#endif
