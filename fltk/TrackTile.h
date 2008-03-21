/*
Manage the underlying MoveTile:
Fill rightmost track edge until the right edge of the window with a pad box of
the given color.

Accept zoom callbacks from parent Zoom and BlockView.

Tracks come in pairs of a title and body.  

TrackTile_______
   |             \
EventTrackTitle EventTrack

TODO:
MoveTile will also have to report moves (in the callback?) which can then be
used to resize the left_body.

Only resize the pad when the widget resizes, not the children.  Setting
resizable or using an Fl_Scroll could solve this.

*/
#ifndef __TRACK_TILE_H
#define __TRACK_TILE_H

#include <math.h>

#include <FL/Fl_Box.H>

#include "util.h"
#include "f_util.h"

#include "MoveTile.h"
#include "Track.h"


class TrackTile : public MoveTile {
public:
    TrackTile(int X, int Y, int W, int H, Color bg_color, int title_height);

    void set_bg_color(Color c) { track_pad.color(color_to_fl(c)); redraw(); }
    void set_zoom(const ZoomInfo &zoom);

    // TrackPos of the end of the last event.
    TrackPos time_end() const;
    // Right side of the rightmost track.
    int track_end() const;

    void insert_track(int at, TrackView *track, int width);
    // Remove and return the TrackView, so the parent can delete it.
    TrackView *remove_track(int at);
    // A track is a (title, body) pair, minus the track_pad.
    int tracks() const { return floor(children()/2.0); }
    TrackView *track_at(int at);
    int get_track_width(int at);
    void set_track_width(int at, int width);

    // Return the track currently being dragged right now, or -1.
    int get_dragged_track() const;
private:
    int title_height;
    Fl_Box track_pad; // box to take up space not covered by tracks

    void update_sizes();
};

#endif
