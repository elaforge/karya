/* This adds track-specific stuff to a MoveTile.

Manage the underlying MoveTile:
Fill rightmost track edge until the right edge of the window with a pad box of
the given color.

Accept zoom callbacks from parent Zoom and BlockView.

Tracks come in pairs of a title and body.

TrackTile_______
   |             \
EventTrackTitle EventTrack
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
    virtual int handle(int evt);

    void set_bg_color(Color c) {
        track_pad.color(color_to_fl(c));
        track_pad.redraw();
    }
    void set_zoom(const ZoomInfo &zoom);
    void set_title_height(int title_height) {
        this->title_height = title_height;
        this->update_sizes();
        this->redraw();
    }

    // TrackPos of the end of the last event.
    TrackPos time_end() const;
    // TrackPos of the bottom of the visible window.
    TrackPos view_end() const;
    // Visible amount of track.
    TrackPos visible_time() const;
    // Right side of the rightmost track.
    int track_end() const;

    void insert_track(int tracknum, TrackView *track, int width);
    // Remove and return the TrackView, so the parent can delete it.
    TrackView *remove_track(int tracknum);
    // A track is a (title, body) pair, minus the track_pad.
    int tracks() const { return floor(children()/2.0); }
    TrackView *track_at(int tracknum);
    int get_track_width(int tracknum);
    void set_track_width(int tracknum, int width);

    // Return the track currently being dragged right now, or -1.
    int get_dragged_track() const;

protected:
    virtual void draw();
private:
    int title_height;
    ZoomInfo zoom;
    Fl_Box track_pad; // box to take up space not covered by tracks

    void update_sizes();
};

#endif
