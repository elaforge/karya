// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

/* This adds track-specific stuff to a MoveTile.

    Manage the underlying MoveTile:
    Fill rightmost track edge until the right edge of the window with a pad box
    of the given color.

    Accept zoom callbacks from parent Zoom and BlockView.

    Tracks come in pairs of a title and body.

    TrackTile________________
       |           \         \
    title_input  EventTrack  WrappedInput (edit_input, temporary)
*/

#ifndef __TRACK_TILE_H
#define __TRACK_TILE_H

#include <math.h>

#include <FL/Fl_Box.H>

#include "util.h"
#include "f_util.h"

#include "MoveTile.h"
#include "Track.h"
#include "WrappedInput.h"


class TrackTile : public MoveTile {
public:
    TrackTile(int X, int Y, int W, int H, Color bg_color, int title_height);
    int handle(int evt) override;

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

    // Open a text input field at the given ScoreTime.  It will contain the
    // given text, selected with the given select range.
    //
    // The edit_input is handled by TrackTile, so I can't put one on the ruler
    // track.  Also, when I report the tracknum to the MsgCollector, I should
    // report the absolute tracknum, not the TrackTile relative one.  So this
    // method takes an unadjusted absolute tracknum, and subtracts one
    // internally, except for tracknum 0 of course.
    void edit_open(int tracknum, ScoreTime pos, const char *text,
        int select_start, int select_end);
    void edit_close();
    // If there's an open edit field, insert text at the insertion point.
    void edit_insert(const char *text);

    // ScoreTime of the end of the last event.
    ScoreTime time_end() const;
    // ScoreTime of the bottom of the visible window.
    ScoreTime view_end() const;
    // Visible amount of track.
    ScoreTime visible_time() const;
    // Right side of the rightmost track.
    int track_end() const;
    // Visible width and height.
    IPoint visible_pixels() const;

    void insert_track(int tracknum, TrackView *track, int width);
    // Remove and return the TrackView, so the parent can delete it.
    TrackView *remove_track(int tracknum);
    // A track is a (title, body) pair, minus the track_pad.
    int tracks() const {
        return floor((children() - (edit_input ? 1 : 0)) / 2.0);
    }
    TrackView *track_at(int tracknum);
    const TrackView *track_at(int tracknum) const;
    int get_track_width(int tracknum) const;
    void set_track_width(int tracknum, int width);

    // Return the track currently being dragged right now, or -1.
    int get_dragged_track() const;

private:
    int title_height;
    ZoomInfo zoom;
    Fl_Box track_pad; // box to take up space not covered by tracks
    // Created and destroyed when 'edit_open' is called.
    WrappedInput *edit_input;

    void update_sizes();
    static void edit_input_cb(Fl_Widget *_w, void *vp);
    static void title_input_cb(Fl_Widget *_w, void *vp);
};

#endif
