// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

/* This adds track-specific stuff to a MoveTile.

    Fill the rightmost track edge until the right edge of the window with a pad
    box of the bg_color.

    Accept zoom callbacks from parent Zoom and Block.

    Tracks come in pairs of a title and body.

    TrackTile____________________________
       |                  \       \      \
    (Track::title_widget, Track)  ...  FloatingInput (floating_input, temporary)
*/

#ifndef __TRACK_TILE_H
#define __TRACK_TILE_H

#include <math.h>

#include <FL/Fl_Box.H>

#include "FloatingInput.h"
#include "MoveTile.h"
#include "Track.h"
#include "f_util.h"
#include "global.h"


class TrackTile : public MoveTile {
public:
    TrackTile(int x, int y, int w, int h, Color bg_color, int title_height);
    int handle(int evt) override;

    void set_bg_color(Color c) {
        track_pad.color(c.fl());
        track_pad.redraw();
    }
    void set_zoom(const Zoom &zoom);
    void set_title_height(int title_height) {
        this->title_height = title_height;
        this->update_sizes();
        this->redraw();
    }

    // Open a text input field at the given ScoreTime.  It will contain the
    // given text, selected with the given select range.
    void floating_open(int tracknum, ScoreTime pos, const char *text,
        int select_start, int select_end);
    void floating_close();
    // If there's an open floating edit input, insert text at the insertion
    // point.
    void floating_insert(const char *text);

    // ScoreTime of the end of the last event.
    ScoreTime time_end() const;
    // ScoreTime of the bottom of the visible window.
    ScoreTime view_end() const;
    // Visible amount of track.
    ScoreTime visible_time() const;
    // Right side of the rightmost track.
    int track_end() const;

    void insert_track(int tracknum, Track *track, int width);
    // Remove and return the Track, so the parent can delete it.
    Track *remove_track(int tracknum);
    // A track is a (title, body) pair, minus the track_pad.
    int tracks() const {
        return floor((children() - (bool(floating_input) ? 1 : 0)) / 2.0);
    }
    Track *track_at(int tracknum);
    const Track *track_at(int tracknum) const;
    int get_track_width(int tracknum) const;
    void set_track_width(int tracknum, int width);

    // Return the track currently being dragged right now, or -1.
    int get_dragged_track() const;

protected:
    void draw() override;

private:
    int title_height;
    Zoom zoom;
    Fl_Box track_pad; // box to take up space not covered by tracks
    // Created and destroyed when 'floating_open' is called.
    FloatingInput *floating_input;

    void update_sizes();
    static void title_input_cb_dispatch(Fl_Widget *w, void *arg);
    void title_input_cb(Fl_Widget *title);
};

#endif
