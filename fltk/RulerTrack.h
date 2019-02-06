// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

/* Rulers appear in two places: on a dedicated ruler track, and as an overlay
    on an event track.  The former can be implemented as an overlay onto a
    plain box.  There are some differences though: I may want to disable names
    for the event track overlay, and alpha for the ruler track.
*/

#pragma once

#include <FL/Fl_Box.H>

#include "Track.h"
#include "SelectionOverlay.h"
#include "RulerOverlay.h"


class RulerTrack : public Track {
public:
    explicit RulerTrack(const RulerConfig &config);
    virtual Fl_Box &title_widget() override;
    virtual void set_selection(int selnum, const std::vector<Selection> &sels)
        override;
    virtual ScoreTime time_end() const override {
        return ruler_overlay.time_end();
    }
    virtual void update(const Tracklike &track, ScoreTime start, ScoreTime end)
        override;
    virtual void set_track_signal(const TrackSignal &tsig) override {
        DEBUG("WARNING: got a track signal on a ruler track!");
    }
    virtual void finalize_callbacks() override;
    virtual std::string dump() const override;

protected:
    void draw() override;

private:
    // If created, this is owned by a Fl_Group, which deletes it.
    Fl_Box *title_box;
    Fl_Box bg_box;
    RulerOverlay ruler_overlay;
    SelectionOverlay selection_overlay;
};
