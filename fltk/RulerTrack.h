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

#include "CachedScroll.h"
#include "Track.h"
#include "SelectionOverlay.h"
#include "RulerOverlay.h"
#include "Zoom.h"


class RulerTrack : public Track {
public:
    explicit RulerTrack(const RulerConfig &config);
    void resize(int x, int y, int w, int h) override;
    virtual Fl_Box &title_widget() override;
    virtual void set_selection(int selnum, const std::vector<Selection> &sels)
        override;
    virtual void set_zoom(const Zoom &new_zoom) override;
    virtual ScoreTime time_end() const override {
        return body.time_end();
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
    class Body : public Fl_Widget {
    public:
        Body(const RulerConfig &config);
        ScoreTime time_end() const {
            return ruler_overlay.time_end();
        }
        void update_size();
        RulerOverlay ruler_overlay;
        Zoom zoom;
    protected:
        void draw() override;
    };

    // I can't override redraw() since it's not virtual.  I could override it
    // anyway, but it seems sketchy.
    void invalidate() {
        redraw();
        body_scroll.invalidate();
    }

    // If created, this is owned by a Fl_Group, which deletes it.
    Fl_Box *title_box;
    CachedScroll body_scroll;
    Body body;

    SelectionOverlay selection_overlay;
};
