// Copyright 2014 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#ifndef __SELECTION_H
#define __SELECTION_H

#include "Color.h"
#include "types.h"


struct Selection {
    Selection() : start_track(-1), cur_track(-1) {}
    Selection(Color color, int start_track, ScoreTime start_pos, int cur_track,
            ScoreTime cur_pos) :
        color(color), start_track(start_track), start_pos(start_pos),
        cur_track(cur_track), cur_pos(cur_pos)
    {}

    bool operator==(const Selection &o) const {
        return color == o.color && start_track == o.start_track
            && start_pos == o.start_pos && cur_track == o.cur_track
            && cur_pos == o.cur_pos;
    }
    bool operator!=(const Selection &o) const { return !(*this == o); }

    // Both being -1 ensures that checking 'low <= track <= high' will be false
    // for an empty selection.
    bool empty() const { return cur_track == -1 && start_track == -1; }
    int low_track() const { return std::min(start_track, cur_track); }
    int high_track() const { return std::max(start_track, cur_track); }

    Color color;
    int start_track;
    ScoreTime start_pos;
    int cur_track;
    ScoreTime cur_pos;
};

struct TrackSelection {
    TrackSelection() : cur(ScoreTime::invalid) {}
    TrackSelection(const Selection &sel, int tracknum) {
        if (sel.low_track() <= tracknum && tracknum <= sel.high_track()) {
            color = sel.color;
            start = sel.start_pos;
            cur = sel.cur_pos;
            is_cur_track = tracknum == sel.cur_track;
        } else {
            start = cur = ScoreTime::invalid;
        }
    }
    bool empty() const {
        return cur == ScoreTime::invalid;
    }
    ScoreTime low() const { return std::min(start, cur); }
    ScoreTime high() const { return std::max(start, cur); }
    bool is_point() const { return start == cur; }
    Color color;
    ScoreTime start, cur;
    // True if this track is the the start or cur track of the selection.
    bool is_cur_track;
};

std::ostream &operator<<(std::ostream &os, const Selection &sel);

#endif
