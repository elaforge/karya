// Copyright 2014 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

#include "Color.h"
#include "types.h"


// The selection on one track.  This is different from the haskell level
// selection in Ui.Types.Selection, that one is a contiguous block that
// can cover multiple tracks.
struct Selection {
    // None is taken by X11/X.h.
    enum Orientation { SelNone, Negative, Positive, Both };

    Selection() : start(ScoreTime::invalid), cur(ScoreTime::invalid),
        orientation(SelNone) {}
    Selection(Color color, ScoreTime start, ScoreTime cur,
            Orientation orientation)
        : color(color), start(start), cur(cur), orientation(orientation)
    {}
    bool empty() const;
    ScoreTime low() const { return std::min(start, cur); }
    ScoreTime high() const { return std::max(start, cur); }
    bool is_point() const { return start == cur; }

    Color color;
    ScoreTime start, cur;
    // This affects arrow existence or direction.
    Orientation orientation;
};

std::ostream &operator<<(std::ostream &os, const Selection &sel);
