// Copyright 2014 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#ifndef __SELECTION_H
#define __SELECTION_H

#include "Color.h"
#include "types.h"


// The selection on one track.  This is different from the haskell level
// selection in Ui.Types.Selection, that one is a contiguous block that
// can cover multiple tracks.
struct Selection {
    Selection() : start(ScoreTime::invalid), cur(ScoreTime::invalid) {}
    Selection(Color color, ScoreTime start, ScoreTime cur, bool draw_arrow)
        : color(color), start(start), cur(cur), draw_arrow(draw_arrow)
    {}
    bool empty() const;
    ScoreTime low() const { return std::min(start, cur); }
    ScoreTime high() const { return std::max(start, cur); }
    bool is_point() const { return start == cur; }

    Color color;
    ScoreTime start, cur;
    // Draw an arrow at the 'cur' end of the selection.
    bool draw_arrow;
};

std::ostream &operator<<(std::ostream &os, const Selection &sel);

#endif
