// Copyright 2016 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

#include <vector>

#include "Selection.h"
#include "Zoom.h"


// This holds the set of selections for a single track, and can draw them.
class SelectionOverlay {
public:
    void draw(int x, int track_start, int w, int track_end, const Zoom &zoom);
    const std::vector<Selection> &get(int selnum);
    void set(int selnum, const std::vector<Selection> &news);

    // Height in pixels both above and below  of the special indicator that is
    // drawn on a 0 size selection.
    const static int selection_point_size = 6;
private:
    // Selections indexed by selnum.
    std::vector<std::vector<Selection>> selections;
};
