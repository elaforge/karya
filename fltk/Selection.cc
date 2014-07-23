// Copyright 2014 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <ostream>
#include "Color.h"
#include "Selection.h"


std::ostream &
operator<<(std::ostream &os, const Selection &sel)
{
    return os << "Selection(" << sel.start_track << ", " << sel.start_pos
        << ", " << sel.cur_track << ", " << sel.cur_pos << ")";
}
