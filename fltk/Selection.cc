// Copyright 2014 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <ostream>

#include "Color.h"

#include "Selection.h"


bool
Selection::empty() const
{
    return cur == ScoreTime::invalid;
}


std::ostream &
operator<<(std::ostream &os, const Selection &sel)
{
    return os << "Selection(" << sel.color << ", " << sel.start
        << ", " << sel.cur << ", " << sel.orientation << ")";
}
