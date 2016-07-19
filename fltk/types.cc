// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include "types.h"
#include <ostream>


const ScoreTime ScoreTime::invalid = ScoreTime(-1);

std::ostream &
operator<<(std::ostream &os, const ScoreTime &pos)
{
    return os << pos._val << "t";
}

std::ostream &
operator<<(std::ostream &os, const ZoomInfo &z)
{
    return os << "ZoomInfo(" << z.offset << ", " << z.factor << ")";
}
