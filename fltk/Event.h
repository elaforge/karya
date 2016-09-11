// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

/* Events have a start and duration ScoreTime.  The duration can be 0, or
    even -0.
*/

#ifndef __EVENT_H
#define __EVENT_H

#include <ostream>
#include <string>
#include <vector>

#include <FL/Fl_Box.H>

#include "StyleTable.h"
#include "types.h"
#include "util.h"


struct Event {
    Event(ScoreTime start, ScoreTime duration, const char *text,
            StyleId style_id, bool align_to_bottom = false) :
        start(start), duration(duration), text(text), style_id(style_id)
    {}
    bool is_negative() const {
        return duration < ScoreTime(0) || duration.negative_zero();
    }
    bool is_positive() const { return !is_negative(); }
    ScoreTime min() const { return std::min(start, start + duration); }
    ScoreTime max() const { return std::max(start, start + duration); }

    const ScoreTime start;
    const ScoreTime duration;
    // std::string would be nicer but I can't serialize to that from haskell.
    // This won't be modified, but will be freed, so const_cast for that.
    const char *text;
    const StyleId style_id;
};

inline std::ostream &
operator<<(std::ostream &os, const Event &e)
{
    return os << "Event(" << e.start << ", " << e.duration << ", "
        << int(e.style_id) << " '" << (e.text ? e.text : "") << "'" << ")";
}

#endif
