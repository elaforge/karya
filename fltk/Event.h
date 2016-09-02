// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

/* Events have a start and duration ScoreTime.  The duration can be 0.
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


// Events are immutable, but as usual I can't use const since I assign them by
// value into STL containers.
struct Event {
    // This has a default contsructor so I can assign it by value into
    // the EventTrackModel::Events map.
    Event() : start(0), duration(0), style_id(0) {}
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

    ScoreTime start;
    ScoreTime duration;
    // std::string would be nicer but I can't serialize to that from haskell.
    // This won't be modified, but will be freed, so const_cast for that.
    const char *text;
    StyleId style_id;
};

inline std::ostream &
operator<<(std::ostream &os, const Event &e)
{
    return os << "Event(" << e.start << ", " << e.duration << ", "
        << int(e.style_id) << ")";
}

#endif
