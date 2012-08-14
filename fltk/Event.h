/* Events have a start and duration ScoreTime.  The duration can be 0.
*/

#ifndef __EVENT_H
#define __EVENT_H

#include <string>
#include <vector>

#include <FL/Fl_Box.H>

#include "util.h"
#include "types.h"
#include "StyleTable.h"


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

    ScoreTime start;
    ScoreTime duration;
    // std::string would be nicer but I can't serialize to that from haskell.
    // This won't be modified, but will be freed, so const_cast for that.
    const char *text;
    StyleId style_id;
};

#endif
