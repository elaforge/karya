#ifndef __TYPES_H
#define __TYPES_H

#include <ostream>
#include <algorithm>
#include <math.h>
#include <limits.h>
#include "util.h"


// Display signals are in RealTime like all signals.  Since the score is in
// ScoreTime they have to be converted at draw time.
typedef double RealTime;

class ScoreTime {
public:
    ScoreTime() : _val(0) {}
    explicit ScoreTime(double val) : _val(val) {}
    static const ScoreTime invalid;
    bool negative_zero() const { return _val == 0 && signbit(_val); }

    // RealTime conversion.
    RealTime to_real() const {
        return RealTime(_val);
    }
    static ScoreTime from_real(RealTime t) {
        return ScoreTime(t);
    }

    // Scale by a given factor, for zooming.
    double scale(double factor) const { return _val * factor; }

    // I don't provide * and / directly because usually it doesn't make sense
    // to do that to ScoreTimes.  However, I wind up scaling them according to
    // zoom and warp so it happens anyway.
    ScoreTime divide(ScoreTime div) const {
        return ScoreTime(_val / div._val);
    }
    // TODO redundant with scale(), I should maybe get rid of that, or
    // overload (*) and (/).
    ScoreTime multiply(ScoreTime factor) const {
        return ScoreTime(_val * factor._val);
    }

    // Basic arithmetic and comparisons work on a ScoreTime.
    ScoreTime operator-() const { return ScoreTime(-_val); }
#define OP(X) \
ScoreTime operator X(const ScoreTime &o) const { \
    ScoreTime r(_val X o._val); return r; \
}
    OP(+) OP(-)
#undef OP
#define OP(X) bool operator X(const ScoreTime &o) const { return _val X o._val; }
    OP(==) OP(!=) OP(<) OP(<=) OP(>) OP(>=)
#undef OP
    // The only reason this isn't private is so the haskell FFI can see it.
    double _val;
private:
    friend std::ostream &operator<<(std::ostream &os, const ScoreTime &pos);
};

std::ostream &operator<<(std::ostream &os, const ScoreTime &pos);

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

std::ostream &operator<<(std::ostream &os, const Selection &sel);

struct TrackSelection {
    TrackSelection() : cur(ScoreTime::invalid) {}
    TrackSelection(const Selection &sel, int tracknum) {
        if (sel.low_track() <= tracknum && tracknum <= sel.high_track())
        {
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

// You divide by factor to go from ScoreTime -> pixels, so it can't be 0.
#define MINIMUM_FACTOR .0001

struct ZoomInfo {
    // ScoreTime are bigger than ints, so they have to be clipped.  If they
    // are clipped to INT_MIN/INT_MAX, overflow happens easily when they have
    // pixel offsets added to them.  So make it something with plenty of room
    // but still probably bigger than your monitor.
    enum { max_pixels = INT_MAX / 2 };
    ZoomInfo() : offset(0), factor(1) {}
    ZoomInfo(ScoreTime offset, double factor) :
        offset(offset), factor(std::max(MINIMUM_FACTOR, factor))
    {}
    bool operator==(const ZoomInfo &o) const {
        return offset == o.offset && factor == o.factor;
    }
    bool operator!=(const ZoomInfo &o) const { return !(*this == o); }

    // How many pixels is the given pos at, at this zoom?  This doesn't take
    // the zoom offset into account, so you'll have to subtract that from 'pos'
    // if you want a scroll position.
    int to_pixels(const ScoreTime pos) const {
        // A ScoreTime is not guaranteed to fit in an int.
        double scaled = pos.scale(this->factor);
        return int(round(std::max(double(-max_pixels),
                                  std::min(double(max_pixels), scaled))));
    }

    // Given the current zoom, this many pixels corresponds to how much
    // trackpos?
    ScoreTime to_time(int pixels) const {
        return ScoreTime(double(pixels) / factor);
    }

    ScoreTime offset;
    // 1.0 means that each ScoreTime gets 1 pixel.
    // 2.0 each ScoreTime gets 2 pixels.
    // etc.
    double factor;
};

std::ostream &operator<<(std::ostream &os, const ZoomInfo &z);

#endif
