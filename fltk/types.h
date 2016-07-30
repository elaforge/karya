// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#ifndef __TYPES_H
#define __TYPES_H

#include <ostream>
#include <algorithm>
#include <cmath>
#include <limits.h>
#include "Color.h"


// Display signals are in RealTime like all signals.  Since the score is in
// ScoreTime they have to be converted at draw time.
typedef double RealTime;

class ScoreTime {
public:
    ScoreTime() : _val(0) {}
    explicit ScoreTime(double val) : _val(val) {}
    static const ScoreTime invalid;
    bool negative_zero() const { return _val == 0 && std::signbit(_val); }

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
#define OP(X) bool operator X(const ScoreTime &o) const {return _val X o._val;}
    OP(==) OP(!=) OP(<) OP(<=) OP(>) OP(>=)
#undef OP
    // The only reason this isn't private is so the haskell FFI can see it.
    double _val;
private:
    friend std::ostream &operator<<(std::ostream &os, const ScoreTime &pos);
};

std::ostream &operator<<(std::ostream &os, const ScoreTime &pos);

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
