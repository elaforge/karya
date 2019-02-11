// Copyright 2019 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

#include "types.h"

// You divide by factor to go from ScoreTime -> pixels, so it can't be 0.
#define MINIMUM_FACTOR .0001

struct Zoom {
    // ScoreTime are bigger than ints, so they have to be clipped.  If they
    // are clipped to INT_MIN/INT_MAX, overflow happens easily when they have
    // pixel offsets added to them.  So make it something with plenty of room
    // but still probably bigger than your monitor.
    enum { max_pixels = INT_MAX / 2 };
    Zoom() : offset(0), factor(1) {}
    Zoom(ScoreTime offset, double factor) :
        offset(offset), factor(std::max(MINIMUM_FACTOR, factor))
    {}
    bool operator==(const Zoom &o) const {
        return offset == o.offset && factor == o.factor;
    }
    bool operator!=(const Zoom &o) const { return !(*this == o); }

    // How many pixels is the given pos at, at this zoom?  This doesn't take
    // the zoom offset into account, so you'll have to subtract that from 'pos'
    // if you want a scroll position.
    int to_pixels(const ScoreTime pos) const {
        double scaled = to_pixels_d(pos);
        // A ScoreTime is not guaranteed to fit in an int.
        return int(round(std::max(double(-max_pixels),
                                  std::min(double(max_pixels), scaled))));
    }
    double to_pixels_d(const ScoreTime pos) const {
        return pos.scale(this->factor);
    }

    // Given the current zoom, this many pixels corresponds to how much
    // trackpos?
    ScoreTime to_time(int pixels) const {
        return to_time_d(double(pixels));
    }
    ScoreTime to_time_d(double pixels) const {
        return ScoreTime(pixels / factor);
    }

    ScoreTime offset;
    // 1.0 means that each ScoreTime gets 1 pixel.
    // 2.0 each ScoreTime gets 2 pixels.
    // etc.
    double factor;
};

inline std::ostream &
operator<<(std::ostream &os, const Zoom &z)
{
    return os << "Zoom(" << z.offset << ", " << z.factor << ")";
}

#undef MINIMUM_FACTOR
