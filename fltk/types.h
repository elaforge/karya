// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

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

    ScoreTime operator+(const ScoreTime &o) const {
        return ScoreTime(_val + o._val);
    }
    ScoreTime operator-(const ScoreTime &o) const {
        return ScoreTime(_val - o._val);
    }

#define OP(X) bool operator X(const ScoreTime &o) const {return _val X o._val;}
    OP(==) OP(!=) OP(<) OP(<=) OP(>) OP(>=)
#undef OP

    // The only reason this isn't private is so the haskell FFI can see it.
    double _val;
private:
    friend std::ostream &operator<<(std::ostream &os, const ScoreTime &pos);
};

std::ostream &operator<<(std::ostream &os, const ScoreTime &pos);
