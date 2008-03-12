#ifndef __TYPES_H
#define __TYPES_H

#include "util.h"
#include <ostream>
#include <algorithm>
#include <math.h>

enum Orientation { HorizontalTime, VerticalTime };

// typedef double TrackPos;

class TrackPos {
public:
    TrackPos(double val) : _val(val) {}

    // Scale by a given factor, for zooming.
    double scale(double factor) const { return _val * factor; }

    // Basic arithmetic and comparisons work on a TrackPos.
#define OP(X) \
TrackPos operator X(const TrackPos &o) const { \
    TrackPos r(_val X o._val); return r; \
}
    OP(+) OP(-)
#undef OP
#define OP(X) bool operator X(const TrackPos &o) const { return _val X o._val; }
    OP(==) OP(!=) OP(<) OP(<=) OP(>) OP(>=)
#undef OP
    double _val;
private:
    friend std::ostream &operator<<(std::ostream &os, const TrackPos &pos);
};

inline std::ostream &
operator<<(std::ostream &os, const TrackPos &pos)
{
    os << "TrackPos(" << pos._val << ")";
}


struct Selection {
    int start_track;
    TrackPos start_pos;
    int end_track;
    TrackPos end_pos;
};


struct ZoomInfo {
    ZoomInfo() : offset(0), factor(1) {}
    TrackPos offset;
    // 1.0 means that each TrackPos gets 1 pixel.
    // 2.0 each TrackPos gets 2 pixels.
    // etc.
    double factor;

    // How many pixels is the given pos at, at this zoom?
    int to_pixels(TrackPos pos) const {
        // A TrackPos is not guaranteed to fit in an int, but (pos-offset)
        // should put it in range.
        double scaled = (pos-offset).scale(this->factor);
        return int(floor(std::max(double(INT_MIN), std::min(double(INT_MAX),
                            scaled))));
    }
};


struct TextStyle {
    char *font;
    char *style;
    int size;
    Color color;
};

#endif
