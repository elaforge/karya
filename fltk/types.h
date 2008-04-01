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
    TrackPos() : _val(0) {}
    TrackPos(double val) : _val(val) {}
    // Used by EventTrack::create_widget for "everything"
    // static TrackPos max_pos;

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
    // The only reason this isn't private is so the haskell FFI can see it.
    // Lets have lots of space.
    long long _val;
private:
    friend std::ostream &operator<<(std::ostream &os, const TrackPos &pos);
};

inline std::ostream &
operator<<(std::ostream &os, const TrackPos &pos)
{
    os << "TrackPos(" << pos._val << ")";
}


// "No selection" is if 'tracks' is 0.
struct Selection {
    Selection() : start_track(0), start_pos(0), tracks(0), duration(0) {}
    Selection(int start_track, TrackPos start_pos, int tracks,
            TrackPos duration) :
        start_track(start_track), start_pos(start_pos),
        tracks(tracks), duration(duration)
    {}

    int start_track;
    TrackPos start_pos;
    int tracks;
    TrackPos duration;
};

// You divide by factor to go from TrackPos -> pixels, so it can't be 0.
#define MINIMUM_FACTOR .0001

struct ZoomInfo {
    ZoomInfo() : offset(0), factor(1) {}
    ZoomInfo(TrackPos offset, double factor) :
        offset(offset), factor(std::max(MINIMUM_FACTOR, factor))
    {}
    bool operator==(const ZoomInfo &o) {
        return offset == o.offset && factor == o.factor;
    }
    bool operator!=(const ZoomInfo &o) { return !(*this == o); }

    // How many pixels is the given pos at, at this zoom?
    // This subtracts 'offset' to get an absolute screen position, which means
    // if you're using it to get a "duration" in pixels you have to add offset
    // back on.
    int to_pixels(const TrackPos pos) const {
        // A TrackPos is not guaranteed to fit in an int, but (pos-offset)
        // should put it in range.
        double scaled = (pos-offset).scale(this->factor);
        return int(floor(std::max(double(INT_MIN), std::min(double(INT_MAX),
                            scaled))));
    }

    // Given the current offset and zoom, this many pixels corresponds to
    // how much trackpos?
    // As above, this adds 'offset', so if you want trackpos/pixel, subtract it
    // back off.
    TrackPos to_trackpos(int pixels) const {
        return TrackPos(double(pixels) / factor) + offset;
    }

    TrackPos offset;
    // 1.0 means that each TrackPos gets 1 pixel.
    // 2.0 each TrackPos gets 2 pixels.
    // etc.
    double factor;
};

inline std::ostream &
operator<<(std::ostream &os, const ZoomInfo &z)
{
    os << "ZoomInfo(" << z.offset << ", " << z.factor << ")";
}


struct TextStyle {
    int font; // font and face as an fltk font index
    int size;
    Color color;
};

#endif
