#ifndef __TYPES_H
#define __TYPES_H

#include "util.h"
#include <ostream>
#include <algorithm>
#include <math.h>

// Pass the addresses of callbacks to this before they are replaced.  Won't be
// valid outside of the specific function that passes it, such as update() or
// remove_track().
typedef void (*FinalizeCallback)(void *callback);

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
    Selection() : start_track(0), tracks(0) {}
    Selection(Color color) : color(color), start_track(0), tracks(0) {}
    Selection(Color color, int start_track, TrackPos start_pos, int tracks,
            TrackPos duration) :
        color(color), start_track(start_track), start_pos(start_pos),
        tracks(tracks), duration(duration)
    {}

    bool operator==(const Selection &o) const {
        return color == o.color && start_track == o.start_track
            && start_pos == o.start_pos && tracks == o.tracks
            && duration == o.duration;
    }
    bool operator!=(const Selection &o) const { return !(*this == o); }
    // bool no_selection() const { return tracks == 0; }

    Color color;
    int start_track;
    TrackPos start_pos;
    int tracks;
    TrackPos duration;
};

struct TrackSelection {
    TrackSelection() : start(TrackPos(0)), end(TrackPos(-1)) {}
    TrackSelection(const Selection &sel, int tracknum) {
        if (sel.start_track <= tracknum
                && tracknum < sel.start_track + sel.tracks)
        {
            color = sel.color;
            start = sel.start_pos;
            end = sel.start_pos + sel.duration;
        } else {
            start = TrackPos(0);
            end = TrackPos(-1);
        }
    }
    bool empty() const { return end < start; }
    /*
    bool operator==(const Selection &o) const {
        return color == o.color && start == o.start && duration == o.duration;
    }
    bool operator!=(const Selection &o) const { !(*this == o); }
    */
    Color color;
    TrackPos start, end;
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

    // How many pixels is the given pos at, at this zoom?  This doesn't take
    // the zoom offset into account, so you'll have to subtract that from 'pos'
    // if you want a scroll position.
    int to_pixels(const TrackPos pos) const {
        // A TrackPos is not guaranteed to fit in an int.
        double scaled = pos.scale(this->factor);
        return int(floor(std::max(double(INT_MIN), std::min(double(INT_MAX),
                            scaled))));
    }

    // Given the current offset and zoom, this many pixels corresponds to
    // how much trackpos?
    TrackPos to_trackpos(int pixels) const {
        return TrackPos(double(pixels) / factor);
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
