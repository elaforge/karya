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

class TrackPos {
public:
    TrackPos() : _val(0) {}
    explicit TrackPos(double val) : _val(val) {}
    static const TrackPos invalid;
    bool negative_zero() const { return _val == 0 && signbit(_val); }

    // Scale by a given factor, for zooming.
    double scale(double factor) const { return _val * factor; }

    // Basic arithmetic and comparisons work on a TrackPos.
    TrackPos operator-() const { return TrackPos(-_val); }
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
    double _val;
private:
    friend std::ostream &operator<<(std::ostream &os, const TrackPos &pos);
};

std::ostream &operator<<(std::ostream &os, const TrackPos &pos);

struct Selection {
    Selection() : start_track(-1), cur_track(-1) {}
    Selection(Color color, int start_track, TrackPos start_pos, int cur_track,
            TrackPos cur_pos) :
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
    TrackPos start_pos;
    int cur_track;
    TrackPos cur_pos;
};

std::ostream &operator<<(std::ostream &os, const Selection &sel);

struct TrackSelection {
    TrackSelection() : cur(TrackPos::invalid) {}
    TrackSelection(const Selection &sel, int tracknum) {
        if (sel.low_track() <= tracknum && tracknum <= sel.high_track())
        {
            color = sel.color;
            start = sel.start_pos;
            cur = sel.cur_pos;
            is_cur_track = tracknum == sel.cur_track;
        } else {
            start = cur = TrackPos::invalid;
        }
    }
    bool empty() const {
        return cur == TrackPos::invalid;
    }
    TrackPos low() const { return std::min(start, cur); }
    TrackPos high() const { return std::max(start, cur); }
    bool is_point() const { return start == cur; }
    Color color;
    TrackPos start, cur;
    // True if this track is the the start or cur track of the selection.
    bool is_cur_track;
};

// You divide by factor to go from TrackPos -> pixels, so it can't be 0.
#define MINIMUM_FACTOR .0001

struct ZoomInfo {
    // TrackPos are bigger than ints, so they have to be clipped.  If they
    // are clipped to INT_MIN/INT_MAX, overflow happens easily when they have
    // pixel offsets added to them.  So make it something with plenty of room
    // but still probably bigger than your monitor.
    enum { max_pixels = INT_MAX / 2 };
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
        return int(floor(std::max(double(-max_pixels),
                                  std::min(double(max_pixels), scaled))));
    }

    // Given the current zoom, this many pixels corresponds to how much
    // trackpos?
    TrackPos to_trackpos(int pixels) const {
        return TrackPos(double(pixels) / factor);
    }

    TrackPos offset;
    // 1.0 means that each TrackPos gets 1 pixel.
    // 2.0 each TrackPos gets 2 pixels.
    // etc.
    double factor;
};

std::ostream &operator<<(std::ostream &os, const ZoomInfo &z);


struct TextStyle {
    int font; // font and face as an fltk font index
    int size;
    Color color;
};

#endif
