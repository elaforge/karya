// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

// generic utilities

#ifndef __GEOM_UTIL_H
#define __GEOM_UTIL_H

#include <iostream>
#include <stdexcept>
#include <vector>
#include <utility>


namespace geom {

// Point

template<class T>
struct PointTmpl {
    T x, y;
    PointTmpl(T x, T y) : x(x), y(y) {};
    PointTmpl() : x(), y() {};
    bool operator==(const PointTmpl<T> &o) const {
        return o.x == x && o.y == y;
    }
    bool operator!=(const PointTmpl<T> &o) const { return !(*this == o); }
    PointTmpl<T> operator+(const PointTmpl<T> &o) const {
        return PointTmpl<T>(x + o.x, y + o.y);
    }
    PointTmpl<T> operator-(const PointTmpl<T> &o) const {
        return PointTmpl<T>(x - o.x, y - o.y);
    }
    double dist(const PointTmpl<T> &o) const {
        return sqrt(pow(x - o.x, 2) + pow(y - o.y, 2));
    }
};

template <class T> inline std::ostream &
operator<<(std::ostream &os, const PointTmpl<T> &p)
{
    return os << "Point(" << p.x << ", " << p.y << ")";
}


// This was originally Point, but OS X headers steal Point, and it's too much
// hassle to try to #define it away at every include.
typedef PointTmpl<int> IPoint;
typedef PointTmpl<double> DPoint;
typedef PointTmpl<bool> BoolPoint;


// Rect

// Rects are stored as x, y, w, h.  Actually, using this instead of
// x0, y0, x1, y1 seems like a dumb move because that way the whole question
// of negative widths wouldn't arise.  But I guess x0>x1 is the same problem.
template<class T>
struct RectTmpl {
    T x, y, w, h;
    RectTmpl(T x, T y, T w, T h) : x(x), y(y), w(w), h(h) {}
    RectTmpl() : x(), y(), w(), h() {}
    template <class U> RectTmpl(const RectTmpl<U> &o) :
        x(U(o.x)), y(U(o.y)), w(U(o.w)), h(U(o.h))
    {}

    bool operator==(const RectTmpl<T> &o) const {
        return o.x == x && o.y == y && o.w == w && o.h == h;
    }
    bool operator!=(const RectTmpl<T> &o) const { return !(*this == o); }

    T r() const { return x + w; }
    void r(T v) { x = v - w; }
    T b() const { return y + h; }
    void b(T v) { y = v - h; }
    // Overlaps upper left edge, inside lower right edge.
    bool contains(const PointTmpl<T> &p) const {
        return (x <= p.x && p.x < r() && y <= p.y && p.y < b());
    }

    // Set size to given values.
    void size(T w, T h) { this->w = w; this->h = h; }
    // Translate rect by x and y.
    void translate(const PointTmpl<T> &o) { this->x += o.x; this->y += o.y; }

    // Move this to be inside 'o'.  If 'o' is too large, overflow to +xy.
    void clamp(const RectTmpl<T> &o) {
        r(std::min(r(), o.r()));
        b(std::min(b(), o.b()));
        x = std::max(x, o.x);
        y = std::max(y, o.y);
    }

    // Return the union of this and 'o'.
    RectTmpl<T> union_(const RectTmpl<T> &o) {
        // Rects with 0 width are ignored.
        if (o.w==T() || o.h==T())
            return *this;
        else if (w==T() || h==T())
            return o;
        else {
            T x_ = std::min(this->x, o.x);
            T y_ = std::min(this->y, o.y);
            T r_ = std::max(this->r(), o.r());
            T b_ = std::max(this->b(), o.b());
            return RectTmpl<T>(x_, y_, r_ - x_, b_ - y_);
        }
        return *this; // not reached, but shut up gcc
    }

    // Return the intersection of this with 'o'.
    RectTmpl<T> intersect(const RectTmpl<T> &o) {
        T x_ = std::max(this->x, o.x);
        T y_ = std::max(this->y, o.y);
        T r_ = std::min(this->r(), o.r());
        T b_ = std::min(this->b(), o.b());
        return RectTmpl<T>(x_, y_, std::max(0, r_ - x_), std::max(0, b_ - y_));
    }

    // Return whether the two rects intersect each other at all.
    // A rect with zero width or height will never intersect with anything.
    bool intersects(const RectTmpl<T> &o) {
        if (!(w && h && o.w && o.h))
            return false;
        return !(x >= o.r() || r() <= o.x || y >= o.b() || b() <= o.y);
    }

    void normalize() {
        if (w < T()) {
            w = -w;
            x -= w;
        }
        if (h < T()) {
            h = -h;
            y -= h;
        }
    }

    PointTmpl<T> width_range() const { return PointTmpl<T>(x, x + w); }
    PointTmpl<T> height_range() const { return PointTmpl<T>(y, y + h); }
};

template <class T> inline std::ostream &
operator<<(std::ostream &os, const RectTmpl<T> &r)
{
    return os << "Rect(" << r.x << ", " << r.y << ", " << r.w << ", "
        << r.h << ")";
}


// Same story as Point.
typedef RectTmpl<int> IRect;
typedef RectTmpl<double> DRect;

}

#endif
