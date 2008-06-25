// generic utilities

#ifndef __GEOM_UTIL_H
#define __GEOM_UTIL_H

#include <iostream>
#include <stdexcept>
#include <vector>
#include <utility>

// so code has access to debugging printf
#include <stdio.h>

namespace geom_util {

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
    double dist(const PointTmpl<T> &o) const {
        return sqrt(pow(x - o.x, 2) + pow(y - o.y, 2));
    }
};

template <class T> inline std::ostream &
operator<<(std::ostream &os, const PointTmpl<T> &p)
{
    return os << "Point(" << p.x << ", " << p.y << ")";
}


typedef PointTmpl<int> Point;
typedef PointTmpl<double> DPoint;
typedef PointTmpl<bool> BoolPoint;


// Rect

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
    // TODO document this
    bool intersects(const PointTmpl<T> &p) const {
        return ! (p.r() < x || p.b() < y || p.x >= r() || p.y >= b());
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

    // Expand to contain 'o'.
    void union_(const RectTmpl<T> &o) {
        if (w == T() || o.w == T()) { // empty dimensions have no effect
            w = std::max(x, o.w);
            x = std::max(x, o.x);
        } else {
            w = std::max(r(), o.r()) - std::min(x, o.x);
            x = std::min(x, o.x);
        }
        if (h == T() || o.h == T()) {
            h = std::max(h, o.h);
            y = std::max(y, o.y);
        } else {
            h = std::max(b(), o.b()) - std::min(y, o.y);
            y = std::min(y, o.y);
        }
    }

    // clip this to lie within o
    void clip(const RectTmpl<T> &o) {
        if (x < o.x) {
            w -= o.x - x;
            x = o.x;
        }
        if (y < o.y) {
            h -= o.y - y;
            y = o.y;
        }
        if (r() > o.r())
            w = o.r() - x;
        if (b() > o.b())
            h = o.b() - y;
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
};

template <class T> inline std::ostream &
operator<<(std::ostream &os, const RectTmpl<T> &r)
{
    return os << "Rect(" << r.x << ", " << r.y << ", " << r.w << ", "
        << r.h << ")";
}


typedef RectTmpl<int> Rect;
typedef RectTmpl<double> DRect;

}

#endif
