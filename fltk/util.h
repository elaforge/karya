#ifndef __UTIL_H
#define __UTIL_H

#include <string.h>
#include <utility>
#include "geom_util.h"

using namespace geom_util;


namespace utf8 {

const char *backward(const char *str, const char *start);
const char *forward(const char *str, const char *end);
int width(const char *str);
// The byte index of 'chars' char index into a utf8 string, of the given length.
// Returns 'len' if the index is past the end.
int bytes(const char *str, int len, int chars);

}


// Assert //////////////////////////////

// assertions
// I use exceptions so c++ errors won't necessarily crash the whole program.

#define ASSERT(x) if (!(x)) do { \
    AssertionError a(#x, __FILE__, __func__, __LINE__, ""); \
    std::cerr << "assertion: " << a << '\n'; \
    throw a; \
} while (0)

#define ASSERT_MSG(x, msg) if (!(x)) do { \
    AssertionError a(#x, __FILE__, __func__, __LINE__, msg); \
    std::cerr << "assertion: " << a << '\n'; \
    throw a; \
} while (0)


struct AssertionError : std::exception {
    AssertionError(const char *expr, const char *file, const char *func,
            int line, const char *msg) :
        expr(expr), file(file), func(func), line(line), msg(msg)
    {}
    const char *expr, *file, *func;
    const int line;
    const char *msg;
};

inline std::ostream &
operator<<(std::ostream &os, const AssertionError &a)
{
    os << "<assertion failed at " << a.file << ':' << a.line << ' '
        << a.func << "(): '" << a.expr;
    if (strlen(a.msg))
        os << "(" << a.msg << ")";
    return os << "'>";
}


// Numeric /////////////////////////////

// Restrict 'v' to be in the given range, like composed min and max.
// If 'max' is less than 'min', the result will be 'min'.
template<class T> inline T
clamp(T min, T max, T v)
{
    return std::max(min, std::min(max, v));
}

// Normalize 'v', which is between 'min' and 'max' inclusive, to be between
// 0--1.
template<class T> inline T
normalize(T min, T max, T v)
{
    return (v-min) / (max-min);
}

// Scale 'v', which is between 0--1 inclusive, to be between 'min' and 'max'.
template<class T> inline T
scale(T min, T max, T v)
{
    return v * (max-min) + min;
}


#define DEBUG(X) do { std::cout << __FILE__ << ':' << __LINE__ << ' ' \
    << X << '\n'; std::cout.flush(); } while (0)

// vector //////////////////////////////

template <class T> inline std::ostream &
operator<<(std::ostream &os, const std::vector<T> &a)
{
    os << '[';
    for (size_t i = 0; i < a.size(); i++) {
        if (i)
            os << ", ";
        os << a[i];
    }
    os << ']';
    return os;
}

template <class T> inline void
vector_put(std::vector<T> &a, int i, T v)
{
    while (static_cast<size_t>(i) >= a.size())
        a.push_back(T());
    a[i] = v;
}

template <class T> inline T
vector_get(std::vector<T> &a, int i, T def = T())
{
    if (static_cast<size_t>(i) >= a.size())
        return def;
    return a[i];
}

template <class T> inline void
vector_erase(std::vector<T> &a, int i)
{
    if (static_cast<size_t>(i) < a.size())
        a.erase(a.begin() + i);
}




template <class T, class U> inline std::ostream &
operator<<(std::ostream &os, const std::pair<T, U> &p)
{
    return os << "(" << p.first << ", " << p.second << ")";
}


// Color ///////////////////////////////

struct Color {
    Color(unsigned char r, unsigned char g, unsigned char b,
            unsigned char a=0xff)
        : r(r), g(g), b(b), a(a) {}
    explicit Color() : r(0), g(0), b(0), a(0) {}
    static Color from_doubles(double r, double g, double b, double a) {
        return Color(clamp(0.0, 255.0, r), clamp(0.0, 255.0, g),
            clamp(0.0, 255.0, b), clamp(0.0, 255.0, a));
    }
    static Color rgb_normalized(double r, double g, double b) {
        return from_doubles(r * 255, g * 255, b * 255, 255);
    }
    static Color from_rgb_word(unsigned int rgb) {
        return Color(0xff & (rgb >> 16), 0xff & (rgb >> 8), 0xff & rgb, 0xff);
    }
    static Color from_rgba_word(unsigned int rgba) {
        return Color(0xff & (rgba >> 24), 0xff & (rgba >> 16),
            0xff & (rgba >> 8), 0xff & rgba);
    }
    bool operator==(const Color &o) const {
        return r==o.r && g==o.g && b==o.b && a==o.a;
    }
    bool operator!=(const Color &o) const { return !(*this == o); }

    unsigned char r, g, b, a;

    Color brightness(double d) const {
        if (d < 1) {
            return Color::from_doubles(
                    scale(0.0, double(r), d), scale(0.0, double(g), d),
                    scale(0.0, double(b), d), a);
        } else {
            return Color::from_doubles(
                    scale(double(r), 255.0, d-1), scale(double(g), 255.0, d-1),
                    scale(double(b), 255.0, d-1), a);
        }
    }

    static const Color black, white;
};

inline std::ostream &
operator<<(std::ostream &os, const Color &c)
{
    return os << "Color(" << (int) c.r << ", " << (int) c.g << ", "
        << (int) c.b << ", " << (int) c.a << ")";
}

#endif
