#ifndef __UTIL_H
#define __UTIL_H

#include <geom_util.h>

using namespace geom_util;


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
// If 'v' is less than 'min' *and* greater than 'max', it will be 'min'.
template<class T> inline T
clamp(T min, T max, T v)
{
    if (v < min) return min;
    else if (v > max) return max;
    else return v;
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
    << X << '\n'; } while (0)


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


// Color ///////////////////////////////

struct Color {
    Color(unsigned char r, unsigned char g, unsigned char b,
            unsigned char a=0xff)
        : r(r), g(g), b(b), a(a) {}
    Color(unsigned long rgb) :
        r(0xff & (rgb >> 16)), g(0xff & (rgb >> 8)), b(0xff & rgb), a(0xff) {}
    bool operator==(const Color &o) const {
        return r==o.r && g==o.g && b==o.b && a==o.a;
    }
    bool operator!=(const Color &o) const { return !(*this == o); }

    Color() : r(0), g(0), b(0), a(0) {}
    unsigned char r, g, b, a;

    Color scale(double d) const {
        return Color(clamp(0.0, 255.0, r * d), clamp(0.0, 255.0, g * d),
                clamp(0.0, 255.0, b * d), a);
    }
};

inline std::ostream &
operator<<(std::ostream &os, const Color &c)
{
    return os << "Color(" << (int) c.r << ", " << (int) c.g << ", "
        << (int) c.b << ", " << (int) c.a << ")";
}

// Color(unsigned long rgba) :
//     r(0xff & (rgba >> 24)), g(0xff & (rgba >> 16)), b(0xff & (rgba >> 8)),
//         a(0xff & rgba) {}

#endif
