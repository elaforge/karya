#ifndef __UTIL_H
#define __UTIL_H

#include <geom_util.h>

using namespace geom_util;


struct Color {
    Color(unsigned char r, unsigned char g, unsigned char b,
            unsigned char a=0xff)
        : r(r), g(g), b(b), a(a) {}
    Color(unsigned long rgb) :
        r(0xff & (rgb >> 16)), g(0xff & (rgb >> 8)), b(0xff & rgb), a(0xff) {}

    Color() : r(0), g(0), b(0), a(0) {}
    unsigned char r, g, b, a;
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


// assertions
// I use exceptions so c++ errors won't necessarily crash the whole program.

#define ASSERT(x) if (!(x)) do { \
    AssertionError a(#x, __FILE__, __func__, __LINE__); \
    std::cerr << "assertion: " << a << '\n'; \
    throw a; \
} while (0)


struct AssertionError : std::exception {
    AssertionError(const char *expr, const char *file, const char *func,
            int line) :
        expr(expr), file(file), func(func), line(line)
    {}
    const char *expr, *file, *func;
    const int line;
};

inline std::ostream &
operator<<(std::ostream &os, const AssertionError &a)
{
    return os << "<assertion failed at " << a.file << ':' << a.line << ' '
        << a.func << "(): '" << a.expr << "'>";
}


// Supposedly these are in <algorithm>, but when I include that, they're not.
template<class T> inline T
max(T a, T b)
{
    return a > b ? a : b;
}

template<class T> inline T
min(T a, T b)
{
    return a < b ? a : b;
}


#define DEBUG(X) do { std::cout << __FILE__ << ':' << __LINE__ << ' ' \
    << X << '\n'; } while (0)


template <class T> inline std::ostream &
operator<<(std::ostream &os, const std::vector<T> &a)
{
    os << '[';
    for (int i = 0; i < a.size(); i++) {
        if (i)
            os << ", ";
        os << a[i];
    }
    os << ']';
    return os;
}

#endif
