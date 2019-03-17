// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

#include <execinfo.h>
#include <iostream>
#include <set>
#include <string.h>
#include <string>
#include <utility>
#include <vector>


#define DEBUG(X) do { std::cout << __FILE__ << ':' << __LINE__ << ' ' \
    << X << std::endl; } while (0)

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


enum { _backtrace_size = 10 };

struct AssertionError : std::exception {
    AssertionError(const char *expr, const char *file, const char *func,
            int line, std::string msg) :
        expr(expr), file(file), func(func), line(line), msg(msg)
    {
        backtrace(trace, _backtrace_size);
    }
    const char *expr, *file, *func;
    const int line;
    std::string msg;
    void *trace[_backtrace_size];
};

inline std::ostream &
operator<<(std::ostream &os, const AssertionError &a)
{
    os << "assertion failed at " << a.file << ':' << a.line << ' '
        << a.func << "(): '" << a.expr << "'";
    if (a.msg.length() > 0)
        os << ": " << a.msg;
    os << '\n';
    char **symbols = backtrace_symbols(a.trace, _backtrace_size);
    for (int i = 0; i < _backtrace_size && symbols[i]; i++) {
        os << symbols[i] << '\n';
    }
    free(symbols);

    return os;
}


// Numeric /////////////////////////////

namespace util {

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

}

// The stdlib lacks this instance.
template <class T, class U> inline std::ostream &
operator<<(std::ostream &os, const std::pair<T, U> &p)
{
    return os << "(" << p.first << ", " << p.second << ")";
}

template <class T> inline std::ostream &
operator<<(std::ostream &os, const std::set<T> &set)
{
    os << "{";
    bool first = true;
    for (const T &val : set) {
        if (!first) {
            os << ", ";
            first = false;
        }
        os << val;
    }
    return os << "}";
}
