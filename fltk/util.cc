#include <FL/fl_utf8.h>
#include <FL/fl_draw.H>
#include "util.h"

namespace utf8 {

const char *
backward(const char *str, const char *start)
{
    if (str == start)
        return str;
    else
        return fl_utf8back(str-1, start, str);
}

const char *
forward(const char *str, const char *end)
{
    if (str + 1 >= end)
        return end;
    else
        return fl_utf8fwd(str+1, str, end);
}

int
width(const char *str)
{
    return fl_width(str, fl_utf8len(*str));
}

int
bytes(const char *str, int len, int chars)
{
    const char *end = str + len;
    const char *p = str;
    for (int i = 0; i < chars; i++)
        p = forward(p, end);
    return p - str;
}

}

const Color Color::black = Color(0, 0, 0, 0);
const Color Color::white = Color(255, 255, 255, 0);
