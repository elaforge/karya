// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <FL/fl_utf8.h>
#include <FL/fl_draw.H>

#include "utf8.h"

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

const char *
encode(rune c)
{
    static char buf[7];
    // I want a 0 rune to become an empty string.  That way if I append zero to
    // nonzero I don't get a leading null.
    int len = c == 0 ? 0 : fl_utf8encode(c, buf);
    buf[len] = '\0';
    return buf;
}

}
