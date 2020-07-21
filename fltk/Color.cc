// Copyright 2014 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include "Color.h"


const Color Color::black = Color(0, 0, 0);
const Color Color::white = Color(255, 255, 255);

// 0-1 scales to black, 1-2 scales to white.
Color
Color::brightness(double d) const
{
    if (d < 1) {
        return Color::from_doubles(
            util::scale(0.0, double(r), d), util::scale(0.0, double(g), d),
            util::scale(0.0, double(b), d), a);
    } else {
        return Color::from_doubles(
            util::scale(double(r), 255.0, d-1),
            util::scale(double(g), 255.0, d-1),
            util::scale(double(b), 255.0, d-1), a);
    }
}

Color
Color::crossfade(Color c, double d) const
{
    if (d <= 0) {
        return *this;
    } else if (d >= 1) {
        return c;
    } else {
        return Color::from_doubles(
            util::scale(double(r), double(c.r), d),
            util::scale(double(g), double(c.g), d),
            util::scale(double(b), double(c.b), d), a);
    }
}

Color
ColorCycle::get()
{
    static std::vector<Color> colors;
    if (colors.empty()) {
        colors.push_back(Color(0xff, 0, 0));
        colors.push_back(Color(0, 0xff, 0));
        colors.push_back(Color(0xff, 0xff, 0));
        colors.push_back(Color(0, 0, 0xff));
        colors.push_back(Color(0xff, 0, 0xff));
        colors.push_back(Color(0, 0xff, 0xff));
    }
    return colors[this->index_++ % colors.size()];
}
