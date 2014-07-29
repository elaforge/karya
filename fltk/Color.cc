// Copyright 2014 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include "Color.h"


const Color Color::black = Color(0, 0, 0, 0);
const Color Color::white = Color(255, 255, 255, 0);

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
