// Copyright 2014 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

#include <FL/Fl.H>
#include "util.h"


struct Color {
    Color(unsigned char r, unsigned char g, unsigned char b,
            unsigned char a=0xff)
        : r(r), g(g), b(b), a(a) {}
    explicit Color() : r(0), g(0), b(0), a(0) {}
    static Color from_doubles(double r, double g, double b, double a) {
        return Color(util::clamp(0.0, 255.0, r), util::clamp(0.0, 255.0, g),
            util::clamp(0.0, 255.0, b), util::clamp(0.0, 255.0, a));
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

    // Convert to Fl_Color.
    Fl_Color fl() const { return fl_rgb_color(r, g, b); }

    unsigned char r, g, b, a;

    // 0-1 scales to black, 1-2 scales to white.
    Color brightness(double d) const;
    // 0 is entirely this color, 1 is entirely the other color.
    Color crossfade(Color c, double d) const;
    static const Color black, white;
};

inline std::ostream &
operator<<(std::ostream &os, const Color &c)
{
    return os << "Color(" << (int) c.r << ", " << (int) c.g << ", "
        << (int) c.b << ", " << (int) c.a << ")";
}

// Cycle colors. This is useful for drawing debugging rectangles.
class ColorCycle {
public:
    ColorCycle() : index_(0) {}
    int index() const { return index_; }
    Color get();
private:
    int index_;
};
