// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

#include <FL/Fl_Box.H>
#include <vector>
#include <utility>

#include "Color.h"
#include "utf8.h"


struct SkeletonEdge {
    SkeletonEdge(int parent, int child, int width, Color color)
        : parent(parent), child(child), width(width), color(color) {}
    int parent;
    int child;
    int width;
    Color color;
};

inline std::ostream &
operator<<(std::ostream &os, const SkeletonEdge &e)
{
    return os << "SkeletonEdge(" << e.parent << ", " << e.child << ", "
        << e.width << ", " << e.color << ")";
};

// This is a list of pairs linking parent tracknums to child tracknums.
// "Parent" and "child" have no meaning at the UI level, but it uses them to
// show the relationship visually.  Out of range tracknums will be warned about
// and ignored.
struct SkeletonConfig {
    SkeletonConfig(int edges_len, SkeletonEdge *edges)
        : edges_len(edges_len), edges(edges)
    {}
    int edges_len;
    SkeletonEdge *edges;
};

struct SkeletonStatus {
    SkeletonStatus() : color(), c1(0), c2(0) {}
    SkeletonStatus(Color color, utf8::rune c1, utf8::rune c2) :
        color(color), c1(c1), c2(c2)
    {}
    bool operator==(const SkeletonStatus &that) const {
        return color == that.color && c1 == that.c1 && c2 == that.c2;
    }
    bool operator!=(const SkeletonStatus &that) const {
        return !(*this == that);
    }
    Color color;
    // I'd really like to have a string with a variable number of characters,
    // but then I'd have to do malloc/free and all that C pain.  There are
    // probably ways to treat a short string as a value, e.g. by declaring a
    // struct or stuffing it into an int, but they're either not much better
    // than declaring a bunch of chars, or are kind of grody.
    // '\0' means don't draw a status and ignore the color.  ' ' means draw
    // the color, but with no text, of course.
    utf8::rune c1, c2;
};

inline std::ostream &
operator<<(std::ostream &os, const SkeletonStatus &s)
{
    return os << "SkeletonStatus(" << s.color << ", " << s.c1 << ", " << s.c2
        << ")";
}

// Display the arrows for the skeleton tree.
//
// Since the arrows line up with the tracks below them, this must be kept
// in sync with the state of the tracks.  However, I also want the skeleton
// to remain the same until told otherwise, so I don't modify it when new
// tracks are added or removed, and it's not fatal for the config to point to
// out of range tracknums.
class SkeletonDisplay : public Fl_Box {
public:
    SkeletonDisplay(int x, int y, int w, int h);
    void print_debug() const;
    void update(
        const SkeletonConfig &config, const std::vector<int> &widths,
        const std::vector<SkeletonStatus> &status);
    // This does not reject configs with tracks out of range because fltk
    // should do what haskell says or it gets out of sync.  But draw() will
    // complain about them.
    // Since this copies the config, it doesn't need to live beyond this call.
    // void set_config(
    //     const SkeletonConfig &config, const std::vector<int> &widths);
    void set_config(const SkeletonConfig &config);
    void set_title(const char *title);
    void set_status(int tracknum, SkeletonStatus status);
    void set_width(int tracknum, int width);

    void insert_track(int tracknum);
    void remove_track(int tracknum);

protected:
    void draw() override;

private:
    struct Track {
        Track() : width(0), left(0), center(0), height(0), status() {}
        Track(int width, int height, SkeletonStatus status)
            : width(width), left(0), center(0), height(height), status(status)
        {}
        int width; // Width of this track.
        // Left edge of the track.  It's technically redundant, since it should
        // always be center - width/2, but it's simpler to cache it.
        int left;
        int center;
        int height; // Number of children, as defined by 'track_height'.
        SkeletonStatus status;
    };

    void recalculate_centers();
    std::string title;
    std::vector<Track> tracks;
    std::vector<SkeletonEdge> edges;
};
