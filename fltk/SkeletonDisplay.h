// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#ifndef __SKELETON_DISPLAY_H
#define __SKELETON_DISPLAY_H

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

struct SkeletonStatus {
    SkeletonStatus(Color color, utf8::rune status1, utf8::rune status2) :
        color(color), status1(status1), status2(status2)
    {}
    Color color;
    // I'd really like to have a string with a variable number of characters,
    // but then I'd have to do malloc/free and all that C pain.  There are
    // probably ways to treat a short string as a value, e.g. by declaring a
    // struct or stuffing it into an int, but they're either not much better
    // than declaring a bunch of chars, or are kind of grody.
    utf8::rune status1, status2;
};

// This is a list of pairs linking parent tracknums to child tracknums.
// "Parent" and "child" have no meaning at the UI level, but it uses them to
// show the relationship visually.  Out of range tracknums will be warned about
// and ignored.
struct SkeletonConfig {
    SkeletonConfig(int edges_len, SkeletonEdge *edges) :
        edges_len(edges_len), edges(edges), statuses_len(0),
        statuses(0)
    {}
    int edges_len;
    SkeletonEdge *edges;
    int statuses_len;
    SkeletonStatus *statuses;
};

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
    // This does not reject configs with tracks out of range because fltk
    // should do what haskell says or it gets out of sync.  But draw() will
    // complain about them.
    // Since this copies the config, it doesn't need to live beyond this call.
    void set_config(
        const SkeletonConfig &config, const std::vector<int> &widths);
    void set_title(const char *title);
    void set_status(int tracknum, utf8::rune status1, utf8::rune status2,
        Color color);
    void set_width(int tracknum, int width);

protected:
    void draw() override;

private:
    struct Track {
        Track(int width, int height) : width(width), left(0),
            center(0), height(height), status1(0), status2(0)
        {}
        int width; // Width of this track.
        // Left edge of the track.  It's technically redundant, since it should
        // always be center - width/2, but it's simpler to cache it.
        int left;
        int center;
        int height; // Number of children, as defined by 'track_height'.
        // '\0' means don't draw a status and ignore the color.  ' ' means draw
        // the color, but with no text, of course.
        utf8::rune status1, status2;
        Color color;
    };

    void recalculate_centers();
    std::string title;
    std::vector<Track> tracks;
    std::vector<SkeletonEdge> edges;
};

#endif
