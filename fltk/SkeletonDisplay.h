#ifndef __SKELETON_DISPLAY_H
#define __SKELETON_DISPLAY_H

#include <FL/Fl_Box.H>
#include <vector>
#include <utility>

#include "util.h"


// This is a list of pairs linking parent tracknums to child tracknums.
// "Parent" and "child" have no meaning at the UI level, but it uses them to
// show the relationship visually.  Out of range tracknums will be warned about
// and ignored.
struct SkeletonConfig {
    int len;
    int *parents;
    int *children;
};

class SkeletonDisplay : public Fl_Box {
public:
    SkeletonDisplay(int X, int Y, int W, int H);
    void set_config(
            const SkeletonConfig &config, const std::vector<int> &widths);
    void set_status(int tracknum, char status, Color color);
    void get_status(int tracknum, char *status, Color *color);
    void set_width(int tracknum, int width);

    void resize(int x, int y, int w, int h) {
        w = std::max(right_edge, w);
        Fl_Box::resize(x, y, w, h);
    }

protected:
    void draw();

private:
    void recalculate_centers();
    std::vector<int> track_widths;
    std::vector<int> track_centers;
    std::vector<std::pair<int, int> > parent_child;
    std::vector<std::pair<char, Color> > status_color;
    int right_edge;
};

#endif
