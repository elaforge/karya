#include <math.h>
#include "SkeletonDisplay.h"
#include <FL/Fl_Box.H>
#include <FL/fl_draw.H>

#include "util.h"
#include <iostream>


SkeletonDisplay::SkeletonDisplay(int X, int Y, int W, int H)
    : Fl_Box(X, Y, W, H)
{
    box(FL_FLAT_BOX);
}


void
SkeletonDisplay::reset()
{
    this->track_widths.clear();
    this->track_centers.clear();
    this->parent_child.clear();
}


void
SkeletonDisplay::recalculate_centers()
{
    this->track_centers.clear();
    int right = 0;
    for (size_t i = 0; i < this->track_widths.size(); i++) {
        track_centers.push_back(right + (track_widths[i] / 2));
        right += track_widths[i];
    }
    this->right_edge = right;
}


void
SkeletonDisplay::set_config(
        const SkeletonConfig &config, const std::vector<int> &widths)
{
    this->track_widths = widths;
    this->recalculate_centers();

    this->parent_child.clear();
    for (int i = 0; i < config.len; i++) {
        this->parent_child.push_back(
                std::make_pair(config.parents[i], config.children[i]));
    }
    this->redraw();
}


void
SkeletonDisplay::set_width(int tracknum, int width)
{
    // If set_config hasn't been called yet, there will be no track widths.
    if (track_widths.size()) {
        ASSERT(0 <= tracknum && (size_t) tracknum < track_widths.size());
        this->track_widths.at(tracknum) = width;
        this->recalculate_centers();
        this->redraw();
    }
}


static void
draw_arrow(int fromx, int tox, int bottom, int top)
{
    const static int offset = 5;
    // The bigger the difference between px and cx, the higher top should
    // be.  This is a kinda half-assed heuristic but it seems to look ok.
    double distance = ::normalize(20.0, 110.0, fabs(tox - fromx));
    double ratio = ::clamp(.30, 1.0, ::scale(.30, 1.0, distance));
    // printf("%d->%d: %f %f\n", fromx, tox, distance, ratio);
    top = bottom - ((bottom - top) * ratio);

    fl_color(FL_BLACK);
    fl_begin_line();
    fl_curve(fromx, bottom,
            fromx + (fromx<tox ? offset : -offset), top,
            tox + (fromx<tox ? -offset : offset), top,
            tox, bottom);
    fl_end_line();

    // TODO get arrow angle right
    fl_color(FL_RED);
    fl_begin_polygon();
    fl_vertex(tox-2, bottom-5);
    fl_vertex(tox, bottom);
    fl_vertex(tox+2, bottom-5);
    fl_end_polygon();
}


void
SkeletonDisplay::draw()
{
    Fl_Box::draw();
    int tracks = this->track_widths.size();
    for (size_t i = 0; i < this->parent_child.size(); i++) {
        int parent = parent_child[i].first;
        int child = parent_child[i].second;
        if (!(0 <= parent && parent<tracks) || !(0 <= child && child<tracks)) {
            DEBUG("parent->child out of range: " << parent << "->" << child);
            continue;
        }
        int cx = track_centers[child] + this->x();
        int px = track_centers[parent] + this->x();
        int top = this->y();
        int bottom = this->y() + this->h();

        // printf("draw %d->%d: (%d->%d) (%d, %d)\n", child, parent,
        //         cx, px, bottom, top);
        draw_arrow(cx, px, bottom-1, top);
    }
}
