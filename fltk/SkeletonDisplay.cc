#include <math.h>
#include "SkeletonDisplay.h"
#include <FL/Fl_Box.H>
#include <FL/fl_draw.H>

#include "config.h"
#include "util.h"
#include "f_util.h"
#include <iostream>


SkeletonDisplay::SkeletonDisplay(int X, int Y, int W, int H)
    : Fl_Box(X, Y, W, H), right_edge(X+W)
{
    box(FL_FLAT_BOX);
    // box(FL_THIN_DOWN_BOX);
}


void
SkeletonDisplay::resize(int x, int y, int w, int h)
{
    w = std::max(this->right_edge, w);
    Fl_Box::resize(x, y, w, h);
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
    edges.assign(config.edges, config.edges + config.len);
    this->redraw();
}


void
SkeletonDisplay::set_status(int tracknum, char status, Color color)
{
    while (this->status_color.size() <= static_cast<size_t>(tracknum)) {
        this->status_color.push_back(std::make_pair('\0', Color()));
    }
    this->status_color[tracknum] = std::make_pair(status, color);
    this->redraw();
}


void
SkeletonDisplay::get_status(int tracknum, char *status, Color *color)
{
    ASSERT(tracknum >= 0);
    std::pair<char, Color> v = vector_get(
        status_color, tracknum, std::make_pair(' ', Color()));
    *status = v.first;
    *color = v.second;
}


void
SkeletonDisplay::set_width(int tracknum, int width)
{
    ASSERT(tracknum >= 0);
    // If a track has been added and the skeleton not yet updated, tracknum
    // could be out of range.
    if (static_cast<size_t>(tracknum) < track_widths.size()) {
        ASSERT(0 <= tracknum && (size_t) tracknum < track_widths.size());
        this->track_widths.at(tracknum) = width;
        this->recalculate_centers();
        this->redraw();
    }
}


static void
draw_arrow(int fromx, int tox, int width, Color color, int bottom, int top)
{
    const static int offset = 5;
    // The bigger the difference between px and cx, the higher top should
    // be.  This is a kinda half-assed heuristic but it seems to look ok.
    double distance = ::normalize(20.0, 110.0, fabs(tox - fromx));
    double ratio = ::clamp(.30, 1.0, ::scale(.30, 1.0, distance));
    // printf("%d->%d: %f %f\n", fromx, tox, distance, ratio);
    top = bottom - ((bottom - top) * ratio);

    fl_color(color_to_fl(color));
    fl_line_style(FL_SOLID, width, 0);
    fl_begin_line();
    fl_curve(fromx, bottom,
        fromx + (fromx<tox ? offset : -offset), top,
        tox + (fromx<tox ? -offset : offset), top,
        tox, bottom);
    fl_end_line();

    // TODO get arrow angle right
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
    int top = this->y();
    int bottom = this->y() + this->h();
    size_t status_size = std::min(status_color.size(), track_centers.size());
    for (size_t i = 0; i < status_size; i++) {
        char c = status_color[i].first;
        if (c) {
            fl_color(color_to_fl(status_color[i].second));
            int w = track_widths[i];
            fl_rectf(this->x()+track_centers[i] - w/2, this->y(), w, this->h());
        }
    }
    for (size_t i = 0; i < this->edges.size(); i++) {
        const SkeletonEdge &e = edges[i];
        if (!(0 <= e.parent && e.parent < tracks)
            || !(0 <= e.child && e.child < tracks))
        {
            // +1 because the ruler track has been subtracted.
            DEBUG("parent->child out of range: " << e.parent + 1 << "->"
                << e.child + 1);
            continue;
        }
        // Offset the ends of the arrow to make it clearer which is coming and
        // which is going.  But if the arc is too narrow it looks ugly, so
        // put it in the center in that case.
        int coffset = track_widths[e.child] < 15
            ? 0 : track_widths[e.child] / 8;
        int poffset = track_widths[e.parent] < 15
            ? 0 : track_widths[e.parent] / 8;
        if (abs(track_centers[e.child] - track_centers[e.parent]) < 15) {
            coffset = poffset = 0;
        }
        int cx = track_centers[e.child] - coffset + this->x();
        int px = track_centers[e.parent] + poffset + this->x();

        // printf("draw %d->%d: (%d->%d) (%d, %d)\n", e.child, e.parent,
        //         cx, px, bottom, top);
        draw_arrow(px, cx, e.width, e.color, bottom-1, top);
    }
    fl_font(Config::font + FL_BOLD, Config::font_size::track_status);
    for (size_t i = 0; i < status_size; i++) {
        char c = status_color[i].first;
        // DEBUG("center " << i << " " << track_centers[i]);
        if (c) {
            // DEBUG("draw " << i << " " << this->status_color[i]);
            int cw = fl_width(c);
            int ch = fl_height() + fl_descent();
            int xpos = this->x() + track_centers[i] - cw/2;
            fl_color(color_to_fl(status_color[i].second));
            fl_rectf(xpos-1, bottom - ch, cw + 2, ch);
            fl_color(FL_BLACK);
            fl_draw(&c, 1, xpos, bottom - fl_descent());
        }
    }
}
