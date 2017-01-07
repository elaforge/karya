// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <math.h>
#include <FL/Fl_Box.H>
#include <FL/fl_draw.H>

#include "config.h"
#include "f_util.h"
#include "utf8.h"
#include "util.h"

#include "SkeletonDisplay.h"


SkeletonDisplay::SkeletonDisplay(int x, int y, int w, int h) :
    Fl_Box(x, y, w, h)
{
    box(FL_FLAT_BOX);
}


void
SkeletonDisplay::recalculate_centers()
{
    int right = 0;
    for (size_t i = 0; i < tracks.size(); i++) {
        int width = tracks[i].width;
        tracks[i].center = right + (width/2);
        tracks[i].left = right;
        right += width;
    }
}


static void
children_of(const std::vector<SkeletonEdge> &edges, int tracknum,
    std::vector<int> *children)
{
    for (size_t i = 0; i < edges.size(); i++) {
        if (tracknum == edges[i].parent)
            children->push_back(edges[i].child);
    }
}

// Get the height of a track, which is the maximum depth of its children.
// Only a track of >1 child counts toward the depth, so linear sequences don't
// contribute to the height.
static int
track_height(const std::vector<SkeletonEdge> &edges, int tracknum)
{
    std::vector<int> children;
    children_of(edges, tracknum, &children);
    if (children.size() == 0)
        return 0;
    int height = 0;
    for (size_t i = 0; i < children.size(); i++) {
        // Avoid endless recursion.
        if (tracknum == children[i])
            DEBUG("track has itself as a child: " << tracknum);
        else
            height = std::max(height, track_height(edges, children[i]));
    }
    if (children.size() > 1)
        height++;
    return height;
}


void
SkeletonDisplay::set_config(
    const SkeletonConfig &config, const std::vector<int> &widths)
{
    edges.assign(config.edges, config.edges + config.edges_len);
    std::vector<Track> new_tracks;
    new_tracks.reserve(widths.size());
    for (size_t i = 0; i < widths.size(); i++) {
        new_tracks.push_back(Track(
            widths[i],
            track_height(edges, i),
            i < tracks.size() ? tracks[i].status : SkeletonStatus()));
    }
    this->tracks = new_tracks;
    this->recalculate_centers();
    this->redraw();
}


void
SkeletonDisplay::set_title(const char *title)
{
    this->title = title;
    this->redraw();
}


void
SkeletonDisplay::set_status(int tracknum, SkeletonStatus status)
{
    ASSERT(0 <= tracknum);
    if (static_cast<size_t>(tracknum) < tracks.size()) {
        tracks[tracknum].status = status;
    }
    this->redraw();
}


void
SkeletonDisplay::set_width(int tracknum, int width)
{
    ASSERT(tracknum >= 0);
    // If a track has been added and the skeleton not yet updated, tracknum
    // could be out of range.
    if (static_cast<size_t>(tracknum) < tracks.size()) {
        ASSERT(0 <= tracknum && (size_t) tracknum < tracks.size());
        if (tracks[tracknum].width != width) {
            tracks[tracknum].width = width;
            this->recalculate_centers();
            this->redraw();
        }
    }
}


void
SkeletonDisplay::insert_track(int tracknum)
{
    tracks.insert(tracks.begin() + tracknum, Track());
    this->redraw();
}


void
SkeletonDisplay::remove_track(int tracknum)
{
    tracks.erase(tracks.begin() + tracknum);
    this->redraw();
}


static void
draw_arrow(IPoint from, IPoint to, int width, Color color, int bottom, int top)
{
    const static int offset = 5;
    // The bigger the difference between px and cx, the higher top should
    // be.  This is a kinda half-assed heuristic but it seems to look ok.
    double distance = util::normalize(20.0, 110.0, fabs(double(to.x - from.x)));
    double ratio = util::clamp(.30, 1.0, util::scale(.30, 1.0, distance));
    // DEBUG(from << " -> " << to << ": dist " << distance << " rat " << ratio);
    top = bottom - ((bottom - top) * ratio);

    fl_color(color.fl());
    fl_line_style(FL_SOLID, width, 0);
    fl_begin_line();
    fl_curve(from.x, bottom - from.y,
        from.x + (from.x<to.x ? offset : -offset), top,
        to.x + (from.x<to.x ? -offset : offset), top,
        to.x, bottom - to.y);
    fl_end_line();

    // TODO get arrow angle right
    fl_begin_polygon();
    fl_vertex(to.x-2, bottom-5 - to.y);
    fl_vertex(to.x, bottom - to.y);
    fl_vertex(to.x+2, bottom-5 - to.y);
    fl_end_polygon();
}


// The arrow source will be raised depending on how levels it's at, where
// a parent with only one child doesn't count as a level.
void
SkeletonDisplay::draw()
{
    Fl_Box::draw();
    const int ntracks = this->tracks.size();
    const int top = y();
    const int bottom = y() + h();

    int max_height = 0;
    for (int i = 0; i < ntracks; i++)
        max_height = std::max(max_height, tracks[i].height);
    // Keep half of the display free for the arrow arcs, and divide the lower
    // half among the steps.
    const int height_step = h() / 2 / (max_height+1);

    // Draw status colors.
    for (int i = 0; i < ntracks; i++) {
        if (tracks[i].status.c1) {
            fl_color(tracks[i].status.color.fl());
            fl_rectf(x() + tracks[i].left, y(), tracks[i].width, h());
        }
        // Draw the step rectangle so it looks like the arrows are sitting on
        // something.
        if (tracks[i].height) {
            int height = tracks[i].height * height_step;
            if (tracks[i].status.c1)
                fl_color(tracks[i].status.color.brightness(0.8).fl());
            else
                fl_color(Config::skeleton_display_bg.brightness(0.8).fl());
            fl_rectf(x() + tracks[i].left, y() + h() - height,
                tracks[i].width, height);
        }
    }

    // Initially I thought to center the text, but it turns out the upper left
    // corner is the least likely to collide with skeleton arcs.
    if (title.size() > 0) {
        fl_font(Config::font, Config::font_size::skeleton_title);
        fl_color(FL_BLACK);
        fl_draw(title.c_str(), title.size(),
            x() + 3, top + fl_height() - fl_descent());
    }

    for (size_t i = 0; i < this->edges.size(); i++) {
        const SkeletonEdge &e = edges[i];
        if (!(0 <= e.parent && e.parent < ntracks)
            || !(0 <= e.child && e.child < ntracks))
        {
            // +1 because the ruler track has been subtracted.
            DEBUG("parent->child out of range: " << e.parent + 1 << "->"
                << e.child + 1);
            continue;
        }
        // Offset the ends of the arrow to make it clearer which is coming and
        // which is going.  But if the arc is too narrow it looks ugly, so
        // put it in the center in that case.
        int coffset = tracks[e.child].width < 15
            ? 0 : tracks[e.child].width / 8;
        int poffset = tracks[e.parent].width < 15
            ? 0 : tracks[e.parent].width / 8;
        if (abs(tracks[e.child].center - tracks[e.parent].center) < 15) {
            coffset = poffset = 0;
        }
        int cx = tracks[e.child].center - coffset + this->x();
        int px = tracks[e.parent].center + poffset + this->x();
        int ch = tracks[e.child].height * height_step;
        int ph = tracks[e.parent].height * height_step;

        // printf("draw %d->%d: (%d->%d) (%d, %d)\n", e.child, e.parent,
        //         cx, px, bottom, top);
        draw_arrow(IPoint(px, ph), IPoint(cx, ch),
            e.width, e.color, bottom-1, top);
    }

    // Draw status letters.
    fl_font(Config::font + FL_BOLD, Config::font_size::track_status);
    for (int i = 0; i < ntracks; i++) {
        std::string str(utf8::encode(tracks[i].status.c1));
        str.append(utf8::encode(tracks[i].status.c2));
        if (str.size()) {
            // DEBUG("draw " << i << " " << tracks[i].color);
            int cw = fl_width(str.c_str(), 2);
            int xpos = this->x() + tracks[i].center - cw/2;
            fl_color(tracks[i].status.color.fl());
            fl_rectf(xpos-1, bottom - fl_height(), cw + 2, fl_height());
            fl_color(FL_BLACK);
            fl_draw(str.c_str(), 2, xpos, bottom - fl_descent());
        }
    }
}
