// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <math.h>
#include <utility>

#include "f_util.h"
#include "util.h"

#include "RulerTrack.h"


// Hack for debugging.
#define SHOW_RANGE(r) (r).y << "--" << (r).b()

// RulerTrack

RulerTrack::RulerTrack(const RulerConfig &config) :
    Track("ruler"),
    title_box(0),
    bg_box(0, 0, 1, 1),
    ruler_overlay(config, true)
{
    this->add(bg_box);
    end();

    bg_box.box(FL_THIN_DOWN_BOX);
    bg_box.color(config.bg.fl());
}


// Don't create the title widget until it is actually requested.  This avoids
// creating a title box for Rulers that don't have one.
Fl_Box &
RulerTrack::title_widget()
{
    if (!this->title_box) {
        this->title_box = new Fl_Box(0, 0, 1, 1);
        title_box->box(FL_FLAT_BOX);
        title_box->color(this->ruler_overlay.config.bg.fl());
    }
    return *this->title_box;
}


void
RulerTrack::update(const Tracklike &track, ScoreTime start, ScoreTime end)
{
    ASSERT_MSG(track.ruler && !track.track,
        "updated a ruler track with an event track config");
    this->damage_range(start, end, false);

    this->ruler_overlay.set_config(true, *track.ruler);
    if (track.ruler->bg.fl() != bg_box.color()) {
        bg_box.color(track.ruler->bg.fl());
        bg_box.redraw();
        if (title_box) {
            title_box->color(track.ruler->bg.fl());
            title_box->redraw();
        }
    }
}

void
RulerTrack::finalize_callbacks()
{
    ruler_overlay.delete_config();
}

// TODO: parts of this are the same as EventTrack::draw
// Drawing order:
// EventTrack: bg -> events -> ruler -> text -> trigger -> selection
// RulerTrack: bg ->           ruler ->                    selection
void
RulerTrack::draw()
{
    IRect draw_area = f_util::rect(this);

    // I used to look for FL_DAMAGE_SCROLL and use fl_scroll() for a fast
    // blit, but it was too hard to get right.  The biggest problem is that
    // events are at floats which are then rounded to ints for pixel positions.
    // DEBUG("damage: " << f_util::show_damage(damage()));
    if (damage() == FL_DAMAGE_CHILD || damage() == Track::DAMAGE_RANGE) {
        // Only CHILD damage means a selection was set.  But since I overlap
        // with the child, I have to draw too.
        // DEBUG("intersection with child: "
        //     << SHOW_RANGE(draw_area) << " + "
        //     << SHOW_RANGE(damaged_area) << " = "
        //     << SHOW_RANGE(draw_area.intersect(damaged_area)));
        draw_area = draw_area.intersect(this->damaged_area);
    } else {
        this->damage(FL_DAMAGE_ALL);
    }
    if (draw_area.w <= 0 || draw_area.h <= 0)
        return;

    // Prevent marks at the top and bottom from drawing outside the ruler.
    f_util::ClipArea clip_area(draw_area);
    this->draw_child(this->bg_box);

    // This is more than one pixel, but otherwise I draw on top of the bevel on
    // retina displays.
    IRect inside_bevel = f_util::rect(this);
    inside_bevel.x += 2; inside_bevel.w -= 3;
    inside_bevel.y += 2; inside_bevel.h -= 3;
    f_util::ClipArea clip_area2(inside_bevel);

    IRect box(x(), track_start(), w(), h() - (y()-track_start()));
    this->ruler_overlay.draw(box, zoom, inside_bevel);
    this->selection_overlay.draw(x(), track_start(), w(), zoom);
    this->damaged_area.w = this->damaged_area.h = 0;
}


// TODO: duplicated with EventTrack, move this into Track?
void
RulerTrack::set_selection(int selnum, const std::vector<Selection> &news)
{
    for (auto &sel : selection_overlay.get(selnum))
        damage_range(sel.low(), sel.high(), true);
    selection_overlay.set(selnum, news);
    for (auto &sel : news)
        damage_range(sel.low(), sel.high(), true);
}


std::string
RulerTrack::dump() const
{
    return "type ruler";
}
