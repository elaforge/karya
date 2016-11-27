// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include "f_util.h"

#include "EventTrack.h"
#include "MsgCollector.h"

#include "Track.h"
#include "SelectionOverlay.h" // for selection_point_size


#define SHOW_RANGE(r) (r).y << "--" << (r).b()

int
Track::handle(int evt)
{
    // Capture drags, so that even if they go out of the bounds of the track
    // tile or the window I still send drag events from this track.
    if (evt == FL_PUSH || evt == FL_DRAG || evt == FL_RELEASE) {
        // If there is an floating_input it might want the event.
        if (!Fl_Group::handle(evt))
            MsgCollector::get()->event(evt, true);
        return 1;
    }
    return Fl_Group::handle(evt);
}


void
Track::set_zoom(const Zoom &new_zoom)
{
    if (new_zoom == this->zoom)
        return;
    if (this->zoom.factor == new_zoom.factor)
        this->damage(FL_DAMAGE_SCROLL);
    else
        this->damage(FL_DAMAGE_ALL);
    this->zoom = new_zoom;
}


void
Track::damage_range(ScoreTime start, ScoreTime end, bool selection)
{
    IRect r = f_util::rect(this);
    if (start == ScoreTime(-1) && end == ScoreTime(-1)) {
        ; // leave it covering the whole widget
    } else {
        r.y += this->zoom.to_pixels(start - this->zoom.offset);
        r.h = this->zoom.to_pixels(end - start);
        if (selection) {
            // Extend the damage area to cover the bevel arrow thing in
            // draw_selections().
            r.y -= SelectionOverlay::selection_point_size;
            // +2, otherwise retina displays get a hanging pixel.
            r.h += SelectionOverlay::selection_point_size * 2 + 2;
        }
    }

    // DEBUG("damage_range(" << start << ", " << end << "): "
    //     << SHOW_RANGE(damaged_area) << " + " << SHOW_RANGE(r)
    //     << " = " << SHOW_RANGE(damaged_area.union_(r)));
    this->damaged_area = this->damaged_area.union_(r);
    // Ensure that the next draw() call redraws the damaged_area.
    this->damage(Track::DAMAGE_RANGE);
}


Divider::Divider(const DividerConfig &config) :
    Track("divider"), box(0, 0, 1, 1)
{
    box.box(FL_FLAT_BOX);
    box.color(config.color.fl());
    add(box);

    this->title_box = new Fl_Box(0, 0, 1, 1);
    title_box->box(FL_FLAT_BOX);
    title_box->color(config.color.fl());
}


void
Divider::draw()
{
    IRect r = f_util::rect(this);
    r.h--;
    f_util::ClipArea c(r);
    Track::draw();
}


std::string
Divider::dump() const
{
    return "type divider";
}
