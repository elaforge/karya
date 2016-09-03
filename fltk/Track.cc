// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include "f_util.h"

#include "EventTrack.h"
#include "MsgCollector.h"

#include "Track.h"


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
