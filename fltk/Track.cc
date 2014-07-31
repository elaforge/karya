// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include "f_util.h"

#include "Track.h"
#include "EventTrack.h"
#include "Ruler.h"
#include "MsgCollector.h"


int
TrackView::handle(int evt)
{
    // Capture drags, so that even if they go out of the bounds of the track
    // tile or the window I still send drag events from this track.
    if (evt == FL_PUSH || evt == FL_DRAG || evt == FL_RELEASE) {
        // If there is an edit_input it might want the event.
        if (!Fl_Group::handle(evt))
            MsgCollector::get()->event(evt, true);
        return 1;
    }
    return Fl_Group::handle(evt);
}


DividerView::DividerView(const DividerConfig &config) :
    TrackView("divider"), box(0, 0, 1, 1)
{
    box.box(FL_FLAT_BOX);
    box.color(config.color.fl());
    add(box);

    this->title_box = new Fl_Box(0, 0, 1, 1);
    title_box->box(FL_FLAT_BOX);
    title_box->color(config.color.fl());
}


void
DividerView::draw()
{
    IRect r = f_util::rect(this);
    r.h--;
    f_util::ClipArea c(r);
    TrackView::draw();
}


std::string
DividerView::dump() const
{
    return "type divider";
}
