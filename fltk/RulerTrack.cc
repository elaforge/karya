// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <math.h>
#include <utility>

#include "f_util.h"
#include "util.h"

#include "RulerTrack.h"


// RulerTrack

RulerTrack::RulerTrack(const RulerConfig &config) :
    Track("ruler"),
    title_box(nullptr),
    body_scroll(0, 0, 1, 1),
        body(config)
{
    end();
    this->add(body_scroll);
    body_scroll.add(body);
    body_scroll.set_bg(config.bg);
}


void
RulerTrack::resize(int x, int y, int w, int h)
{
    bool changed = w != this->w() || h != this->h();
    Track::resize(x, y, w, h);
    // CachedScroll doesn't propagate size changes to its child, so I have to
    // do it manually for width.
    body.resize(x, y, w, body.h());
    if (changed)
        invalidate();
}


// Don't create the title widget until it is actually requested.  This avoids
// creating a title box for Rulers that don't have one.
Fl_Box &
RulerTrack::title_widget()
{
    if (!this->title_box) {
        this->title_box = new Fl_Box(0, 0, 1, 1);
        title_box->box(FL_FLAT_BOX);
        title_box->color(body.ruler_overlay.config.bg.fl());
    }
    return *this->title_box;
}


void
RulerTrack::update(const Tracklike &track, ScoreTime start, ScoreTime end)
{
    ASSERT_MSG(track.ruler && !track.track,
        "updated a ruler track with an event track config");

    body.ruler_overlay.set_config(true, *track.ruler);
    body.update_size();
    body_scroll.set_bg(track.ruler->bg);
    if (title_box) {
        title_box->color(track.ruler->bg.fl());
        title_box->redraw();
    }
    invalidate();
}


void
RulerTrack::finalize_callbacks()
{
    body.ruler_overlay.delete_config();
}


// Drawing order:
// EventTrack: bg -> events -> wave -> signal -> ruler -> text -> trigger -> sel
// RulerTrack: bg ->                             ruler ->                 -> sel
void
RulerTrack::draw()
{
    // The selection moved, so get the cache to redraw, but not recache.
    if ((damage() & ~FL_DAMAGE_CHILD) == Track::DAMAGE_SELECTION) {
        this->clear_damage();
        body_scroll.damage(FL_DAMAGE_SCROLL);
    }
    Track::draw();
    selection_overlay.draw(x(), track_start(body), w(), body.zoom);
}


// TODO: duplicated with EventTrack, move this into Track?
void
RulerTrack::set_selection(int selnum, const std::vector<Selection> &news)
{
    selection_overlay.set(selnum, news);
    // selection_overlay isn't a Fl_Widget, so I can't redraw() it.  Instead, I
    // add a special kind of damage.
    this->damage(Track::DAMAGE_SELECTION);
}


void
RulerTrack::set_zoom(const Zoom &new_zoom)
{
    if (new_zoom == body.zoom)
        return;
    bool factor_changed = new_zoom.factor != body.zoom.factor;

    if (factor_changed)
        invalidate();
    // Otherwise just offset changed and CachedScroll can avoid a redraw.
    // this->zoom = new_zoom;
    body_scroll.set_offset(IPoint(0, -new_zoom.to_pixels(new_zoom.offset)));

    body.zoom = new_zoom;
    if (factor_changed)
        body.update_size();
}


std::string
RulerTrack::dump() const
{
    return "type ruler";
}


// Body ////////////////////////////////


RulerTrack::Body::Body(const RulerConfig &config) :
    Fl_Widget(0, 0, 1, 1),
    ruler_overlay(config, true)
{
    update_size();
}


void
RulerTrack::Body::draw()
{
    fl_color(ruler_overlay.config.bg.fl());
    fl_rectf(x(), y(), w(), h());

    // TODO later ruler_overlay will always draw from 0
    IRect box(x(), track_start(*this),
        w(), track_end(*this) - track_start(*this));
    this->ruler_overlay.draw(box, Zoom(ScoreTime(0), zoom.factor), box);
}


void
RulerTrack::Body::update_size()
{
    // I'm not sure why +4, but otherwise it's not quite long enough.
    // Maybe it has to do with the +2 in track_start()?
    this->resize(x(), y(), w(), zoom.to_pixels(time_end()) + 4);
}
