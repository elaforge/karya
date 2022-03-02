// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <string>
#include <FL/Fl.H>
#include <FL/Fl_Window.H>
#include <FL/Fl_Widget.H>

#include "MsgCollector.h"
#include "WrappedInput.h"
#include "config.h"
#include "utf8.h"
#include "util.h"

#include "TrackTile.h"


TrackTile::TrackTile(int x, int y, int w, int h, Color bg_color,
        int title_height) :
    MoveTile(x, y, w, h),
    title_height(title_height),
    track_pad(x, y, w, h),
    floating_input(nullptr)
{
    ASSERT(title_height >= 0);
    end(); // don't automatically put more children in here
    track_pad.box(FL_FLAT_BOX);
    resizable(this);
    set_bg_color(bg_color);
}

// Widgets alternate [title0, track0, title1, track1, ... box]
static int
track_index(int tracknum)
{
    return tracknum * 2 + 1;
}

static int
title_index(int tracknum)
{
    return tracknum * 2;
}


int
TrackTile::handle(int evt)
{
    if (this->floating_input && Fl::event_inside(floating_input))
        return floating_input->handle(evt);
    if (evt == FL_PUSH || evt == FL_DRAG) {
        // Give mouse priority to any focused child, which may be a
        // WrappedInput overlapping other children.
        for (int i = 0; i < children(); i++) {
            if (Fl::focus() == child(i) && Fl::event_inside(child(i))) {
                return Fl::focus()->handle(evt);
            }
        }
    }
    if (evt == FL_PUSH) {
        // If the click is on a track I want to take focus off of a text input.
        for (int i = 0; i < tracks(); i++) {
            if (Fl::event_inside(track_at(i))) {
                Fl::focus(this->window());
                break;
            }
        }
    }
    return MoveTile::handle(evt);
}


void
TrackTile::set_zoom(const Zoom &zoom)
{
    this->zoom = zoom;
    for (int i = 0; i < this->tracks(); i++)
        this->track_at(i)->set_zoom(zoom);
}


// floating input //////////////////////

static void
floating_input_cb(Fl_Widget *w, void *arg)
{
    TrackTile *self = static_cast<TrackTile *>(arg);
    WrappedInput *input = static_cast<WrappedInput *>(w);
    if (Fl::event() == FL_UNFOCUS) {
        self->floating_close();
    } else {
        input->update_size();
        self->redraw();
    }
}

void
TrackTile::floating_open(int tracknum, ScoreTime pos, const char *text,
    int select_start, int select_end)
{
    ASSERT_MSG(0 <= tracknum && tracknum < tracks(), std::to_string(tracknum));
    this->floating_close();
    int ypos = this->zoom.to_pixels(pos - zoom.offset);
    ypos = std::max(0, std::min(h(), ypos));
    int xpos = tracks() == 0 ? x() : track_at(tracknum)->x();
    // The OS will make sure the window doesn't go off the right edge.
    const int width = tracks() == 0 ? 60 : track_at(tracknum)->w() - 3;

    // +3 gets the input right below the trigger line of an event at this
    // position.
    ypos += y() + title_height + 3;
    // More pixel tweaks to keep the input from going off the bottom.
    int max_y = this->y() + this->h() - Config::font_size::input - 4;
    ypos = std::min(ypos, max_y);
    xpos += 2;
    int max_w = window()->w() - xpos;
    this->floating_input = new WrappedInput(
        xpos, ypos, width, Config::Block::track_title_height,
        false, max_w);
    if (text && *text)
        floating_input->set_text(text);
    floating_input->take_focus();
    floating_input->update_size();
    this->add(floating_input);
    floating_input->callback(floating_input_cb, static_cast<void *>(this));
    int len = strlen(text);
    floating_input->position(
        utf8::bytes(text, len, select_end),
        utf8::bytes(text, len, select_start));
    this->redraw();
}


void
TrackTile::floating_close()
{
    if (!this->floating_input)
        return;
    MsgCollector::get()->floating_input(
        this,
        floating_input->text_changed() ? floating_input->get_text() : nullptr);
    this->remove(floating_input);
    floating_input->hide();
    // This function can be called from the callback, and you can't delete
    // yourself from inside a callback without crashing.  So I have to delay
    // the delete until its safe to do so.
    Fl::delete_widget(floating_input);
    this->floating_input = nullptr;
    this->redraw();
}


void
TrackTile::floating_insert(const char *text)
{
    if (this->floating_input)
        floating_input->insert(text);
    else
        DEBUG("TrackTile::floating_insert called but no open input: " << text);
}

////////////////////////////////////////


ScoreTime
TrackTile::time_end() const
{
    ScoreTime end(0);
    // It's too much hassle to make a const version of track_at when I know
    // I'm using it const.
    for (int i = 0; i < this->tracks(); i++) {
        end = std::max(
            end, const_cast<TrackTile *>(this)->track_at(i)->time_end());
    }
    return end;
}

ScoreTime
TrackTile::view_end() const
{
    return this->zoom.to_time(this->h() - this->title_height)
        + this->zoom.offset;
}

ScoreTime
TrackTile::visible_time() const
{
    return this->zoom.to_time(this->h() - this->title_height);
}

int
TrackTile::track_end() const
{
    // These both have a 1 minimum to keep others from dividing by 0.
    int end = 1;
    for (int i = 0; i < this->tracks(); i++) {
        const Track *t = const_cast<TrackTile *>(this)->track_at(i);
        end = std::max(end, t->x() + t->w() - this->x());
    }
    return end;
}

void
TrackTile::insert_track(int tracknum, Track *track, int width)
{
    // Unlike the other tracknums, this is allowed to be one past the end.
    // It's because this tracknum is where to insert a new track, not an
    // address for an existing one.
    ASSERT_MSG(0 <= tracknum && tracknum <= tracks(), std::to_string(tracknum));

    // Can't create a track smaller than you could resize, except dividers
    // which are supposed to be small.
    if (track->track_resizable())
        width = std::max(this->minimum_size.x, width);

    // Just set sizes here, coords will be fixed by update_sizes()
    Fl_Widget &title = track->title_widget();
    title.size(width, this->title_height);
    this->insert_child(title, title_index(tracknum));

    track->size(width, h() - this->title_height);
    this->insert_child(*track, track_index(tracknum));
    // EventTrack will call this to tell TrackTile to redraw.
    track->callback(title_input_cb_dispatch, static_cast<void *>(this));

    if (!track->track_resizable()) {
        this->set_stiff_child(track_index(tracknum));
        this->set_stiff_child(title_index(tracknum));
    }
    this->update_sizes();
}


Track *
TrackTile::remove_track(int tracknum)
{
    ASSERT_MSG(0 <= tracknum && tracknum < tracks(), std::to_string(tracknum));
    Track *t = track_at(tracknum);
    this->remove_child(t);
    this->remove_child(&t->title_widget());
    this->update_sizes();
    return t;
}


Track *
TrackTile::track_at(int tracknum)
{
    ASSERT_MSG(0 <= tracknum && tracknum < tracks(), std::to_string(tracknum));
    return dynamic_cast<Track *>(child(track_index(tracknum)));
}


const Track *
TrackTile::track_at(int tracknum) const
{
    ASSERT_MSG(0 <= tracknum && tracknum < tracks(), std::to_string(tracknum));
    return dynamic_cast<const Track *>(child(track_index(tracknum)));
}


int
TrackTile::get_track_width(int tracknum) const
{
    return this->track_at(tracknum)->w();
}


void
TrackTile::set_track_width(int tracknum, int width)
{
    width = std::max(3, width);
    Track *track = this->track_at(tracknum);
    if (track->track_resizable())
        width = std::max(this->minimum_size.x, width);
    if (track->w() == width)
        return;

    Fl_Widget &title = track->title_widget();
    title.size(width, title.h());
    track->size(width, track->h());
    this->update_sizes();
}


int
TrackTile::get_dragged_track() const
{
    if (this->dragged_child == -1)
        return -1;
    else
        return this->dragged_child / 2; // see track_at()
}


void
TrackTile::draw()
{
    MoveTile::draw();
    // Always draw the floating_input last, since it's on top.
    if (floating_input != nullptr) {
        draw_child(*floating_input);
    }
    // Also give draw priority to a focused child, which may be a WrappedInput
    // overlapping other children.
    for (int i = 0; i < children(); i++) {
        if (Fl::focus() == child(i))
            draw_child(*child(i));
    }
    util::timing(2, "TrackTile::draw");
}


void
TrackTile::update_sizes()
{
    int xpos = 0;

    for (int i = 0; i < tracks(); i++) {
        Fl_Widget *title = child(title_index(i));
        Fl_Widget *body = child(track_index(i));

        // If it's a WrappedText, and has focus, then resize to
        // max(title_height, text_height()), otherwise title_height.
        WrappedInput *input = dynamic_cast<WrappedInput *>(title);
        int height = this->title_height;
        if (input && input == Fl::focus())
            height = std::max(height, input->text_height());
        title->resize(x() + xpos, y(), body->w(), height);
        body->resize(x() + xpos, y() + height, body->w(), h() - height);
        xpos += body->w();
    }
    // track_pad can't be 0 width, see MoveTile.
    track_pad.resize(x() + xpos, y(), std::max(1, w() - xpos), h());
    // They should have been inserted at the right place.
    ASSERT(!this->sort_children());
    init_sizes();
    redraw();
}


void
TrackTile::title_input_cb_dispatch(Fl_Widget *w, void *arg)
{
    TrackTile *self = static_cast<TrackTile *>(arg);
    self->title_input_cb(w);
}

void
TrackTile::title_input_cb(Fl_Widget *w)
{
    redraw();
}
