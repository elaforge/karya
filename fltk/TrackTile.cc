// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <FL/Fl.H>
#include <FL/Fl_Window.H>
#include <FL/Fl_Widget.H>

#include "util.h"
#include "config.h"

#include "TrackTile.h"
#include "WrappedInput.h"
#include "MsgCollector.h"


TrackTile::TrackTile(int X, int Y, int W, int H, Color bg_color,
        int title_height) :
    MoveTile(X, Y, W, H),
    title_height(title_height),
    track_pad(X, Y, W, H),
    edit_input(NULL)
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
    if (this->edit_input && Fl::event_inside(edit_input))
        return edit_input->handle(evt);
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
TrackTile::set_zoom(const ZoomInfo &zoom)
{
    this->zoom = zoom;
    for (int i = 0; i < this->tracks(); i++)
        this->track_at(i)->set_zoom(zoom);
}


// edit input //////////////////////////

void
TrackTile::edit_input_cb(Fl_Widget *_w, void *vp)
{
    TrackTile *self = static_cast<TrackTile *>(vp);
    WrappedInput *input = self->edit_input;
    if (input == Fl::focus()) {
        int height = input->text_height();
        if (height != input->h()) {
            input->size(input->w(), height);
            self->redraw();
        }
    } else {
        self->edit_close();
    }
}

void
TrackTile::edit_open(int tracknum, ScoreTime pos, const char *text,
    int select_start, int select_end)
{
    ASSERT(0 <= tracknum && tracknum <= tracks());
    this->edit_close();
    int ypos = this->zoom.to_pixels(pos - zoom.offset);
    int xpos, width;
    if (tracks() == 0) {
        xpos = x();
        width = 60;
    } else {
        xpos = track_at(std::max(0, tracknum - 1))->x();
        width = track_at(std::max(0, tracknum - 1))->w();
    }
    // +3 gets the input right below the trigger line of an event at this
    // position.
    ypos += y() + title_height + 3;
    width -= 3;
    xpos += 2;
    this->edit_input = new WrappedInput(
        xpos, ypos, width, Config::View::track_title_height, false);
    edit_input->callback(edit_input_cb, static_cast<void *>(this));
    edit_input->show();
    // When a widget gets focus from a click, it becomes Fl::focus() and then
    // gets FL_FOCUS.  But when it gets focus from take_focus(), it gets
    // FL_FOCUS and then becomes Fl::focus().  edit_input_cb relies on the
    // former, so I have to assign focus first, or it will delete the widget
    // while I'm still working on it.
    Fl::focus(edit_input);
    edit_input->take_focus();
    if (*text)
        edit_input->set_text(text);
    if (select_start >= 0) {
        int len = strlen(text);
        edit_input->position(
            utf8::bytes(text, len, select_end),
            utf8::bytes(text, len, select_start));
    }

    this->add(edit_input);
    this->redraw();
}


void
TrackTile::edit_close()
{
    if (!this->edit_input)
        return;
    MsgCollector::get()->edit_input(this, edit_input->get_text());
    this->remove(edit_input);
    // This function can be called from the callback, and you can't delete
    // yourself from inside a callback without crashing.  So I have to delay
    // the delete until its safe to do so.
    Fl::delete_widget(edit_input);
    edit_input = NULL;
    this->redraw();
}


void
TrackTile::edit_insert(const char *text)
{
    if (!this->edit_input)
        return;
    edit_input->insert(text);
}

////////////////////////////////////////


ScoreTime
TrackTile::time_end() const
{
    // These both have a 1 minimum to keep others from dividing by 0.
    ScoreTime end(1);
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
        const TrackView *t = const_cast<TrackTile *>(this)->track_at(i);
        end = std::max(end, t->x() + t->w() - this->x());
    }
    return end;
}

IPoint
TrackTile::visible_pixels() const
{
    return IPoint(this->w(), std::max(0, this->h() - this->title_height));
}

void
TrackTile::insert_track(int tracknum, TrackView *track, int width)
{
    ASSERT(0 <= tracknum && tracknum <= tracks());

    // Track placement assumes [(title, track)], which the extra edit_input
    // child messes up.
    this->edit_close();
    // Can't create a track smaller than you could resize, except dividers
    // which are supposed to be small.
    if (track->track_resizable())
        width = std::max(this->minimum_size.x, width);

    // Just set sizes here, coords will be fixed by update_sizes()
    Fl_Widget &title = track->title_widget();
    title.size(width, this->title_height);
    WrappedInput *wrapped = dynamic_cast<WrappedInput *>(&title);
    if (wrapped)
        wrapped->callback(title_input_cb, static_cast<void *>(this));
    this->insert_child(title, title_index(tracknum));

    track->size(width, h() - this->title_height);
    this->insert_child(*track, track_index(tracknum));

    if (!track->track_resizable()) {
        this->set_stiff_child(track_index(tracknum));
        this->set_stiff_child(title_index(tracknum));
    }
    this->update_sizes();
}


TrackView *
TrackTile::remove_track(int tracknum)
{
    ASSERT(0 <= tracknum && tracknum <= tracks());
    this->edit_close();
    TrackView *t = track_at(tracknum);
    this->remove_child(t);
    this->remove_child(&t->title_widget());
    this->update_sizes();
    return t;
}


TrackView *
TrackTile::track_at(int tracknum)
{
    ASSERT(0 <= tracknum && tracknum < tracks());
    return dynamic_cast<TrackView *>(child(track_index(tracknum)));
}


const TrackView *
TrackTile::track_at(int tracknum) const
{
    ASSERT(0 <= tracknum && tracknum < tracks());
    return dynamic_cast<const TrackView *>(child(track_index(tracknum)));
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
    TrackView *track = this->track_at(tracknum);
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
TrackTile::title_input_cb(Fl_Widget *w, void *vp)
{
    WrappedInput *input = static_cast<WrappedInput *>(w);
    TrackTile *self = static_cast<TrackTile *>(vp);
    if (input == Fl::focus()) {
        int height = std::max(self->title_height, input->text_height());
        if (height != input->h()) {
            self->update_sizes();
            for (int i = 0; i < self->tracks(); i++) {
                if (self->child(title_index(i)) == input) {
                    TrackView *track = self->track_at(i);
                    ZoomInfo z = self->zoom;
                    track->set_zoom(ZoomInfo(
                        z.offset + z.to_time(height - self->title_height),
                        z.factor));
                    break;
                }
            }
        }
        // scroll the track underneath by an additional title_height - h()
    } else {
        // Collapse to the default height.
        self->update_sizes();
        // scroll track back
        for (int i = 0; i < self->tracks(); i++) {
            if (self->child(title_index(i)) == input) {
                MsgCollector::get()->track_title(
                    self, i + 1, input->get_text());
                self->track_at(i)->set_zoom(self->zoom);
                break;
            }
        }
    }
}
