#include <FL/Fl.H>
#include <FL/Fl_Window.H>
#include <FL/Fl_Widget.H>

#include "util.h"
#include "config.h"

#include "TrackTile.h"
#include "SeqInput.h"
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


static bool
raised_widget(Fl_Widget *w)
{
    SeqInput *input = dynamic_cast<SeqInput *>(w);
    return input && input->is_expanded();
}


// HACK: See comment on SeqInput::is_expanded.
int
TrackTile::handle(int evt)
{
    // Ahh, the copy-and-pastey goodness.  Offer the event to any raised
    // children first, and if they don't want it, pass it on to the superclass
    // as normal.  Since c++ makes it so hard to factor out bits of code,
    // I just copy and paste.  Good thing my event handling is minimal.

    switch (evt) {
    case FL_ENTER:
    case FL_MOVE:
        for (int i = 0; i < children(); i++) {
            if (!raised_widget(child(i)))
                continue;
            Fl_Widget *c = child(i);
            if (c->visible() && Fl::event_inside(c)) {
                if (c->contains(Fl::belowmouse())) {
                    return c->handle(FL_MOVE);
                } else {
                    Fl::belowmouse(c);
                    if (c->handle(FL_ENTER))
                        return 1;
                }
            }
        }
        return MoveTile::handle(evt);
    case FL_PUSH:
        for (int i = 0; i < children(); i++) {
            if (!raised_widget(child(i)))
                continue;
            Fl_Widget *c = child(i);
            if (c->takesevents() && Fl::event_inside(c)) {
                if (c->handle(FL_PUSH)) {
                    if (Fl::pushed() && !c->contains(Fl::pushed()))
                        Fl::pushed(c);
                    return 1;
                }
            }
        }
        // HACK: defocus any inputs since there's been a click elsewhere.
        // Normally BlockViewWindow::handle does this, but it can't tell the
        // difference between the input taking the event and the TrackTile
        // taking it.
        // There's a small bug where the cursor switch back to normal because
        // the refocus will change the cursor and MoveTile:set_cursor won't
        // notice, but it's not a big deal.
        // Fl_Cursor c = this->window()->cursor(); how to get old cursor?
        Fl::focus(this->window());
        // this->window()->cursor(c);
        return MoveTile::handle(evt);
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

static void
edit_input_cb(Fl_Widget *_w, void *vp)
{
    TrackTile *self = static_cast<TrackTile *>(vp);
    self->edit_close();
}

void
TrackTile::edit_open(int tracknum, ScoreTime pos, const char *text,
    int select_start, int select_end)
{
    // The edit_input is handled by TrackTile, so I can't put one on the ruler
    // track.  Also, when I report the tracknum to the MsgCollector, I should
    // report the absolute tracknum, not the TrackTile relative one.  So this
    // method takes an unadjusted absolute tracknum, and subtracts one
    // internally, except for tracknum 0 of course.
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
    this->edit_input = new SeqInput(
        xpos, ypos, width, Config::View::track_title_height, true, false);
    this->edit_input_tracknum = tracknum;
    this->edit_input_pos = pos;
    edit_input->set_callback2(edit_input_cb, static_cast<void *>(this));
    edit_input->show();
    if (text)
        edit_input->set_text(text);
    if (select_start >= 0)
        edit_input->position(select_end, select_start);

    this->add(edit_input);
    edit_input->take_focus();
    this->redraw();
}


void
TrackTile::edit_close()
{
    // Return or escape makes the input defocus itself.  Then it gets
    // FL_UNFOCUS, which calls SeqInput::contract and then
    // SeqInput::redraw_neighbors.  Then it invokes the 'edit_input_cb', which
    // then calls 'edit_close'.  edit_close then deletes the SeqInput, which
    // causes it to be removed from the TrackTile.
    if (!this->edit_input)
        return;
    MsgCollector::get()->edit_input(
        this, edit_input_tracknum, edit_input_pos, edit_input->value());
    delete edit_input;
    edit_input = NULL;
    this->redraw();
}


void
TrackTile::edit_append(const char *text)
{
    if (!this->edit_input)
        return;
    edit_input->insert_text(text);
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
        end = std::max(end,
                const_cast<TrackTile *>(this)->track_at(i)->time_end());
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
    int child_pos = tracknum*2;
    this->insert_child(title, child_pos);

    track->size(width, h() - this->title_height);
    this->insert_child(*track, child_pos+1);

    if (!track->track_resizable()) {
        this->set_stiff_child(child_pos);
        this->set_stiff_child(child_pos+1);
    }
    this->update_sizes();
    this->redraw();
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
    this->redraw();
    return t;
}


TrackView *
TrackTile::track_at(int tracknum)
{
    ASSERT(0 <= tracknum && tracknum < tracks());
    // Widgets alternate [title0, track0, title1, track1, ... box]
    return dynamic_cast<TrackView *>(child(tracknum*2 + 1));
}


const TrackView *
TrackTile::track_at(int tracknum) const
{
    ASSERT(0 <= tracknum && tracknum < tracks());
    // Widgets alternate [title0, track0, title1, track1, ... box]
    return dynamic_cast<const TrackView *>(child(tracknum*2 + 1));
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
    this->redraw();
}


int
TrackTile::get_dragged_track() const
{
    // DEBUG("dragged child " << this->dragged_child);
    if (this->dragged_child == -1)
        return -1;
    else
        return this->dragged_child / 2; // see track_at()
}


// HACK: See comment on SeqInput::is_expanded.
void
TrackTile::draw()
{
    // Copied from Fl_Group::draw
    if (damage() & ~FL_DAMAGE_CHILD) {
        draw_box();
        draw_label();
    }
    for (int i = 0; i < children(); i++) {
        if (raised_widget(child(i)))
            continue;
        Fl_Widget &c = *child(i);
        if (damage() & ~FL_DAMAGE_CHILD) {
            draw_child(c);
            draw_outside_label(c);
        } else {
            update_child(c);
        }
    }
    // Draw the expanded inputs on top.
    for (int i = 0; i < children(); i++) {
        if (!raised_widget(child(i)))
            continue;
        Fl_Widget &c = *child(i);
        draw_child(c);
        draw_outside_label(c);
    }
}


void
TrackTile::update_sizes()
{
    int xpos = 0;

    for (int i = 0; i < tracks(); i++) {
        Fl_Widget *title = child(i*2);
        Fl_Widget *body = child(i*2+1);
        // The title might be expanded if it's being edited.
        ASSERT(title->w() >= body->w());
        title->resize(x() + xpos, y(), title->w(), this->title_height);
        body->resize(x() + xpos, y() + this->title_height,
                body->w(), h() - this->title_height);
        xpos += body->w();
    }
    // track_pad can't be 0 width, see MoveTile.
    track_pad.resize(x() + xpos, y(), std::max(1, w() - xpos), h());
    // They should have been inserted at the right place.
    ASSERT(!this->sort_children());
    init_sizes();
}
