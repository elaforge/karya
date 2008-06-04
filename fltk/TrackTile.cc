#include <FL/Fl_Widget.H>

#include "util.h"

#include "TrackTile.h"
#include "SeqInput.h"


TrackTile::TrackTile(int X, int Y, int W, int H, Color bg_color,
        int title_height) :
    MoveTile(X, Y, W, H),
    title_height(title_height),
    track_pad(X, Y, W, H)
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
        return MoveTile::handle(evt);
    }
    return MoveTile::handle(evt);
}


void
TrackTile::set_zoom(const ZoomInfo &zoom)
{
    for (int i = 0; i < this->tracks(); i++)
        this->track_at(i)->set_zoom(zoom);
}


TrackPos
TrackTile::time_end() const
{
    // These both have a 1 minimum to keep others from dividing by 0.
    TrackPos end(1);
    // It's too much hassle to make a const version of track_at when I know
    // I'm using it const.
    for (int i = 0; i < this->tracks(); i++) {
        end = std::max(end,
                const_cast<TrackTile *>(this)->track_at(i)->time_end());
    }
    return end;
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


void
TrackTile::insert_track(int tracknum, TrackView *track, int width)
{
    ASSERT(0 <= tracknum && tracknum <= tracks());

    // Can't create a track smaller than you could resize, except dividers
    // which are supposed to be small.
    if (track->track_resizable())
        width = std::max(this->minimum_size.x, width);

    // Just set sizes here, coords will be fixed by update_sizes()
    Fl_Widget &title = track->title_widget();
    title.size(width, this->title_height);
    int child_pos = tracknum*2;
    this->insert(title, child_pos);

    track->size(width, h() - this->title_height);
    this->insert(*track, child_pos+1);

    if (!track->track_resizable()) {
        this->set_stiff_child(child_pos);
        this->set_stiff_child(child_pos+1);
        // DEBUG("stiff: " << child_pos);
    }
    this->update_sizes();
    this->redraw();
}


TrackView *
TrackTile::remove_track(int tracknum)
{
    ASSERT(0 <= tracknum && tracknum <= tracks());
    TrackView *t = track_at(tracknum);
    remove(t);
    remove(t->title_widget());
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


int
TrackTile::get_track_width(int tracknum)
{
    return this->track_at(tracknum)->w();
}


void
TrackTile::set_track_width(int tracknum, int width)
{
    ASSERT(width > 0);
    TrackView *track = this->track_at(tracknum);
    if (track->track_resizable())
        width = std::max(this->minimum_size.x, width);

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
        ASSERT(title->w() == body->w());
        int width = title->w();

        title->resize(x() + xpos, y(), width, this->title_height);
        body->resize(x() + xpos, y() + this->title_height,
                width, h() - this->title_height);
        xpos += width;
    }
    // track_pad can't be 0 width, see MoveTile.
    track_pad.resize(x() + xpos, y(), std::max(1, w() - xpos), h());
    // They should have been inserted at the right place.
    ASSERT(!this->sort_children());
    init_sizes();
}
