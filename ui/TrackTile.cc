#include <FL/Fl_Widget.H>

#include "util.h"

#include "TrackTile.h"


TrackTile::TrackTile(int X, int Y, int W, int H, Color bg_color,
        int title_height) :
    MoveTile(X, Y, W, H),
    title_height(title_height),
    track_pad(X, Y, W, H, "P")
{
    ASSERT(title_height >= 0);
    end(); // don't automatically put more children in here
    track_pad.box(FL_FLAT_BOX);
    resizable(this);
    set_bg_color(bg_color);
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
TrackTile::insert_track(int at, TrackView *track, int width)
{
    ASSERT(0 <= at && at <= tracks());
    ASSERT(width > 0);
    int child_pos = at*2;

    // Just set sizes here, coords will be fixed by update_sizes()
    Fl_Widget &title = track->title_widget();
    DEBUG("insert " << &title << " " << this->title_height);
    title.size(width, this->title_height);
    DEBUG("insert " << track);
    this->insert(title, child_pos);

    track->size(width, h() - this->title_height);
    this->insert(*track, child_pos+1);

    if (!track->track_resizable()) {
        this->set_stiff_child(child_pos);
        this->set_stiff_child(child_pos+1);
        DEBUG("stiff: " << child_pos);
    }
    this->update_sizes();
    this->redraw();
}


TrackView *
TrackTile::remove_track(int at)
{
    ASSERT(0 <= at && at <= tracks());
    TrackView *t = track_at(at);
    remove(t);
    remove(t->title_widget());
    this->update_sizes();
    this->redraw();
    return t;
}


TrackView *
TrackTile::track_at(int at)
{
    ASSERT(0 <= at && at <= tracks());
    return dynamic_cast<TrackView *>(child(at*2 + 1));
}


void
TrackTile::set_track_width(int at, int width)
{
    ASSERT(width > 0);
    TrackView *track = this->track_at(at);
    Fl_Widget &title = track->title_widget();
    title.size(width, title.h());
    track->size(width, track->h());
    this->update_sizes();
    this->redraw();
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
