#include "util.h"

#include "TrackTile.h"


TrackTile::TrackTile(int X, int Y, int W, int H, Color bg_color) :
    MoveTile(X, Y, W, H),
    track_pad(X, Y, W, H, "pad")
{
    end(); // don't automatically put more children in here
    track_pad.box(FL_FLAT_BOX);
    resizable(this);
    set_bg_color(bg_color);
}


TracklikeView *
TrackTile::track_at(int at)
{
    ASSERT(0 <= at && at <= tracks());
    return dynamic_cast<TracklikeView *>(child(at));
}


void
TrackTile::insert_track(int at, TracklikeView *track, int width)
{
    ASSERT(0 <= at && at <= tracks());
    ASSERT(width > 0);
    // Coords will be fixed by update_sizes()
    track->size(width, h());
    this->insert(*track, at);
    if (!track->resizable()) {
        this->set_child_not_resizable(at);
    }
    update_sizes();
}


TracklikeView *
TrackTile::remove_track(int at)
{
    ASSERT(0 <= at && at <= tracks());
    TracklikeView *t = track_at(at);
    remove(t);
    update_sizes();
    return t;
}


void
TrackTile::update_sizes()
{
    int xpos = 0;

    for (int i = 0; i < tracks(); i++) {
        Fl_Widget *w = child(i);
        w->resize(x() + xpos, y(), w->w(), h());
        xpos += w->w();
    }
    track_pad.resize(x() + xpos, y(), max(0, w() - xpos), h());
    init_sizes();
}
