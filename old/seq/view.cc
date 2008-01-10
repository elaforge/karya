#include <FL/Fl_Group.H>

// sequencer stuff
#include "util.h"
#include "s_util.h"
#include "Marklist.h"
#include "state.h"

// widgets
#include "f_util.h"
#include "A_input.h"
#include "A_scrollbar.h"
#include "Event.h"
#include "Ruler.h"
#include "Track.h"
#include "Zoom.h"
#include "Tile_ext.h"
#include "Track_tile.h"
#include "Block.h"

#include "view.h"

namespace seq {

Event_view::Event_view(Event_state *e, const Defaults *d) :
	event(e)
{
	widget = new widgets::Event(0, 0, 0, 0);
	update_colors();
}

Track_view::Track_view(Track_state *st, widgets::Track *t) :
	widget(t), state(st)
{
	events.reserve(state->events.size());
	for (unsigned int i = 0; i < state->events.size(); i++ ) {
		Event_view *ev = new Event_view(state->events[i], defaults);
		events.push_back(ev);
	}
	update_colors();
}

Block_view::Block_view(Fl_Group *parent, Block_state *st,
		const Defaults *d) :
	defaults(d),
	_title_size(d->title_size),
	_scrollbar_size(d->scrollbar_size),
	_ruler_size(d->ruler_size),
	_orientation(d->orientation),
	_time_zoom_speed(d->time_zoom_speed),
	_track_zoom_speed(d->track_zoom_speed),
	state(st)
{
	widget = new widgets::Block(parent->x(), parent->y(),
		parent->w(), parent->h(), _orientation);
	parent->add(widget);
	update_sizes();
	zoom_speed(_time_zoom_speed, _track_zoom_speed);
	// fully zoom out in y, but since track_tile has no tracks its width
	// is 0 and zoom 0/0 will be nonsensical.  As special magic, the
	// first insert_track() could reset the zoom.
	// Tpoint z = widget->track_tile_dimensions();
	// zoom_win(Trect(Trackpos(), Trackpos(), z.x, z.y));
	update_colors();
}

Block_view::~Block_view()
{
	for (unsigned i = 0; i < tracks.size(); i++)
		delete tracks[i];
}

void
Block_view::insert_track(int i, Track_state *st)
{
	invariant();
	i = int(container_clamp(tracks, i));
	// track height is not really time, but pretend it is to make things easier
	Trackpos th = Trackpos::from_sec(defaults->track_size);
	widgets::Track *t = widget->tracks()->insert_track(i, Tpoint(st->length, th));
	widget->update_sb();
	tracks.insert(tracks.begin() + i, new Track_view(st, t));
	invariant();
}

void
Block_view::remove_track(int i)
{
	invariant();
	Track_view *tv = track(i);
	widget->tracks()->remove_track(i);
	delete tv;
	tracks.erase(tracks.begin() + i);
	widget->update_sb();
	invariant();
}

void
Block_view::invariant() const
{
	for (unsigned int i = 0; i < tracks.size(); i++) {
		// Track_view widgets in sync with underlying widgets
		Assert(tracks[i]->widget == widget->tracks()->track(i));
	}

}

}
