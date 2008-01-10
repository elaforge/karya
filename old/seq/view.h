#include <FL/Fl_Group.H>

/*
X_state - data for X which is should be displayed in all views
X_info - gui state for X.  view objects and widgets modify them, and widgets
watch them. 
X_view - the gui state for X.  has a pointer to its X_state and has data
which could be different for each view of X_state.
also contains the underlying widget.  setter/getter
overloaded pairs change state and inform the widgets or retrieve state.
set_*() methods change state without informing widgets and are passed
to the widgets that want to modify the state.

An attribute goes into X_state if it's intrinsic to the object so that
multiple views of the same object will show changes to the attribute.
If it can be different for different views, it goes into X_view.

Block_view -> Block_state
	|
Track_views -> Track_states
	|			|
Event_views -> Event_states

Widgets are dynamically allocated because fltk is going to want
to take possesion of them and delete them itself.
*/

namespace seq {

struct Defaults {
	// the block will default to its window's size, so this is for
	// whoever makes the window
	Point window_size; 

	int title_size, scrollbar_size, ruler_size;
	Orientation orientation;
	int track_size;

	double time_zoom_speed, track_zoom_speed;
	
	Block_state::Colors block_colors;
	Track_state::Colors track_colors;
	Event_state::Colors event_colors;
};

class Event_view {
public:
	Event_view(Event_state *e, const Defaults *d);
	void colors(const Event_state::Colors *c) {
		event->colors = c; 
		update_colors();
	}
	Event_state *event;
private:
	void update_colors() {
		widget->color(event->colors->bg);
		widget->text_color(event->colors->bg);
	}
	widgets::Event *widget;
};

class Track_view {
public:
	Track_view(Track_state *st, widgets::Track *t);
	void colors(const Track_state::Colors *c) {
		state->colors = c; 
		update_colors();
	}
	void length(Trackpos len) {
		state->length = len;
		widget->dimensions(Tpoint(len, widget->dimensions().y));
	}
	void insert_marklist(int i, const Marklist *m) {
		widget->insert_marklist(i, m);
	}
	void remove_marklist(int i) { widget->remove_marklist(i); }
	std::vector<Event_view *> events;
	// public since Block_view messes with it
	// XXX should this be the case?
	widgets::Track *widget;
private:
	Track_state *state;
	void update_colors() {
		widget->bg_color(state->colors->bg);
		widget->selection_colors(&state->colors->selections);
	}
	const Defaults *defaults;
};

class Block_view {
	// invariant: tracks[i]->widget == widget.track_tile().child(i)
public:
	Block_view(Fl_Group *parent, Block_state *st, const Defaults *d);
	~Block_view();

	void title_size(int n) { _title_size = n; update_sizes(); }
	void scrollbar_size(int n) { _scrollbar_size = n; update_sizes(); }
	void ruler_size(int n) { _ruler_size = n; update_sizes(); }
	void orientation(Orientation o) { _orientation = o; update_sizes(); }
	Orientation orientation() const { return _orientation; }
	Dpoint zoom_speed() const {
		return Dpoint(_time_zoom_speed, _track_zoom_speed);
	}
	void zoom_speed(double time, double track) {
		_time_zoom_speed = time;
		_track_zoom_speed = track;
		widget->update_zoom_speed(time, track);
	}
	void absolute_zoom(Trect zw) {
		widget->absolute_zoom(zw);
	}
	void relative_zoom(Dpoint z, Dpoint center) {
		widget->relative_zoom(z, center);
	}
	Trect zoom_win() const { return widget->zoom_win(); }
	// yay for c++ and no automatic accessors

	void title(const char *t) { state->title = t; update_title(); }
	const char *title() const { return state->title; }
	void colors(const Block_state::Colors *c) {
		state->colors = c;
		update_colors();
	}
	const Block_state::Colors *colors() const { return state->colors; }

	Track_view *track(int i) { return tracks.at(i); }
	void insert_track(int i, Track_state *t);
	void remove_track(int i);
	void insert_ruler_marklist(int i, const Marklist *m) {
		widget->insert_ruler_marklist(i, m);
	}
	void remove_ruler_marklist(int i) {
		widget->remove_ruler_marklist(i);
	}
	void insert_sb_marklist(int i, const Marklist *m) {
		widget->insert_sb_marklist(i, m);
	}
	void remove_sb_marklist(int i) {
		widget->remove_sb_marklist(i);
	}

private:
	std::vector<Track_view *> tracks;
	const Defaults *defaults;
	int _title_size, _scrollbar_size, _ruler_size;
	Orientation _orientation;
	double _time_zoom_speed, _track_zoom_speed;
	Cmd_history _view_history;

	Block_state *state;
	widgets::Block *widget;

	void update_sizes() {
		widget->update_sizes(_title_size, _ruler_size,
			_scrollbar_size, _orientation);
		// XXX for t in tracks: update_sizes()
	}
	void update_title() { widget->update_title(state->title); }
	void update_colors() { widget->update_colors(state->colors); }
	void invariant() const;
};

}
