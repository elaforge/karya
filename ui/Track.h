/*
*/

#ifndef __TRACK_H
#define __TRACK_H

#include <FL/Fl_Widget.H>
#include <FL/Fl_Group.H>
#include <FL/Fl_Box.H>

#include "util.h"
#include "f_util.h"

// Since I want to keep RulerTrackModel and RulerTrackView in the same file,
// instead of splitting them, I have to forward declare this.
class RulerTrackModel;
class EventTrackModel;


// Dividers are not shared between views like tracks and rulers are, but being
// consistent with this structure is convenient.
struct DividerModel {
    DividerModel(Color c) : color(c) {}
    Color color;
};


struct TrackModel {
    // cheap union type
    TrackModel(EventTrackModel *t, RulerTrackModel *r, DividerModel *d) :
        track(0), ruler(0), divider(0)
    {
        if (t) track = t;
        else if (r) ruler = r;
        else divider = d;
    }
    EventTrackModel *track;
    RulerTrackModel *ruler;
    DividerModel *divider;
};


// Define methods that things appearing in track lanes should support.
// Also acts like a union of Divider, Track, and Ruler.
class TrackView : public Fl_Group {
public:
    TrackView() : Fl_Group(0, 0, 1, 1) {
        end(); // This is a Group, but I don't want anything else to fall in.
    }
    // zoom

    virtual bool track_not_resizable() const { return true; }

    // Factory to generate the title widget for this track.  It should be
    // dynamically allocated because it will be passed to TrackTile who will
    // own it.
    virtual Fl_Widget &title_widget() = 0;
};


class DividerView : public TrackView {
public:
    DividerView(const DividerModel &dm) :
        box(0, 0, 1, 1)
    {
        box.box(FL_FLAT_BOX);
        box.color(color_to_fl(dm.color));
        add(box);

        this->title_box = new Fl_Box(0, 0, 1, 1);
        title_box->box(FL_FLAT_BOX);
        title_box->color(color_to_fl(dm.color));
    }
    bool track_not_resizable() const { return false; }
    virtual Fl_Box &title_widget() { return *this->title_box; }
private:
    Fl_Box box;
    Fl_Box *title_box;
};

#endif
