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
    TrackView(int title_height) :
        Fl_Group(0, 0, 1, 1), title_height(title_height)
    {
        end(); // This is a Group, but I don't want anything else to fall in.
    }
    // zoom

    virtual bool track_not_resizable() const { return true; }

protected:
    int title_height;

private:
    
};


class DividerView : public TrackView {
public:
    DividerView(const DividerModel &dm, int title_height) :
        TrackView(title_height), box(0, 0, 1, 1)
    {
        box.box(FL_FLAT_BOX);
        box.color(color_to_fl(dm.color));
        add(box);
    }
    bool track_not_resizable() const { return false; }
private:
    Fl_Box box;
};

#endif
