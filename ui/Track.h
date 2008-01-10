/*
*/

#ifndef __TRACK_H
#define __TRACK_H

#include <vector>

#include <FL/Fl_Widget.H>
#include <FL/Fl_Group.H>
#include <FL/Fl_Box.H>

#include "util.h"
#include "f_util.h"

// Since I want to keep RulerModel and RulerView in the same file, instead
// of splitting them, I have to forward declare this.
class RulerModel;
// Model and view reference each other.
class TrackView;


// Dividers are not shared between views like tracks and rulers are, but being
// consistent with this structure is convenient.
struct DividerModel {
    DividerModel(Color c) : color(c) {}
    Color color;
};


class TrackModel {
private:
    // events
    // Views of this track, to update when it changes.
    std::vector<TrackView *> views;
};


struct TracklikeModel {
    // cheap union type
    TracklikeModel(TrackModel *t, RulerModel *r, DividerModel *d) :
        track(0), ruler(0), divider(0)
    {
        if (t) track = t;
        else if (r) ruler = r;
        else divider = d;
    }
    TrackModel *track;
    RulerModel *ruler;
    DividerModel *divider;
};


// Define methods that things appearing in track lanes should support.
// Also acts like a union of Divider, Track, and Ruler.
class TracklikeView : public Fl_Group {
public:
    TracklikeView() : Fl_Group(0, 0, 1, 1) {
        end(); // This is a Group, but I don't want anything else to fall in.
    }
    // zoom

    virtual bool resizable() const { return true; }
private:
    
};


class DividerView : public TracklikeView {
public:
    DividerView(const DividerModel &dm) : box(0, 0, 1, 1) {
        box.box(FL_FLAT_BOX);
        box.color(color_to_fl(dm.color));
        add(box);
    }
    bool resizable() const { return false; }
private:
    Fl_Box box;
};


class TrackView : public TracklikeView {
public:
    TrackView(TrackModel &track);

private:
    TrackModel &model;
    Fl_Box bg_box;
};

#endif
