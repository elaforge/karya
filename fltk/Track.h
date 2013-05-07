/*
*/

#ifndef __TRACK_H
#define __TRACK_H

#include <FL/Fl_Widget.H>
#include <FL/Fl_Group.H>
#include <FL/Fl_Box.H>

#include "util.h"
#include "f_util.h"
#include "types.h"

// Since I want to keep RulerTrackModel and RulerTrackView in the same file,
// instead of splitting them, I have to forward declare this.
class RulerConfig;
class EventTrackConfig;
class TrackSignal;


// Dividers are not shared between views like tracks and rulers are, but being
// consistent with this structure is convenient.
struct DividerConfig {
    DividerConfig(Color c) : color(c) {}
    Color color;
};


struct Tracklike {
    // cheap variant record
    Tracklike(EventTrackConfig *t, RulerConfig *r)
        : track(t), ruler(r), divider(0) {}
    Tracklike(RulerConfig *r)
        : track(0), ruler(r), divider(0) {}
    Tracklike(DividerConfig *d)
        : track(0), ruler(0), divider(d) {}
    // If track is non-NULL but ruler is NULL, that means the ruler gets no
    // update.  Normally a track update updates everything, but since copying
    // large rulers on every unrelated change is slow, I omit the ruler except
    // when it actually changed.

    // TODO: const?
    EventTrackConfig *track;
    RulerConfig *ruler;
    DividerConfig *divider;
};

inline std::ostream &
operator<<(std::ostream &os, const Tracklike &track)
{
    return os << "Tracklike(event=" << track.track
        << ", ruler=" << track.ruler << ", div=" << track.divider << ")";
}


// Define methods that things appearing in track lanes should support.
// Also acts like a union of Divider, Track, and Ruler.
class TrackView : public Fl_Group {
public:
    explicit TrackView(const char *label=0) : Fl_Group(0, 0, 1, 1, label) {
        this->labeltype(FL_NO_LABEL);
        end(); // This is a Group, but I don't want anything else to fall in.
        // DEBUG("created track view " << this);
    }
    virtual ~TrackView() {
        // DEBUG("deleted track view " << this);
    }
    virtual int handle(int evt);

    virtual void set_zoom(const ZoomInfo &zoom) {}
    virtual void set_selection(int selnum, int tracknum, const Selection &sel)
    {}
    virtual bool track_resizable() const { return true; }
    virtual void set_event_brightness(double d) {}

    // Return the end of the last event.
    virtual ScoreTime time_end() const { return ScoreTime(0); }
    virtual void update(const Tracklike &track, ScoreTime start, ScoreTime end)
    {}
    virtual void set_track_signal(const TrackSignal &tsig) = 0;

    // This is called before the object is deleted.
    virtual void finalize_callbacks() {}
    virtual std::string dump() const = 0;

    // Only implemented for Event tracks.  Would it be nicer to have one
    // method that returns *EventTrackView?

    // Factory to generate the title widget for this track.  It should be
    // dynamically allocated because it will be passed to TrackTile who will
    // own it.
    virtual Fl_Widget &title_widget() = 0;

    // The text in the title_widget, if there is any.
    virtual const char *get_title() const { return 0; }

    virtual void set_title(const char *title) {}
    virtual void set_title_focus() {}
};


class DividerView : public TrackView {
public:
    explicit DividerView(const DividerConfig &config);
    bool track_resizable() const { return false; }
    virtual void update(const Tracklike &track, ScoreTime start, ScoreTime end)
    {
        ASSERT_MSG(!(track.track || track.ruler),
            "updated a divider with a non-divider config");
        // Of course even a divider config won't do anything...
    }
    virtual void set_track_signal(const TrackSignal &tsig) {
        DEBUG("WARNING: got a track signal on a divider track!");
    }
    virtual Fl_Box &title_widget() { return *this->title_box; }
    virtual std::string dump() const;
protected:
    void draw();
private:
    Fl_Box box;
    Fl_Box *title_box;
};

#endif
