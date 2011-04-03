#ifndef __BLOCK_H
#define __BLOCK_H

/*
      _________ block (Group) ___
     /              |            \
    /               |           status_line (Output)
title ________ body (Tile) ___________________________________
     /           |           \                                \
skel_box  skel_display  ruler_group _____________        track_group
                       /       \         \       \         |        \
                    track_box sb_box   time_sb  ruler track_sb  track_scroll
                                                                  /
                                                              track_tile
                                                              /
                                                track | ruler | divider, ...
                                               /    \
                                      track_body    track_title
                                 overlay_ruler
                                 /
                             event, ...
*/

#include <algorithm>
#include <utility>
#include <vector>
#include <map>

#include <FL/Fl.H>
#include <FL/Fl_Group.H>
#include <FL/Fl_Output.H>
#include <FL/Fl_Tile.H>
#include <FL/Fl_Box.H>
#include <FL/Fl_Double_Window.H>

#include "SimpleScroll.h"
#include "SeqScrollbar.h"
#include "SeqInput.h"

#include "config.h"
#include "types.h"
#include "Track.h"
#include "EventTrack.h"
#include "TrackTile.h"
#include "Ruler.h"
#include "Event.h"
#include "SkeletonDisplay.h"


struct BlockModelConfig {
    Color bg;
    Color track_box;
    Color sb_box;
    char track_char;
    char sb_char;
};


struct BlockViewConfig {
    int block_title_height;
    int track_title_height;
    int skel_height;
    int sb_size;
    int status_size;
};

// Track config local to each BlockView.
struct DisplayTrack {
    double event_brightness;
    Color status_color;
    char status;
};


// The ruler track is a special track that doesn't scroll with set_track_scroll
// like the other tracks do.  There is always just one ruler track and it's
// at tracknum 0.  It ignores remove_track, but will be replaced by
// insert_track.
class BlockView : public Fl_Group {
public:
    BlockView(int X, int Y, int W, int H,
            const BlockModelConfig &model_config,
            const BlockViewConfig &view_config);

    int handle(int evt);
    void resize(int X, int Y, int W, int H);
    void set_view_config(const BlockViewConfig &view_config,
            bool update_all=false);
    void set_model_config(const BlockModelConfig &config,
            bool update_all=false);
    void set_skeleton(const SkeletonConfig &skel);

    // Set the zoom, which is the view rectangle in the timewise direction.
    const ZoomInfo &get_zoom() const { return zoom; }
    void set_zoom(const ZoomInfo &zoom);
    // Get and set trackwise scrolling, in pixels.
    int get_track_scroll() const;
    void set_track_scroll(int offset);
    IPoint get_track_size() const { return track_tile.visible_pixels(); }

    void set_selection(int selnum, const Selection &sel);
    // This is different from 'set_selection' because it only sets or clears
    // the selection for one track.  This way, you can put the selection in
    // different places on different tracks.  The 'start_track' field of the
    // Selection is ignored, and 'tracks' can be 0 to clear, and 1 to set.
    //
    // It's used by the playback updater to display the play positions going
    // at different speeds.
    void set_track_selection(int selnum, int tracknum, const Selection &sel);

    void set_title(const char *s) { title.set_text(s); }
    const char *get_title() const { return title.value(); }
    void set_status(const char *s) { status_line.value(s); }

    void insert_track(int tracknum, const Tracklike &track, int width);
    void remove_track(int tracknum, FinalizeCallback finalizer);
    void set_display_track(int tracknum, const DisplayTrack &dtrack);
    void collapse_track(int tracknum, bool collapse);

private:
    void insert_track_view(int tracknum, TrackView *track, int width);
    TrackView *replace_ruler_track(TrackView *track, int width);
public:
    // Update the given track.  Update scrollbars.
    // 'track' should be the same kind of track as the one at 'tracknum' or
    // this throws.  Update colors and whatnot if they have changed (pointers
    // may be passed as NULL which means no change).  Also mark that the range
    // 'start' to 'end' should be updated.  If 'end' is ScoreTime(0), the
    // entire range should be updated.
    void update_track(int tracknum, const Tracklike &track,
            FinalizeCallback finalizer, ScoreTime start, ScoreTime end);

    // Update the signal for this track.
    void set_track_signal(int tracknum, const TrackSignal &tsig);

    TrackView *track_at(int tracknum) {
        if (tracknum == 0)
            return ruler_track;
        else
            return track_tile.track_at(tracknum-1);
    }
    int tracks() const { return track_tile.tracks() + 1; }
    int get_track_width(int tracknum);
    void set_track_width(int tracknum, int width);

private:
    BlockModelConfig model_config;
    BlockViewConfig view_config;
    ZoomInfo zoom;
    // Save a collapsed track so it can be expanded later.
    struct CollapsedTrack {
        // Uncollapsed tracks have empty CollapsedTrack.
        CollapsedTrack() : track(NULL) {}
        CollapsedTrack(TrackView *track, int width) : track(track), width(width)
        {}
        DisplayTrack display;
        TrackView *track;
        int width;
    };
    // Indexed by tracknum, with track==NULL if this tracknum isn't collapsed.
    std::vector<CollapsedTrack> collapsed_tracks;
    // The ruler track gets this when there's "nothing" in it.
    TrackView *no_ruler;

    SeqInput title;
    Fl_Output status_line;
    Fl_Tile body;
        // Dummy group to limit body tile drag to the ruler track.
        Fl_Group body_resize_group;
        Fl_Box skel_box;
        SimpleScroll skel_display_scroll;
            SkeletonDisplay skel_display;
        Fl_Group ruler_group;
            Fl_Box track_box;
            Fl_Box sb_box;
            FlSeqScrollbar time_sb;
            // P9SeqScrollbar time_sb;
            // This can be replaced.
            TrackView *ruler_track;
        Fl_Group track_group;
            FlSeqScrollbar track_sb;
            // P9SeqScrollbar track_sb;
            SimpleScroll track_scroll;
                TrackTile track_tile;

    void set_zoom_attr(const ZoomInfo &zoom);
    void set_ruler_width(int width);
    void update_scrollbars();

    // Called by scrollbar.
    static void scrollbar_cb(Fl_Widget *w, void *vp);
    static void update_scrollbars_cb(Fl_Widget *w, void *vp);
    static void track_tile_cb(Fl_Widget *w, void *vp);
};


class BlockViewWindow : public Fl_Double_Window {
public:
    BlockViewWindow(int X, int Y, int W, int H,
            const char *label,
            const BlockModelConfig &model_config,
            const BlockViewConfig &view_config);
    virtual void resize(int X, int Y, int W, int H);
    BlockView block;

    // If true, this is running from c++, not haskell.
    bool testing;
protected:
    int handle(int evt);
};

#endif
