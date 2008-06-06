#ifndef __BLOCK_H
#define __BLOCK_H

/*
        block (Group) ---\
        /    \          status_line (Output)
    title   __ body __________________  (Tile)
          /                           \
ruler_group _________________        track_group
   |       \         \       \         |        \
track_box sb_box     time_sb ruler   track_sb  track_scroll
                                                /
                                            track_tile
                                            /
                                        track | ruler | divider, ...
                                       /    \
                              track_body    track_title
                         overlay_ruler
                         /
                     event, ...

also, there should be readouts for insertion point pos, zoom box,
block length, ...
these should be in both Trackpos units and relative to Mark units
(controllable from python)

scrolling:
on an scrollbar callback

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
#include "TrackTile.h"
#include "Ruler.h"
#include "Event.h"

struct BlockModelConfig {
    Color bg;
    Color track_box;
    Color sb_box;
};


struct BlockViewConfig {
    double zoom_speed;

    int block_title_height;
    int track_title_height;
    int sb_size;
    int ruler_size;
    int status_size;
};


class BlockView : public Fl_Group {
public:
    BlockView(int X, int Y, int W, int H,
            const BlockModelConfig &model_config,
            const BlockViewConfig &view_config,
            const Tracklike &ruler_track);

    void resize(int X, int Y, int W, int H);
    void set_view_config(const BlockViewConfig &view_config,
            bool always_update=false);
    void set_model_config(const BlockModelConfig &config,
            bool always_update=false);

    // Set the zoom, which is the view rectangle in the timewise direction.
    const ZoomInfo &get_zoom() const { return zoom; }
    void set_zoom(const ZoomInfo &zoom);
    // Get and set trackwise scrolling, in pixels.
    int get_track_scroll() const;
    void set_track_scroll(int offset);

    void set_selection(int selnum, const Selection &sel);

    void set_title(const char *s) { title.set_text(s); }
    const char *get_title() const { return title.value(); }
    void set_status(const char *s) { status_line.value(s); }

    // The track in the special non-scrolling ruler space is represented by
    // this tracknum.  'insert_track' on this tracknum replaces it, and
    // 'update_track' updates it.
    enum { ruler_tracknum = -1 };
    void insert_track(int tracknum, const Tracklike &track, int width);
    void remove_track(int tracknum, FinalizeCallback finalizer);

    // Update the given track.  Update scrollbars.
    // 'track' should be the same kind of track as the one at 'tracknum' or this
    // throws.  Update colors and whatnot if they have changed (pointers
    // may be passed as NULL which means no change).  Also mark that the range
    // 'start' to 'end' should be updated.  If 'end' is TrackPos(0), the entire
    // range should be updated.
    void update_track(int tracknum, const Tracklike &track,
            FinalizeCallback finalizer, TrackPos start, TrackPos end);

    TrackView *track_at(int tracknum) {
        if (tracknum == BlockView::ruler_tracknum)
            return ruler_track;
        else
            return track_tile.track_at(tracknum);
    }
    int tracks() const { return track_tile.tracks(); }
    int get_track_width(int tracknum) { track_tile.get_track_width(tracknum); }
    void set_track_width(int tracknum, int width) {
        track_tile.set_track_width(tracknum, width);
    }
    void drag_tile(Point drag_from, Point drag_to) {
        track_tile.drag_tile(drag_from, drag_to);
    }

private:
    BlockModelConfig model_config;
    BlockViewConfig view_config;
    ZoomInfo zoom;

    SeqInput title;
    Fl_Output status_line;
    Fl_Tile body;
        // Dummy group to limit body tile.
        Fl_Group body_resize_group;
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
            const BlockViewConfig &view_config,
            const Tracklike &ruler_track);
    virtual void resize(int X, int Y, int W, int H);
    BlockView block;

    // If true, this is running from c++, not haskell.
    bool testing;
protected:
    int handle(int evt);

private:
    // Keep track of which keys are down, to suppress spurious key ups.
    std::map<int, bool> keys_down;
};

#endif
