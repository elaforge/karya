// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

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

#include "Event.h"
#include "EventTrack.h"
#include "Ruler.h"
#include "SeqScrollbar.h"
#include "SimpleScroll.h"
#include "SkeletonDisplay.h"
#include "SymbolOutput.h"
#include "Track.h"
#include "TrackTile.h"
#include "WrappedInput.h"
#include "config.h"
#include "types.h"
#include "utf8.h"


struct BlockBox {
    BlockBox() : color(), c() {}
    BlockBox(Color color, char c) : color(color), c(c) {}
    bool operator==(const BlockBox &that) const {
        return color == that.color && c == that.c;
    }
    bool operator!=(const BlockBox &that) const { return !(*this == that); }
    Color color;
    utf8::rune c;
};

struct BlockConfig {
    BlockBox skel_box;
    BlockBox track_box;
    BlockBox sb_box;
};


// Track config local to each Block.
struct DisplayTrack {
    double event_brightness;
    int width;
    // These are also in SkeletonStatus.  The duplication is unfortunate, and
    // the reasons for it are documented in Ui.Update.Block.
    Color status_color;
    utf8::rune status1, status2;
};


// The ruler track is a special track that doesn't scroll with set_track_scroll
// like the other tracks do.  There is always just one ruler track and it's
// at tracknum 0.  It ignores remove_track, but will be replaced by
// insert_track.
class Block : public Fl_Group {
public:
    Block(int X, int Y, int W, int H,
        const BlockConfig &config, const char *window_title);
    virtual ~Block();

    int handle(int evt) override;
    void resize(int X, int Y, int W, int H) override;
private:
    void set_widget_sizes();
public:
    void set_config(const BlockConfig &config, bool update_all=false);
    void set_skeleton(const SkeletonConfig &skel);
    void set_skeleton_display_bg(const Color &color);

    // Set the zoom, which is the view rectangle in the timewise direction.
    const ZoomInfo &get_zoom() const { return zoom; }
    void set_zoom(const ZoomInfo &zoom);
    // Get and set trackwise scrolling, in pixels.
    int get_track_scroll() const;
    void set_track_scroll(int offset);
    // Get the pixels devoted to non-track overhead like scrollbars, in the
    // (track, time) dimensions.
    IPoint get_padding() const;

    void set_selection(
        int selnum, int tracknum, const std::vector<Selection> &sels);
    void set_title(const char *s);
    void set_title_focus() { title.take_focus(); }
    const char *get_title() const { return title.get_text(); }
    void set_status(const char *s, const Color &color) {
        status_line.value(s);
        status_line.color(color.fl());
    }

    void insert_track(int tracknum, const Tracklike &track, int width);
    void remove_track(int tracknum);
    void set_display_track(int tracknum, const DisplayTrack &dtrack);

private:
    void insert_track_view(int tracknum, Track *track, int width);
    Track *replace_ruler_track(Track *track, int width);
public:
    // Update the given track.  For simplicity, there's only one way to update
    // a track and that's to update everything in it.  This is practical
    // because updates are not super common and track data is fetched via
    // callback so there's little data to copy over.
    //
    // 'track' should be the same kind of track as the one at 'tracknum' or
    // this throws.  Update colors and whatnot if they have changed (pointers
    // may be passed as NULL which means no change).  Also mark that the range
    // 'start' to 'end' should be updated.  If 'end' is ScoreTime(0), the
    // entire range should be updated.
    void update_track(int tracknum, const Tracklike &track,
        ScoreTime start, ScoreTime end);

    // Update the signal for this track.
    void set_track_signal(int tracknum, const TrackSignal &tsig);

    Track *track_at(int tracknum) {
        if (tracknum == 0)
            return ruler_track;
        else
            return track_tile.track_at(tracknum-1);
    }
    const Track *track_at(int tracknum) const {
        if (tracknum == 0)
            return ruler_track;
        else
            return track_tile.track_at(tracknum-1);
    }
    int tracks() const { return track_tile.tracks() + 1; }
    int get_track_width(int tracknum) const;
    void set_track_width(int tracknum, int width);

    // Documented at TrackTile::floating_open.
    void floating_open(int tracknum, ScoreTime pos, const char *text,
        int select_start, int select_end);
    void floating_insert(const char *text);

    const char *dump() const;
    // Just so the MsgCollector can know if the mouse is in a track.
    int status_top() const { return status_line.y(); }

    bool event_in_track_area() const {
        return Fl::event_inside(&track_scroll);
    }
private:
    BlockConfig config;
    ZoomInfo zoom;
    // The ruler track gets this when there's "nothing" in it.
    Track *no_ruler;

    WrappedInput title;
    SymbolOutput status_line;
    Fl_Tile body;
        // Dummy group to limit body tile drag to the ruler track.
        Fl_Group body_resize_group;
        Fl_Box skel_box;
        SimpleScroll skel_display_scroll;
            SkeletonDisplay skel_display;
        Fl_Group ruler_group;
            Fl_Box track_box;
            Fl_Box sb_box;
            // FlSeqScrollbar time_sb;
            P9SeqScrollbar time_sb;
            // This can be replaced.
            Track *ruler_track;
        Fl_Group track_group;
            // FlSeqScrollbar track_sb;
            P9SeqScrollbar track_sb;
            SimpleScroll track_scroll;
                TrackTile track_tile;

    void set_zoom_attr(const ZoomInfo &zoom);
    void set_ruler_width(int width);
    void update_scrollbars();

    // Called by scrollbar.
    static void scrollbar_cb(Fl_Widget *w, void *vp);
    static void update_scrollbars_cb(Fl_Widget *w, void *vp);
    static void track_tile_cb(Fl_Widget *w, void *vp);
    static void title_cb_dispatch(Fl_Widget *w, void *vp);
    void title_cb();
};

class BlockWindow : public Fl_Double_Window {
public:
    BlockWindow(int X, int Y, int W, int H,
        const char *label, const BlockConfig &config);
    void resize(int X, int Y, int W, int H) override;
    Block block;

    // If true, this is running from c++, not haskell.
    bool testing;

    // This should be called once at app startup to do things that need to be
    // done once.
    static void initialize(Config::FreeHaskellFunPtr free_haskell_fun_ptr);
protected:
    int handle(int evt) override;
};

#endif
