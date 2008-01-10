#ifndef __BLOCK_H
#define __BLOCK_H

/*
        block (Group)
        /    \
    title   __ body __________________  (Tile)
          /                           \
ruler_group _________________        track_group
   |       \         \       \         |        \
track_box sb_box     time_sb ruler   track_sb  track_zoom
                                                /
                                            track_tile
                                            /
                                        track | ruler | divider, ...
                                       /    \
                              track_body    track_title
                         transparent_ruler
                         /
                     event, ...

also, there should be readouts for insertion point pos, zoom box,
block length, ...
these should be in both Trackpos units and relative to Mark units
(controllable from python)

bugs:
scrollbars don't resize properly when the Block is resized, in fact, no one
seems to realize the Block has changed sizes

*/

#include <vector>
#include <algorithm>

#include <FL/Fl.H>
#include <FL/Fl_Group.H>
#include <FL/Fl_Tile.H>
#include <FL/Fl_Box.H>
#include <FL/Fl_Double_Window.H>

#include "SeqScrollbar.h"
#include "SeqInput.h"

#include "types.h"
#include "Track.h"
#include "TrackTile.h"
#include "Zoom.h"
#include "Ruler.h"

// The track in the special non-scrolling ruler space is represented by this
// tracknum.  'add_track' on this tracknum replaces the ruler track, and
// 'remove_track' has no effect.
enum { ruler_tracknum = -1 };

struct BlockColorConfig {
    std::vector<Color> select;
    Color bg;
    Color track_box;
    Color sb_box;
};

class BlockView;

class BlockModel {
public:
    BlockModel(const BlockColorConfig &color_config) :
        color_config(color_config) {}

    const char *get_title() const { return title; }
    void set_title(const char *s);

    void insert_track(int at, const TrackModel &track, int width);
    void remove_track(int at);
    const TrackModel track_at(int at) const { return tracks.at(at); }

    const BlockColorConfig &get_color_config() const { return color_config; }
    void set_color_config(const BlockColorConfig &color_config);

    // Called by BlockView, to register itself with the model.
    void add_view(BlockView *view) { views.push_back(view); }
    void remove_view(BlockView *view) {
        remove(views.begin(), views.end(), view);
    }

private:
    const char *title;
    BlockColorConfig color_config;
    // All the BlockViews that point to this BlockModel.  BlockView
    // adds this when it's created so block modifications can notify its views.
    std::vector<BlockView *> views;
    std::vector<TrackModel> tracks;
};


struct BlockConfig {
    Orientation orientation;
    double zoom_speed;

    int title_size;
    int sb_size;
    int ruler_size;
};


class BlockView : public Fl_Group {
public:
    BlockView(int X, int Y, int W, int H, BlockModel &model,
            const BlockConfig &config);
    ~BlockView();

    // fltk methods
    void resize(int X, int Y, int W, int H);
    void redraw();

    // api methods
    const ZoomInfo &get_zoom() const { return this->zoom_info; }
    void set_zoom(const ZoomInfo &zoom);
    const BlockConfig &get_config() const { return this->config; }
    void set_config(const BlockConfig &config);
    const Selection &get_selection() const;
    void set_selection(int selnum, const Selection &sel);

    // Called by BlockModel when it changes:
    void set_title(const char *s);

    void insert_track(int at, const TrackModel &track, int width);
    void remove_track(int at);

protected:
    int handle(int evt);

private:
    BlockModel &model;
    BlockConfig config;
    ZoomInfo zoom_info;

    SeqInput title;
    Fl_Tile body;
        // Dummy group to limit body tile.
        Fl_Group body_resize_group;
        Fl_Group ruler_group;
            Fl_Box track_box;
            Fl_Box sb_box;
            SeqScrollbar time_sb;
            RulerTrackView ruler;
        Fl_Group track_group;
            SeqScrollbar track_sb;
            Zoom track_zoom;
                TrackTile track_tile;

    void update_sizes();
    void update_colors();
};


class BlockViewWindow : public Fl_Double_Window {
public:
    BlockViewWindow(int X, int Y, int W, int H, BlockModel &model,
            const BlockConfig &config) :
        Fl_Double_Window(X, Y, W, H),
        block(X, Y, W, H, model, config)
    {
        resizable(this);
    }
private:
    BlockView block;
};

#endif
