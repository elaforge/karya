#ifndef __BLOCK_H
#define __BLOCK_H

/*
        block (Group) ---\
        /    \          status_line (Output)
    title   __ body __________________  (Tile)
          /                           \
ruler_group _________________        track_group
   |       \         \       \         |        \
track_box sb_box     time_sb ruler   track_sb  track_zoom
                                                  /
                                             track_scroll
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
#include <boost/shared_ptr.hpp>

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
#include "Zoom.h"
#include "Ruler.h"
#include "Event.h"

// The track in the special non-scrolling ruler space is represented by this
// tracknum.  'add_track' on this tracknum replaces the ruler track, and
// 'remove_track' has no effect.
enum { ruler_tracknum = -1 };

struct BlockModelConfig {
    // An array of 3 or more Colors for the selections.
    Color select[Config::max_selections];
    Color bg;
    Color track_box;
    Color sb_box;
};

class BlockView;

class BlockModel {
public:
    BlockModel(const BlockModelConfig &config) :
        serial_number(current_serial_number++),
        config(config)
    {
        DEBUG("creating block model " << this->serial_number);
    }
    ~BlockModel();

    const char *get_title() const { return title.c_str(); }
    void set_title(const char *s);

    // TrackModels are passed and stored by value because a TrackModel is just
    // a union of pointers.
    void insert_track(int at, const TrackModel &track, int width);
    void remove_track(int at);
    std::pair<TrackModel, int> track_at(int at) const { return _tracks.at(at); }
    int tracks() const { return _tracks.size(); }

    const BlockModelConfig &get_config() const { return config; }
    void set_config(const BlockModelConfig &config);

    // Called by BlockView, to register itself with the model.
    void add_view(BlockView *view) { views.push_back(view); }
    void remove_view(BlockView *view) {
        views.erase(std::remove(views.begin(), views.end(), view), views.end());
    }

private:
    // Keep track of created models.
    static int current_serial_number;
    const int serial_number;

    std::string title;
    BlockModelConfig config;
    // All the BlockViews that point to this BlockModel.  BlockView
    // adds this when it's created so block modifications can notify its views.
    std::vector<BlockView *> views;
    std::vector<std::pair<TrackModel, int> > _tracks;
};


struct BlockViewConfig {
    Orientation orientation;
    double zoom_speed;

    int block_title_height;
    int track_title_height;
    int sb_size;
    int ruler_size;
    int status_size;
};


class BlockView : public Fl_Group {
public:
    BlockView(int X, int Y, int W, int H, boost::shared_ptr<BlockModel> model,
            boost::shared_ptr<const RulerTrackModel> ruler_model,
            const BlockViewConfig &config);
    ~BlockView();

    // fltk methods
    void resize(int X, int Y, int W, int H);

    // api methods
    // Set the zoom, which is the view rectangle in the timewise direction.
    const ZoomInfo &get_zoom() const { return zoom; }
    void set_zoom(const ZoomInfo &zoom);
    // Get and set trackwise scrolling, in pixels.
    int get_track_scroll() const;
    void set_track_scroll(int offset);

    // unused by api
    const BlockViewConfig &get_config() const { return config; }
    void set_config(const BlockViewConfig &config);
    const Selection &get_selection(int selnum) const;
    void set_selection(int selnum, const Selection &sel);

    // Called by BlockModel when it changes:
    void set_title(const char *s) { title.value(s); }
    const char *get_title() const { return title.value(); }
    void set_status(const char *s) { status_line.value(s); }

    void insert_track(int at, const TrackModel &track, int width);
    void remove_track(int at);
    // unused by api
    TrackView *track_at(int at) {
        return track_tile.track_at(at);
    }
    // unused by api
    int tracks() const { return track_tile.tracks(); }
    int get_track_width(int at) { track_tile.get_track_width(at); }
    void set_track_width(int at, int width) {
        track_tile.set_track_width(at, width);
    }

    void drag_tile(Point drag_from, Point drag_to) {
        track_tile.drag_tile(drag_from, drag_to);
    }
    void tile_init() { track_tile.init_sizes(); }

protected:
    int handle(int evt);

private:
    boost::shared_ptr<BlockModel> model;
    BlockViewConfig config;
    ZoomInfo zoom;
    std::vector<Selection> selections;

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
            RulerTrackView ruler;
        Fl_Group track_group;
            FlSeqScrollbar track_sb;
            // P9SeqScrollbar track_sb;
            Zoom track_zoom;
                SimpleScroll track_scroll;
                    TrackTile track_tile;

    void update_sizes();
    void update_colors();
    void update_scrollbars();

    // Called by scrollbar.
    static void scrollbar_cb(Fl_Widget *w, void *vp);
    static void update_scrollbars_cb(Fl_Widget *w, void *vp);
};


class BlockViewWindow : public Fl_Double_Window {
public:
    BlockViewWindow(int X, int Y, int W, int H,
            boost::shared_ptr<BlockModel> model,
            boost::shared_ptr<const RulerTrackModel> ruler_model,
            const BlockViewConfig &config);
    BlockView block;
};

#endif
