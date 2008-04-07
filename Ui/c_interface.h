#include "config.h"
#include "MsgCollector.h"
#include "Block.h"
#include "Track.h"
#include "EventTrack.h"

extern "C" {

// UI Event

void initialize();
void ui_wait();
void ui_awake();
int take_ui_msgs(UiMsg **msgs);


// Block view

// Passing a RulerConfig is hard because it uses a vector.  vector is really
// convenient in c++, but to pass it from haskell I need this grody hack where
// I pass a partially constructed RulerConfig and then fill in the vector from
// the passed c array.
// This hack is also in block_view_insert_track.
BlockViewWindow *block_view_create(int x, int y, int w, int h,
        BlockModelConfig *model_config, BlockViewConfig *view_config,
        RulerConfig *ruler, Marklist *marklists, int nmarklists);
void block_view_destroy(BlockViewWindow *b, FinalizeCallback finalizer);

void block_view_set_size(BlockViewWindow *b, int x, int y, int w, int h);
void block_view_get_size(BlockViewWindow *b, int *sz);
void block_view_set_view_config(BlockViewWindow *b, BlockViewConfig *config);
void block_view_set_model_config(BlockViewWindow *b, BlockModelConfig *config);
void block_view_set_zoom(BlockViewWindow *b, const ZoomInfo *zoom);
void block_view_set_track_scroll(BlockViewWindow *b, int pixels);
void block_view_set_selection(BlockViewWindow *b, int selnum,
        const Selection *sel);
void block_view_set_track_width(BlockViewWindow *b, int tracknum, int width);

// tracks
void block_view_insert_track(BlockViewWindow *view, int tracknum,
        Tracklike *track, int width,
        Marklist *marklists, int nmarklists);
void block_view_remove_track(BlockViewWindow *view, int tracknum,
        FinalizeCallback finalizer);
void block_view_update_track(BlockViewWindow *view, int tracknum,
        Tracklike *track, Marklist *marklists, int nmarklists,
        FinalizeCallback finalizer, TrackPos *start, TrackPos *end);


// debugging

const char *i_show_children(const Fl_Widget *w, int nlevels);

}
