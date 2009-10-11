#include "config.h"
#include "MsgCollector.h"
#include "Block.h"
#include "Track.h"
#include "EventTrack.h"
#include "SkeletonDisplay.h"

extern "C" {

// UI Event

void initialize();
void ui_wait();
void ui_awake();
int get_ui_msgs(UiMsg **msgs);
void clear_ui_msgs();


// Block view

// Passing a RulerConfig is hard because it uses a vector.  vector is really
// convenient in c++, but to pass it from haskell I need this grody hack where
// I pass a partially constructed RulerConfig and then fill in the vector from
// the passed c array.
// This hack is also in insert_track.
BlockViewWindow *create(int x, int y, int w, int h, const char *label,
        BlockModelConfig *model_config, BlockViewConfig *view_config);
void destroy(BlockViewWindow *view, FinalizeCallback finalizer);

void set_size(BlockViewWindow *view, int x, int y, int w, int h);
void get_size(BlockViewWindow *view, int *sz);
void set_view_config(BlockViewWindow *view, BlockViewConfig *config);
void set_zoom(BlockViewWindow *view, const ZoomInfo *zoom);
void set_track_scroll(BlockViewWindow *view, int pixels);
void set_selection(BlockViewWindow *view, int selnum,
        const Selection *sel);
void set_track_selection(BlockViewWindow *view, int selnum, int tracknum,
        const Selection *sel);

void set_model_config(BlockViewWindow *view, BlockModelConfig *config);
void set_skeleton(BlockViewWindow *view, SkeletonConfig *skel);
void set_title(BlockViewWindow *view, const char *title);
void set_status(BlockViewWindow *view, const char *status);
void set_display_track(BlockViewWindow *view, int tracknum,
        DisplayTrack *dtrack);
void collapse_track(BlockViewWindow *view, int tracknum, char collapse);

// tracks

void insert_track(BlockViewWindow *view, int tracknum,
        Tracklike *track, int width,
        Marklist *marklists, int nmarklists);
void remove_track(BlockViewWindow *view, int tracknum,
        FinalizeCallback finalizer);
void update_track(BlockViewWindow *view, int tracknum,
        Tracklike *track, Marklist *marklists, int nmarklists,
        FinalizeCallback finalizer, TrackPos *start, TrackPos *end);
void set_track_width(BlockViewWindow *view, int tracknum, int width);
void set_track_title(BlockViewWindow *view, int tracknum, const char *title);

// debugging

const char *i_show_children(const BlockViewWindow *w, int nlevels);

}
