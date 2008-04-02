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

BlockViewWindow *block_view_create(int x, int y, int w, int h,
        BlockModelConfig *model_config, BlockViewConfig *view_config,
        RulerConfig *ruler, Marklist *marklists, int nmarklists);
void block_view_destroy(BlockViewWindow *b);
void block_view_set_size(BlockViewWindow *b, int x, int y, int w, int h);
void block_view_get_size(BlockViewWindow *b, int *sz);
// unused because I store it in the Block now
// const BlockViewConfig *block_view_get_config(BlockViewWindow *b);
void block_view_set_config(BlockViewWindow *b, BlockViewConfig *config);
const ZoomInfo *block_view_get_zoom(const BlockViewWindow *b);
void block_view_set_zoom(BlockViewWindow *b, const ZoomInfo *zoom);
int block_view_get_track_scroll(BlockViewWindow *b);
void block_view_set_track_scroll(BlockViewWindow *b, int pixels);
const Selection *block_view_get_selection(const BlockViewWindow *b, int selnum);
void block_view_set_selection(BlockViewWindow *b, int selnum,
        const Selection *sel);

int block_view_get_track_width(BlockViewWindow *b, int at);
void block_view_set_track_width(BlockViewWindow *b, int at, int width);

// Ruler

// EventTrack

/*
EventTrackModelRef *event_track_model_new(Color *c);
void event_track_model_destroy(EventTrackModelRef *t);

// Return 0 if the insert failed (previous event overlaps) or remove failed
// (no event at that pos).
int event_track_model_insert_event(EventTrackModelRef *t, const TrackPos *pos,
        EventMarshal *em);
int event_track_model_remove_event(EventTrackModelRef *t, const TrackPos *pos);

*/

// debugging

const char *i_show_children(const Fl_Widget *w, int nlevels);

}
