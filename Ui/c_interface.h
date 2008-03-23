#include "config.h"
#include "MsgCollector.h"
#include "Block.h"
#include "Track.h"
#include "EventTrack.h"

typedef boost::shared_ptr<BlockModel> BlockModelRef;
typedef boost::shared_ptr<EventTrackModel> EventTrackModelRef;
typedef boost::shared_ptr<RulerTrackModel> RulerTrackModelRef;
typedef boost::shared_ptr<Marklist> MarklistRef;

typedef BlockViewWindow BlockViewRef;

extern "C" {

// UI Event

void initialize();
void ui_wait();
void ui_awake();
int take_ui_msgs(UiMsg **msgs);


// Block

BlockModelRef *block_model_create(const BlockModelConfig *config);
void block_model_destroy(BlockModelRef *b);
// unused because I store it in the Block now
// const BlockModelConfig *block_model_get_config(BlockModelRef *b);
void block_model_set_config(BlockModelRef *b, BlockModelConfig *config);
const char *block_model_get_title(const BlockModelRef *b);
void block_model_set_title(BlockModelRef *b, const char *s);

void block_model_insert_event_track(BlockModelRef *b, int at, int width,
        EventTrackModelRef *t, RulerTrackModelRef *r);
void block_model_insert_ruler_track(BlockModelRef *b, int at, int width,
        RulerTrackModelRef *r);
void block_model_insert_divider(BlockModelRef *b, int at, int width,
        Color *color);

void block_model_remove_track(BlockModelRef *b, int at);

// Block view

BlockViewWindow *block_view_create(int x, int y, int w, int h,
        BlockModelRef *model, RulerTrackModelRef *r,
        BlockViewConfig *view_config);
void block_view_destroy(BlockViewWindow *b);
void block_view_resize(BlockViewWindow *b, int x, int y, int w, int h);
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

RulerTrackModelRef *ruler_track_model_new(Color *bg, int mlists,
        MarklistRef **marklists,
        bool show_names, bool use_alpha, bool full_width);
void ruler_track_model_destroy(RulerTrackModelRef *r);

// A struct version of Mark, used to pass a marklist from haskell to C all
// in one go.  The duplication with Mark is unfortunate.
// I do it so I can pass (pos, mark) pairs, and also I can only pass char*, not
// std::string and c++ strings deallocate themselves.  I could use a
// scoped_ptr<char *>, but I'd still have the (pos, mark) problem.
struct MarkMarshal {
    TrackPos pos;
    int rank;
    int width;
    Color color;
    char *name;
    double name_zoom_level;
    double zoom_level;
};

MarklistRef *marklist_new(int len, MarkMarshal *marks);
void marklist_destroy(MarklistRef *m);

// EventTrack

struct EventMarshal {
    char *text;
    TrackPos duration;
    Color color;
    TextStyle style;
    bool align_to_bottom;
};

EventTrackModelRef *event_track_model_new(Color *c);
void event_track_model_destroy(EventTrackModelRef *t);

// Return 0 if the insert failed (previous event overlaps) or remove failed
// (no event at that pos).
int event_track_model_insert_event(EventTrackModelRef *t, const TrackPos *pos,
        EventMarshal *em);
int event_track_model_remove_event(EventTrackModelRef *t, const TrackPos *pos);


// debugging

const char *i_show_children(const Fl_Widget *w, int nlevels);

}
