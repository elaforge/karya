#include "Block.h"
#include "Track.h"
#include "EventTrack.h"

typedef boost::shared_ptr<BlockModel> BlockModelRef;
typedef boost::shared_ptr<EventTrackModel> EventTrackModelRef;
typedef boost::shared_ptr<RulerTrackModel> RulerTrackModelRef;

typedef boost::shared_ptr<Marklist> MarklistRef;

extern "C" {

BlockModelRef *block_model_create(const BlockModelConfig *color_config);
void block_model_destroy(BlockModelRef *b);
const char *block_model_get_title(const BlockModelRef *b);
void block_model_set_title(BlockModelRef *b, const char *s);

void block_model_insert_event_track(BlockModelRef *b, int at, int width,
        EventTrackModelRef *t, RulerTrackModelRef *r);
void block_model_insert_ruler_track(BlockModelRef *b, int at, int width,
        RulerTrackModelRef *r);
void block_model_insert_divider(BlockModelRef *b, int at, int width,
        Color *color);

void block_model_remove_track(BlockModelRef *b, int at);

// void block_model_track_at(BlockModelRef *b, int at);

// Ruler

RulerTrackModelRef *ruler_track_model_new(Color *bg, int mlists,
        MarklistRef **marklists);
void ruler_track_model_destroy(RulerTrackModelRef *r);

// A struct version of Mark, used to pass a marklist from haskell to C all
// in one go.  The duplication with Mark is unfortunate.
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

// Event

EventTrackModelRef *event_track_model_new();
void event_track_model_destroy(EventTrackModelRef *t);

}
