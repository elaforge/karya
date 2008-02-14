/*
C procedural interface to the UI level.

Memory management:
Blocks and tracks are kept alive via ref counts.  They are also passed out
to haskell for inspection.  Either:

Interface passes out BlockId and TrackId.  It maintains a map from IDs to
the objects, and can have an operation return Nothing if the ID has been
deallocated.  This means that blocks and tracks must be managed manually and
can leak.

Interface increfs and passes out block and track foreign pointers, with
a finalizer to decref.  Because views represt windows and must be destroyed
deterministically, it passes out IDs for BlockViews and TrackViews.

Small objects are returned as copies marshalled into haskell data structures.

Ref counted:
BlockModel, TrackModel, RulerModel

Explicitly destroyed:
BlockView, TrackView, RulerView

Marshalled:
Divider
BlockModelConfig, BlockViewConfig, TrackModelConfig, TrackViewConfig
RulerMarklist, RuleMark
*/

#include <utility>
#include <boost/shared_ptr.hpp>
#include "c_interface.h"
#include "util.h"

extern "C" {

BlockModelRef *
block_model_create(const BlockModelConfig *config)
{
    DEBUG("new block model");
    return new BlockModelRef(new BlockModel(*config));
}

void
block_model_destroy(BlockModelRef *b)
{
    DEBUG("destroy model");
    delete b;
}

const char *
block_model_get_title(const BlockModelRef *b)
{
    DEBUG("get title");
    return (*b)->get_title();
}

void
block_model_set_title(BlockModelRef *b, const char *s)
{
    DEBUG("set title " << s);
    (*b)->set_title(s);
}

void
block_model_insert_event_track(BlockModelRef *b, int at, int width,
        EventTrackModelRef *t, RulerTrackModelRef *r)
{
    (*b)->insert_track(at, TrackModel(*t, *r), width);
}

void
block_model_insert_ruler_track(BlockModelRef *b, int at, int width,
        RulerTrackModelRef *r)
{
    (*b)->insert_track(at, TrackModel(*r), width);
}

void
block_model_insert_divider(BlockModelRef *b, int at, int width, Color *color)
{
    boost::shared_ptr<DividerModel> d(new DividerModel(*color));
    (*b)->insert_track(at, TrackModel(d), width);
}

void
block_model_remove_track(BlockModelRef *b, int at)
{
    (*b)->remove_track(at);
}


// rulers

RulerTrackModelRef *
ruler_track_model_new(Color *bg, int mlists, MarklistRef **marklists)
{
    Marklists lists;
    for (int i = 0; i < mlists; i++) {
        lists.push_back(**marklists);
        (*marklists)++;
    }
    return new RulerTrackModelRef(new RulerTrackModel(lists, *bg));
}

void
ruler_track_model_destroy(RulerTrackModelRef *r)
{
    delete r;
}

// marklists

// 'marklist_new' is responsible for freeing all storage in 'marks'.
MarklistRef *
marklist_new(int len, MarkMarshal *marks)
{
    boost::shared_ptr<Marklist> mlist(new Marklist());
    mlist->reserve(len);
    MarkMarshal *m = marks;
    for (int i = 0; i < len; i++, m++) {
        Mark mark(m->rank, m->width, m->color, m->name, m->name_zoom_level,
                m->zoom_level);
        mlist->push_back(std::make_pair(m->pos, mark));
        delete m->name;
    }
    delete marks;
    return new MarklistRef(mlist);
}

void
marklist_destroy(MarklistRef *m)
{
    DEBUG("destroy marklist");
    delete m;
}


// Event

EventTrackModelRef *
event_track_model_new()
{
    return new EventTrackModelRef(new EventTrackModel());
}

void
event_track_model_destroy(EventTrackModelRef *t)
{
    delete t;
}


/*
create_block_view
destroy_block_view
show_block_view
hide_block_view

*/

}
