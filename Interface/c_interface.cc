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

// UI Event

void
initialize()
{
    DEBUG("lock");
    Fl::lock();
}

void
ui_msg_wait()
{
    Fl::wait(100);
}

void
ui_msg_awake()
{
    DEBUG("awake");
    Fl::awake((void*) 0);
}

int
take_ui_msgs(UiEvent **msgs)
{
    // fltk calls these events but I call them msgs
    static UiEvent msg_queue[1024];
    std::vector<UiEvent> &events = global_event_collector()->events;
    // Apparently std::copy_n doesn't exist any more?
    int i = 0;
    for (; i <  events.size() && i < sizeof msg_queue; i++)
        msg_queue[i] = events[i];
    if (events.size() > sizeof msg_queue) {
        DEBUG("msg overflow, lost " << events.size() - sizeof msg_queue);
    }
    events.clear();
    *msgs = msg_queue;
    return i;
}



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

const BlockModelConfig *
block_model_get_config(BlockModelRef *b)
{
    return &(*b)->get_config();
}

void
block_model_set_config(BlockModelRef *b, BlockModelConfig *config)
{
    (*b)->set_config(*config);
}

const char *
block_model_get_title(const BlockModelRef *b)
{
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
    DEBUG("insert ruler at " << at);
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

// Block view

BlockViewWindow *
block_view_create(int x, int y, int w, int h, BlockModelRef *model,
        RulerTrackModelRef *r, BlockViewConfig *view_config)
{
    BlockViewConfig c(*view_config);
    // This isn't implemented currently.
    c.orientation = VerticalTime;

    BlockViewWindow *win = new BlockViewWindow(x, y, w, h, *model, *r, c);
    win->show();
    DEBUG("create and show window");
    return win;
}

void
block_view_destroy(BlockViewWindow *b)
{
    delete b;
}

void
block_view_resize(BlockViewWindow *b, int x, int y, int w, int h)
{
    b->resize(x, y, w, h);
}

const BlockViewConfig *
block_view_get_config(BlockViewWindow *b)
{
    return &b->block.get_config();
}

void
block_view_set_config(BlockViewWindow *b, BlockViewConfig *config)
{
    b->block.set_config(*config);
}

const ZoomInfo *
block_view_get_zoom(const BlockViewWindow *b)
{
    return &b->block.get_zoom();
}

void
block_view_set_zoom(BlockViewWindow *b, const ZoomInfo *zoom)
{
    b->block.set_zoom(*zoom);
}

const Selection *
block_view_get_selection(const BlockViewWindow *b)
{
    return &b->block.get_selection();
}

void
block_view_set_selection(BlockViewWindow *b, const Selection *sel)
{
    b->block.set_selection(*sel);
}



// Ruler

RulerTrackModelRef *
ruler_track_model_new(Color *bg, int mlists, MarklistRef **marklists,
        bool show_names, bool use_alpha, bool full_width)
{
    Marklists lists;
    for (int i = 0; i < mlists; i++) {
        lists.push_back(**marklists);
        (*marklists)++;
    }
    return new RulerTrackModelRef(new RulerTrackModel(lists, *bg,
            show_names, use_alpha, full_width));
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
event_track_model_new(Color *c)
{
    return new EventTrackModelRef(new EventTrackModel(*c));
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

// debugging

const char *
i_show_children(const Fl_Widget *w, int nlevels)
{
    show_children(w, nlevels, 0);
}

}
