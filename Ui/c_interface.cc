/*
C procedural interface to the UI level.
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
ui_wait()
{
    // TODO remind myself and fix the comment
    Fl::wait(100);
}

void
ui_awake()
{
    DEBUG("awake");
    Fl::awake((void*) 0);
}

int
take_ui_msgs(UiMsg **msgs)
{
    // Turn vector into c-array.  C++ standard says vector is supposed to
    // use a contiguous array:
    // http://www.open-std.org/jtc1/sc22/wg21/docs/lwg-defects.html#69
    MsgCollector *m = global_msg_collector();
    *msgs = &m->msgs[0];
    int sz = m->msgs.size();
    m->msgs.clear();
    return sz;
}


// Block view

BlockViewWindow *
block_view_create(int x, int y, int w, int h, BlockModelConfig *model_config,
        BlockViewConfig *view_config, RulerConfig *partial_ruler,
        Marklist *marklists, int nmarklists)
{
    Marklists mlists;
    RulerConfig ruler(partial_ruler->bg, partial_ruler->show_names,
            partial_ruler->use_alpha, partial_ruler->full_width);
    for (int i = 0; i < nmarklists; i++)
        ruler.marklists.push_back(marklists[i]);
    BlockViewWindow *win = new BlockViewWindow(x, y, w, h, *model_config,
        *view_config, ruler);
    win->show();
    return win;
}

void
block_view_destroy(BlockViewWindow *b)
{
    delete b;
}

void
block_view_set_size(BlockViewWindow *b, int x, int y, int w, int h)
{
    b->resize(x, y, w, h);
}

void
block_view_get_size(BlockViewWindow *b, int *sz)
{
    sz[0] = b->x();
    sz[1] = b->y();
    sz[2] = b->w();
    sz[3] = b->h();
}

void
block_view_set_view_config(BlockViewWindow *b, BlockViewConfig *config)
{
    b->block.set_view_config(*config);
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

int
block_view_get_track_scroll(BlockViewWindow *b)
{
    return b->block.get_track_scroll();
}

void
block_view_set_track_scroll(BlockViewWindow *b, int pixels)
{
    b->block.set_track_scroll(pixels);
}

const Selection *
block_view_get_selection(const BlockViewWindow *b, int selnum)
{
    return &b->block.get_selection(selnum);
}

void
block_view_set_selection(BlockViewWindow *b, int selnum, const Selection *sel)
{
    b->block.set_selection(selnum, *sel);
}

int
block_view_get_track_width(BlockViewWindow *b, int at)
{
    return b->block.get_track_width(at);
}

void
block_view_set_track_width(BlockViewWindow *b, int at, int width)
{
    b->block.set_track_width(at, width);
}


// tracks

void
block_view_insert_track(BlockViewWindow *view, int tracknum,
        Tracklike *track, int width,
        Marklist *marklists, int nmarklists)
{
    RulerConfig *old_ruler = track->ruler;
    if (track->ruler) {
        // Substitute a complete ruler for the semi-constructed one.
        RulerConfig &partial = *track->ruler;
        RulerConfig config(partial.bg, partial.show_names, partial.use_alpha,
                partial.full_width);
        for (int i = 0; i < nmarklists; i++)
            config.marklists.push_back(marklists[i]);
        track->ruler = &config;
    }
    view->block.insert_track(tracknum, *track, width);
    if (track->ruler) {
        // Put it back the way I found it.
        track->ruler = old_ruler;
    }
}

void
block_view_remove_track(BlockViewWindow *view, int tracknum)
{
    view->block.remove_track(tracknum);
}

// Ruler


// Event

/*
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

int
event_track_model_insert_event(EventTrackModelRef *t, const TrackPos *pos,
        EventMarshal *em)
{
    EventModel e(em->text, em->duration, em->color, em->style,
            em->align_to_bottom);
    return (*t)->insert_event(*pos, e);
}

int
event_track_model_remove_event(EventTrackModelRef *t, const TrackPos *pos)
{
    return (*t)->remove_event(*pos);
}
*/


// debugging

const char *
i_show_children(const Fl_Widget *w, int nlevels)
{
    show_children(w, nlevels, 0);
}

}
