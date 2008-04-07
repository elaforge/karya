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
create(int x, int y, int w, int h, BlockModelConfig *model_config,
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
destroy(BlockViewWindow *view, FinalizeCallback finalizer)
{
    // Make sure all the callbacks are finalized.
    for (int i = view->block.tracks() - 1; i; i--)
        view->block.remove_track(i, finalizer);
    delete view;
}

void
set_size(BlockViewWindow *view, int x, int y, int w, int h)
{
    view->resize(x, y, w, h);
}

void
get_size(BlockViewWindow *view, int *sz)
{
    sz[0] = view->x();
    sz[1] = view->y();
    sz[2] = view->w();
    sz[3] = view->h();
}

void
set_view_config(BlockViewWindow *view, BlockViewConfig *config)
{
    view->block.set_view_config(*config);
}

void
set_model_config(BlockViewWindow *view, BlockModelConfig *config)
{
    view->block.set_model_config(*config);
}

void
set_zoom(BlockViewWindow *view, const ZoomInfo *zoom)
{
    view->block.set_zoom(*zoom);
}

void
set_track_scroll(BlockViewWindow *view, int pixels)
{
    view->block.set_track_scroll(pixels);
}

void
set_selection(BlockViewWindow *view, int selnum, const Selection *sel)
{
    view->block.set_selection(selnum, *sel);
}


// block

void
set_title(BlockViewWindow *view, char *title)
{
    view->block.set_title(title);
}


// tracks

void
insert_track(BlockViewWindow *view, int tracknum,
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
remove_track(BlockViewWindow *view, int tracknum,
        FinalizeCallback finalizer)
{
    view->block.remove_track(tracknum, finalizer);
}

void
update_track(BlockViewWindow *view, int tracknum,
        Tracklike *track, Marklist *marklists, int nmarklists,
        FinalizeCallback finalizer, TrackPos *start, TrackPos *end)
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
    view->block.update_track(tracknum, *track, finalizer, *start, *end);
    if (track->ruler) {
        // Put it back the way I found it.
        track->ruler = old_ruler;
    }
}

void
set_track_width(BlockViewWindow *view, int tracknum, int width)
{
    view->block.set_track_width(tracknum, width);
}


// debugging

const char *
i_show_children(const BlockViewWindow *w, int nlevels)
{
    show_children(w, nlevels, 0);
}

}
