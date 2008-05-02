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
    // DEBUG("lock");
    Fl::lock();
}

void
ui_wait()
{
    // TODO remind myself why this is 100 and comment
    Fl::wait(100);
}

void
ui_awake()
{
    // DEBUG("awake");
    Fl::awake((void*) 0);
}

int
get_ui_msgs(UiMsg **msgs)
{
    // Turn vector into c-array.  C++ standard says vector is supposed to
    // use a contiguous array:
    // http://www.open-std.org/jtc1/sc22/wg21/docs/lwg-defects.html#69
    MsgCollector *m = global_msg_collector();
    *msgs = m->msgs_ptr();
    return m->msgs_size();
}

void
clear_ui_msgs()
{
    global_msg_collector()->clear();
}


// Block view

BlockViewWindow *
create(int x, int y, int w, int h, BlockModelConfig *model_config,
        BlockViewConfig *view_config,
        Tracklike *ruler_track, Marklist *marklists, int nmarklists)
{
    // This is basically a copy and paste of insert_track.
    Tracklike *track = ruler_track;
    RulerConfig *old_ruler = track->ruler;
    BlockViewWindow *win = 0;
    if (track->ruler) {
        // Substitute a complete ruler for the semi-constructed one.
        RulerConfig &partial = *track->ruler;
        RulerConfig config(partial.bg, partial.show_names, partial.use_alpha,
                partial.full_width);
        for (int i = 0; i < nmarklists; i++)
            config.marklists.push_back(marklists[i]);
        track->ruler = &config;
        // Pass 'track' while 'config' is still in scope.
        win = new BlockViewWindow(x, y, w, h, *model_config,
                *view_config, *track);
        // Don't leave it pointing to out of scope data.
        track->ruler = old_ruler;
    } else {
        win = new BlockViewWindow(x, y, w, h, *model_config,
                *view_config, *track);
    }
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
    if (sel)
        view->block.set_selection(selnum, *sel);
    else
        view->block.set_selection(selnum, Selection());
}


// block

void
set_model_config(BlockViewWindow *view, BlockModelConfig *config)
{
    view->block.set_model_config(*config);
}

void
set_title(BlockViewWindow *view, const char *title)
{
    view->block.set_title(title);
}

void
set_status(BlockViewWindow *view, const char *status)
{
    view->block.set_status(status);
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
        view->block.insert_track(tracknum, *track, width);
        // Don't leave it pointing to out of scope data.
        track->ruler = old_ruler;
    } else {
        view->block.insert_track(tracknum, *track, width);
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

void
set_track_title(BlockViewWindow *view, int tracknum, const char *title)
{
    view->block.track_at(tracknum)->set_title(title);
}


// debugging

const char *
i_show_children(const BlockViewWindow *w, int nlevels)
{
    show_children(w, nlevels, 0);
}

}
