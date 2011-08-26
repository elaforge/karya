/*
C procedural interface to the UI level.
*/

#include <utility>
#include "c_interface.h"
#include "util.h"

extern "C" {

// UI Event

void
initialize()
{
    // DEBUG("lock");
    Fl::lock();
    MsgCollector::get()->screen_update();
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
    MsgCollector *m = MsgCollector::get();
    *msgs = m->msgs_ptr();
    return m->msgs_size();
}

void
clear_ui_msgs()
{
    MsgCollector::get()->clear();
}


// Block view

BlockViewWindow *
create(int x, int y, int w, int h, const char *label,
        BlockModelConfig *model_config, BlockViewConfig *view_config)
{
    BlockViewWindow *win = new BlockViewWindow(
            x, y, w, h, label, *model_config, *view_config);
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


void
set_track_selection(BlockViewWindow *view, int selnum, int tracknum,
        const Selection *sel)
{
    // This function is the only one which is called asynchronously from
    // the usual diff->sync rigamorale.  So if a track is deleted while
    // the playback thread is calling this, the tracknum will be incorrect.
    // It's not worth fixing for real, but at least I can not crash.
    tracknum = std::min(view->block.tracks() - 1, tracknum);
    if (sel)
        view->block.set_track_selection(selnum, tracknum, *sel);
    else
        view->block.set_track_selection(selnum, tracknum, Selection());
}


void bring_to_front(BlockViewWindow *view)
{
    view->show();
}


// block

void
set_model_config(BlockViewWindow *view, BlockModelConfig *config)
{
    view->block.set_model_config(*config);
}

void
set_skeleton(BlockViewWindow *view, SkeletonConfig *skel)
{
    view->block.set_skeleton(*skel);
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

void
set_display_track(BlockViewWindow *view, int tracknum,
        DisplayTrack *dtrack)
{
    view->block.set_display_track(tracknum, *dtrack);
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
                partial.full_width, partial.align_to_bottom,
                partial.last_mark_pos);
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
        FinalizeCallback finalizer, ScoreTime *start, ScoreTime *end)
{
    RulerConfig *old_ruler = track->ruler;
    if (track->ruler) {
        // Substitute a complete ruler for the semi-constructed one.
        RulerConfig &partial = *track->ruler;
        RulerConfig config(partial.bg, partial.show_names, partial.use_alpha,
                partial.full_width, partial.align_to_bottom,
                partial.last_mark_pos);
        for (int i = 0; i < nmarklists; i++)
            config.marklists.push_back(marklists[i]);
        track->ruler = &config;
        view->block.update_track(tracknum, *track, finalizer, *start, *end);
        // Don't leave it pointing to out of scope data.
        track->ruler = old_ruler;
    } else {
        view->block.update_track(tracknum, *track, finalizer, *start, *end);
    }
}

void
set_track_signal(BlockViewWindow *view, int tracknum,
        const TrackSignal *tsig)
{
    view->block.set_track_signal(tracknum, *tsig);
}

void
set_track_title(BlockViewWindow *view, int tracknum, const char *title)
{
    view->block.track_at(tracknum)->set_title(title);
}


// symbols

void
insert_symbol(const char *name, int absolute_y,
    const SymbolTable::Glyph *glyphs, int glyphs_len)
{
    SymbolTable::Symbol sym;
    sym.absolute_y = absolute_y;
    for (int i = 0; i < glyphs_len; i++) {
        sym.glyphs.push_back(glyphs[i]);
    }
    SymbolTable::get()->insert(string(strdup(name)), sym);
}


int // Font is Fl_Font, which is just int
get_font(const char *name)
{
    return SymbolTable::get()->font(name);
}

char **
get_fonts()
{
    return SymbolTable::get()->fonts();
}

// styles

void
insert_style(StyleId id, EventStyle *style)
{
    StyleTable::get()->put(id, *style);
}


// debugging

const char *
i_show_children(const BlockViewWindow *w, int nlevels)
{
    return show_children(w, nlevels, 0);
}

const char *
dump_view(const BlockViewWindow *view)
{
    return view->block.dump();
}

}
