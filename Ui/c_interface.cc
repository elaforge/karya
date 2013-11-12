// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

/*
C procedural interface to the UI level.
*/

#include <utility>
#include "c_interface.h"
#include "util.h"

extern "C" {

// UI Event

void
initialize(Config::FreeHaskellFunPtr finalize)
{
    // DEBUG("lock");
    Fl::lock();
    BlockViewWindow::initialize(finalize);
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
    BlockModelConfig *model_config)
{
    BlockViewWindow *win =
        new BlockViewWindow(x, y, w, h, label, *model_config);
    win->show();
    return win;
}


void
destroy(BlockViewWindow *view)
{
    // Make sure all the callbacks are finalized.
    for (int i = view->block.tracks() - 1; i >= 0; i--)
        view->block.remove_track(i);
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
set_status(BlockViewWindow *view, const char *status, Color *color)
{
    view->block.set_status(status, *color);
}

void
set_display_track(BlockViewWindow *view, int tracknum,
        DisplayTrack *dtrack)
{
    view->block.set_display_track(tracknum, *dtrack);
}

void
edit_open(BlockViewWindow *view, int tracknum, double pos, const char *text,
    int select_start, int select_end)
{
    view->block.edit_open(tracknum, ScoreTime(pos), text,
        select_start, select_end);
}

void
edit_append(BlockViewWindow *view, const char *text)
{
    view->block.edit_append(text);
}


// tracks

void
insert_track(BlockViewWindow *view, int tracknum,
        Tracklike *track, int width, Marklist **marklists, int nmarklists)
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
remove_track(BlockViewWindow *view, int tracknum)
{
    view->block.remove_track(tracknum);
}

void
update_track(BlockViewWindow *view, int tracknum,
        Tracklike *track, Marklist **marklists, int nmarklists,
        double start, double end)
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
        view->block.update_track(tracknum, *track,
            ScoreTime(start), ScoreTime(end));
        // No one should be reading this afterwards, but don't leave it
        // pointing to out of scope memory.
        track->ruler = old_ruler;
    } else {
        view->block.update_track(tracknum, *track,
            ScoreTime(start), ScoreTime(end));
    }
}

void
set_track_signal(BlockViewWindow *view, int tracknum, TrackSignal *tsig)
{
    tsig->calculate_val_bounds();
    view->block.set_track_signal(tracknum, *tsig);
}

void
set_track_title(BlockViewWindow *view, int tracknum, const char *title)
{
    view->block.track_at(tracknum)->set_title(title);
}

void
set_track_title_focus(BlockViewWindow *view, int tracknum)
{
    view->block.track_at(tracknum)->set_title_focus();
}

void
set_block_title_focus(BlockViewWindow *view)
{
    view->block.set_title_focus();
}


// rulers

Marklist *
create_marklist(const PosMark *marks, int length)
{
    return new Marklist(marks, length);
}

void
marklist_incref(Marklist *m)
{
    m->incref();
}

void
marklist_decref(Marklist *m)
{
    m->decref();
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
    return show_children(w, nlevels);
}

const char *
dump_view(const BlockViewWindow *view)
{
    return view->block.dump();
}

}
