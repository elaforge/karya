// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include "config.h"
#include "MsgCollector.h"
#include "SymbolTable.h"
#include "Block.h"
#include "Track.h"
#include "EventTrack.h"
#include "SkeletonDisplay.h"

extern "C" {

// UI Event

void initialize(Config::FreeHaskellFunPtr finalize);
void ui_wait();
void ui_awake();
int get_ui_msgs(UiMsg **msgs);
void clear_ui_msgs();


// Block view

// Passing a RulerConfig is hard because it uses a vector.  vector is really
// convenient in c++, but to pass it from haskell I need this grody hack where
// I pass a partially constructed RulerConfig and then fill in the vector from
// the passed c array.
// This hack is also in insert_track.
BlockViewWindow *create(int x, int y, int w, int h, const char *label,
    BlockModelConfig *model_config);
void destroy(BlockViewWindow *view);

void set_size(BlockViewWindow *view, int x, int y, int w, int h);
void get_size(BlockViewWindow *view, int *sz);
void set_zoom(BlockViewWindow *view, const ZoomInfo *zoom);
void set_track_scroll(BlockViewWindow *view, int pixels);
void set_selection(BlockViewWindow *view, int selnum, int tracknum,
    Selection *sels, int nsels);
void bring_to_front(BlockViewWindow *view);

void set_model_config(BlockViewWindow *view, BlockModelConfig *config);
void set_skeleton(BlockViewWindow *view, SkeletonConfig *skel);
void set_title(BlockViewWindow *view, const char *title);
void set_status(BlockViewWindow *view, const char *status, Color *color);
void set_display_track(BlockViewWindow *view, int tracknum,
        DisplayTrack *dtrack);

void floating_open(BlockViewWindow *view, int tracknum, double pos,
    const char *text, int select_start, int select_end);
void floating_insert(BlockViewWindow *view, const char *text);

// tracks

int tracks(BlockViewWindow *view);
void insert_track(BlockViewWindow *view, int tracknum,
        Tracklike *track, int width, Marklist **marklists, int nmarklists);
void remove_track(BlockViewWindow *view, int tracknum);
void update_track(BlockViewWindow *view, int tracknum,
        Tracklike *track, Marklist **marklists, int nmarklists,
        double start, double end);
void set_track_signal(BlockViewWindow *view, int tracknum, TrackSignal *tsig);
void set_track_title(BlockViewWindow *view, int tracknum, const char *title);
void set_track_title_focus(BlockViewWindow *view, int tracknum);
void set_block_title_focus(BlockViewWindow *view);

// rulers

// These can't be methods because the haskell FFI doesn't understand C++.
Marklist *create_marklist(const PosMark *marks, int length);
void marklist_incref(Marklist *m);
void marklist_decref(Marklist *m);

// symbols

void insert_symbol(const char *name, int absolute_y,
    const SymbolTable::Glyph *glyphs, int glyphs_len);
int get_font(const char *name);
char **get_fonts();

// styles

void insert_style(StyleId id, EventStyle *style);

// debugging

const char *i_show_children(const BlockViewWindow *w, int nlevels);
const char *dump_view(const BlockViewWindow *view);

}
