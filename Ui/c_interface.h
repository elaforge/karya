// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include "Block.h"
#include "EventTrack.h"
#include "MsgCollector.h"
#include "SkeletonDisplay.h"
#include "SymbolTable.h"
#include "Track.h"
#include "Zoom.h"
#include "config.h"
#include "geom.h"


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
BlockWindow *create(int x, int y, int w, int h, const char *label,
    BlockConfig *config);
void destroy(BlockWindow *view);
void get_view_status(
    BlockWindow *view, IRect *rect, Zoom *zoom, Padding *padding);

void set_size(BlockWindow *view, int x, int y, int w, int h);
void get_size(BlockWindow *view, int *sz);
void set_zoom(BlockWindow *view, const Zoom *zoom);
void set_track_scroll(BlockWindow *view, int pixels);
void set_selection(BlockWindow *view, int selnum, int tracknum,
    Selection *sels, int nsels);
void bring_to_front(BlockWindow *view);

void set_config(BlockWindow *view, BlockConfig *config);
void set_skeleton(BlockWindow *view, SkeletonConfig *skel);
void set_title(BlockWindow *view, const char *title);
void set_status(BlockWindow *view, const char *status, Color *color);
void set_display_track(BlockWindow *view, int tracknum, DisplayTrack *dtrack);

void floating_open(BlockWindow *view, int tracknum, double pos,
    const char *text, int select_start, int select_end);
void floating_insert(BlockWindow *view, const char *text);

// tracks

int tracks(BlockWindow *view);
void insert_track(BlockWindow *view, int tracknum,
        Tracklike *track, int width, Marklist **marklists, int nmarklists);
void remove_track(BlockWindow *view, int tracknum);
void update_track(BlockWindow *view, int tracknum,
        Tracklike *track, Marklist **marklists, int nmarklists,
        double start, double end);
void set_track_signal(BlockWindow *view, int tracknum, TrackSignal *tsig);
void set_waveform(BlockWindow *view, int tracknum, int chunknum,
    const char *filename, double start, double *ratiosp, int ratios_len);
void set_track_title(BlockWindow *view, int tracknum, const char *title);
void set_track_title_focus(BlockWindow *view, int tracknum);
void set_block_title_focus(BlockWindow *view);

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

const char *i_show_children(const BlockWindow *w, int nlevels);
const char *dump_view(const BlockWindow *view);

}
