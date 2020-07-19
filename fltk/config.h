// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

// This has constants which are not worth putting under host control by putting
// in a *Config class.

#include <FL/Enumerations.H>

#include "global.h"


namespace Config {

enum {
    // Color as a word so haskell can get to it easily.
    abbreviation_color_word = 0x0000ff00
};

// This color signifies that some content has been omitted due to lack of
// space.
extern const Color abbreviation_color;

// Mark the playing end of events with this.
extern const Color event_trigger_color;

// Draw track waveforms with this color.
extern const Color waveform_color;

extern const Color skeleton_display_bg;
extern const Color focus_skeleton_display_bg;
extern const Color block_bg;

// Pass the addresses of callbacks to this before they are replaced.  This
// lets the haskell GC know that C++ no longer references the given value.
typedef void (*FreeHaskellFunPtr)(void *val);

// private except to BlockWindow::initialize
extern FreeHaskellFunPtr _free_haskell_fun_ptr;
void free_haskell_fun_ptr(void *val);

// Default sizes for a Block.
//
// Previously these were configurable from haskell in the same way as
// BlockConfig, but it didn't seem useful so they're hardcoded now.
namespace Block {
    enum {
        block_title_height = 20,
        track_title_height = 20,
        skel_height = 16,
        sb_size = 6,
        status_size = 16,
        // Pixels added to block.get_padding().y.  Otherwise it winds up being
        // a few pixels too small for 'ViewConfig.zoom_to_ruler' to get the
        // bottom of the ruler in sight, probably due to those all-important
        // bevels.
        extra_time_padding = 6
    };
};

enum {
    font = FL_HELVETICA
};

namespace font_size {
    enum {
        // Block and track titles.
        input = 12,
        // Text labels in the ruler.
        ruler = 9,
        // Per-track status, drawn in the SkeletonDisplay.
        track_status = 12,
        // Title drawn in the SkeletonDisplay.
        skeleton_title = 10,

        // Keycap char for Keycaps.
        keycaps_label = 12,
        // Binding text for Keycaps.
        keycaps_binding = 14
    };
};

}
