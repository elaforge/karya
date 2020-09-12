// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include "config.h"

namespace Config {

const Color abbreviation_color = Color::from_rgba_word(
    Config::abbreviation_color_word);
const Color event_trigger_color = Color(255, 0, 0);
const Color trailing_space_color = Color(255, 0, 0);
const Color waveform_color = Color(170, 190, 190);

const Color skeleton_display_bg = Color(0xb0, 0xb0, 0xb0);
const Color focus_skeleton_display_bg = Color(0xd0, 0xb0, 0xb0);
const Color block_bg = Color(0xcc, 0xcc, 0xcc);

FreeHaskellFunPtr _free_haskell_fun_ptr = 0;

void
free_haskell_fun_ptr(void *val)
{
    if (_free_haskell_fun_ptr) {
        _free_haskell_fun_ptr(val);
    } else {
        DEBUG("null _free_haskell_fun_ptr, someone didn't call"
            " BlockWindow::initialize: "  << val);
    }
}

}
