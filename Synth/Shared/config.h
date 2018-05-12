/* Copyright 2013 Evan Laforge
 * This program is distributed under the terms of the GNU General Public
 * License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
 */

// Constants shared between C++ and Haskell.
#pragma once

#define SAMPLING_RATE 44100

// Each audio checkpoint is exactly this many seconds, except the last one.
#define CHECKPOINT_SECONDS 1
