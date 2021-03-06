/* Copyright 2013 Evan Laforge
 * This program is distributed under the terms of the GNU General Public
 * License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
 */

// Constants shared between C++ and Haskell.
#pragma once

#define SAMPLING_RATE 44100

// Each audio checkpoint is exactly this many seconds, except the last one.
#define CHUNK_SECONDS 4

// Delay play_cache start by this much.  Description in Config.hs
//
// 512 (~11ms) should be enough, because that will give a full process() call
// for the streaming thread to get its act together.
#define START_LATENCY_FRAMES 512

// PlayCache listens on this port for realtime note preview, karya sends to it.
#define THRU_PORT 7090
