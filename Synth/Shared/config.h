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
// This has to be a multiple of the host's block size, and at least one block.
// PlayCache::process only calls Streamer::read once per process() call, after
// subtracting what's left of the delay, so the streaming thread only gets time
// for each *whole* block the delay covers: the usable budget is
// (START_LATENCY_FRAMES / block) * block.  A delay shorter than a block rounds
// down to zero, and read() gets called in the same process() that started the
// play, before the streaming thread has run at all.
//
// 1024 is one block at the host block size I use.  Streamer logs what it
// measures against this budget on each play; check PlayCache.log before
// lowering it, and raise it if the host block size goes up.
#define START_LATENCY_FRAMES 1024

// PlayCache listens on this port for realtime note preview, karya sends to it.
#define THRU_PORT 7090
