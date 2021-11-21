// Copyright 2021 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

// Simple wav support.
//
// I used to use libsndfile, but I would get crashes, perhaps due to it being
// non-thread safe.  It's overkill anyway, except when I do want to support
// other formats.
//
// This only supports reading, and float format.

#pragma once

#include <stddef.h>

namespace wav {

typedef const char *Error;
typedef size_t Frames;

struct Wav;

Error open_read(const char *fname, Wav **wav, int channels, int srate);
Error close(Wav *wav);
Frames read(Wav *wav, Frames frames, float *samples);

}
