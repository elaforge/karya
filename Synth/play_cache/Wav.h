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
#include "AudioFile.h"


class Wav : public AudioFile {
public:
    Wav(const char *fname, Frames offset);
    ~Wav() { close(); }

    Frames read(float *samples, Frames frames) override;
    void close() override;
    int channels() const override { return _channels; }
    int srate() const override { return _srate; }
    int bits() const override { return 16; }
    const char *error() const override { return _error; }

private:
    FILE *fp;
    int _channels;
    int _srate;
    const char *_error;
};
