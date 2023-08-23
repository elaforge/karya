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
#include <vector>
#include "AudioFile.h"


class Wav : public AudioFile {
public:
    Wav(const char *fname, Frames offset);
    ~Wav() { close(); }

    Frames read(float *samples, Frames frames) override;
    void close() override;
    int channels() const override { return _channels; }
    int srate() const override { return _srate; }
    int bits() const override { return _bits; }
    const char *error() const override { return _error; }

private:
    FILE *fp;
    int _format;
    int _channels;
    int _srate;
    int _bits;
    const char *_error;
    std::vector<int16_t> buffer;
    // It's a bit goofy to use another buffer for 8 bit samples, but
    // otherwise it's annoying to get all the casting right.
    std::vector<int8_t> buffer8;
};
