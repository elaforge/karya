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


class Wav {
public:
    typedef const char *Error;
    typedef size_t Frames;

    // TODO Signature is inconsistent with Flac::open, make them consistent?
    static Error open(const char *fname, Wav **wav, Frames offset);
    ~Wav();
    Frames read(float *samples, Frames frames);
    Error close();

    int channels() const { return _channels; };
    int srate() const { return _srate; };

private:
    Wav(FILE *fp, int channels, int srate)
        : fp(fp), _channels(channels), _srate(srate) {}
    FILE *fp;
    int _channels;
    int _srate;
};
