// Copyright 2018 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

#include "Wav.h"

typedef Wav::Frames Frames;
// using typename Wav::Frames;  // <-- no work?

// An abstract audio stream.
class Audio {
public:
    virtual ~Audio() {};
    // Read the number of frames and put it in the out pointer.  The samples
    // are interleaved.  Return true if there are 0 frames read, false if
    // all frames were read.
    virtual bool read(int channels, Frames frames, float **out) = 0;
};


class AudioEmpty : public Audio {
public:
    AudioEmpty() {}
    bool read(int channels, Frames frames, float **out) override {
        return true;
    };
};
