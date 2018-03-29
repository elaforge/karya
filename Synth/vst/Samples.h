// Copyright 2017 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
#pragma once
#include <fstream>
#include <memory>
#include <string>
#include <vector>

#include <sndfile.h>

#include "Stream.h"


using std::string;

// Read and mix multiple samples.
//
// Read dir/blockId.wav if present.  Otherwise, mix together dir/blockId-*.wav,
// omitting the ones in mutedInstruments.  This should be created on a
// non-audio thread, so it can allocate memory, but read() should be
// realtime-safe.
//
// This doesn't resample, so the sound files must have the same srate as the
// VST host.  In the future I could link in libsamplerate.
class Samples {
public:
    Samples(
        std::ostream &log, sf_count_t maxBlockFrames, int sampleRate,
        const string &dir, sf_count_t startOffset,
        const std::vector<string> &mutes);

    // This function is realtime-safe.
    sf_count_t read(sf_count_t wantedFrames, float **frames);

private:
    void openDir(const string &dir, sf_count_t startOffset,
        const std::vector<string> &mutes);

    // Constants, just for convenience.
    std::ostream &log;
    const sf_count_t maxBlockFrames;
    const int sampleRate;

    // std::vector<std::unique_ptr<Stream>> samples;
    std::vector<Stream *> samples;
    std::vector<float> mixBuffer;
};
