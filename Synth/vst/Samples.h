// Copyright 2017 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
#pragma once
#include <fstream>
#include <memory>
#include <string>
#include <vector>

#include <sndfile.h>

#include "Sample.h"


using std::string;

// Read and mix multiple samples.
//
// Read dir/blockId.wav if present.  Otherwise, mix together dir/blockId-*.wav,
// omitting the ones in mutedInstruments.  This should go on a non-audio
// thread, so it can allocate memory, but reading samples should be
// non-allocating.  TODO except that isn't implemented yet, it's all on the
// audio thread.
//
// TODO this doesn't resample, so the sound files must have the same srate
// as the VST host.  In the future I could link in libsamplerate.
class Samples {
public:
    Samples(std::ofstream &log, int samplingRate, const char *dir,
        const string &blockId, const std::vector<string> &mutedInstruments);
    const sf_count_t read(
        sf_count_t fromFrame, sf_count_t wantedFrames, float **frames);

    // If non-empty, there was an error loading samples.
    const std::vector<string> &errors() const { return errors_; }
    const std::vector<string> &filenames() const { return filenames_; }

private:
    void openSamples(int sampleRate, const char *dir, const string &blockId,
        const std::vector<string> &muted);
    bool openSample(int sampleRate, const std::string &fname);

    std::vector<string> errors_;
    // Keep a log of each file.
    std::vector<string> filenames_;

    std::vector<std::unique_ptr<Sample>> samples;
    std::vector<float> mixBuffer;
};
