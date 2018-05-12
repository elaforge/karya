// Copyright 2018 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <iostream>

#include "Mix.h"
#include "SampleDirectory.h"
#include "log.h"


Mix::Mix(std::ostream &log, int channels, int sampleRate,
        const std::vector<std::string> &dirnames, sf_count_t startOffset)
    : log(log), channels(channels)
{
    sampleDirs.reserve(dirnames.size());
    for (const auto &dirname : dirnames) {
        std::unique_ptr<SampleDirectory> sampleDir(
            new SampleDirectory(
                log, channels, sampleRate, dirname, startOffset));
        sampleDirs.push_back(std::move(sampleDir));
    }
}

bool
Mix::read(sf_count_t frames, float **out)
{
    buffer.resize(0);
    buffer.resize(frames * channels);
    bool done = true;
    for (const auto &sampleDir : sampleDirs) {
        float *sBuffer;
        sf_count_t count = sampleDir->read(frames, &sBuffer);
        // LOG("requested " << frames << " got " << count);
        for (sf_count_t i = 0; i < count * channels; i++) {
            buffer[i] += sBuffer[i];
        }

        if (count > 0)
            done = false;
    }
    *out = buffer.data();
    return done;
}
