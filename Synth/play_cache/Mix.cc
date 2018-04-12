// Copyright 2018 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <iostream>

#include "Mix.h"
#include "Sample.h"


#define LOG(MSG) LOG_TO(log, MSG)
#define LOG_TO(OUT, MSG) do { OUT << __FILE__ << ':' << __LINE__ << ' ' \
    << MSG << std::endl; } while (0)

Mix::Mix(std::ostream &log, int channels, int sampleRate,
        const std::vector<std::string> &fnames, sf_count_t startOffset)
    : log(log), channels(channels)
{
    samples.reserve(fnames.size());
    for (const auto &fname : fnames) {
        std::unique_ptr<Sample> sample(
            new Sample(log, channels, sampleRate, fname, startOffset));
        samples.push_back(std::move(sample));
    }
}

bool
Mix::read(sf_count_t frames, float **out)
{
    buffer.resize(0);
    buffer.resize(frames * channels);
    bool done = true;
    for (const auto &sample : samples) {
        float *sBuffer;
        sf_count_t count = sample->read(frames, &sBuffer);
        // LOG("requested " << frames << " got " << count);
        for (sf_count_t frame = 0; frame < count; frame++) {
            for (int c = 0; c < channels; c++)
                buffer[frame*channels + c] += sBuffer[frame*channels + c];
        }

        if (count > 0)
            done = false;
    }
    *out = buffer.data();
    return done;
}
