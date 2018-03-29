// Copyright 2017 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <algorithm>
#include <dirent.h>
#include "Samples.h"
#include "Sample.h"

#include "util.h"

enum {
    channels = 2
};

Samples::Samples(
        std::ostream &log, sf_count_t maxBlockFrames, int sampleRate,
        const string &dir, sf_count_t startOffset,
        const std::vector<string> &mutes
    ) : log(log), maxBlockFrames(maxBlockFrames), sampleRate(sampleRate),
        mixBuffer(maxBlockFrames * channels)
{
    openDir(dir, startOffset, mutes);
}


// See if the fname matches any of the muted instruments.
static bool
suffixMatch(const std::vector<string> &mutes, const char *fname)
{
    const char *endp = strrchr(fname, '.');
    int end = endp ? endp - fname : strlen(fname);
    for (const string &mute : mutes) {
        if (mute.length() == end && strncmp(mute.c_str(), fname, end) == 0)
            return true;
    }
    return false;
}


static bool
endsWith(const string &str, const string &suffix)
{
    return str.compare(
            str.length() - std::min(str.length(), suffix.length()),
            string::npos,
            suffix
        ) == 0;
}


void
Samples::openDir(const string &dir, sf_count_t startOffset,
    const std::vector<string> &mutes)
{
    DIR *d = opendir(dir.c_str());
    if (!d) {
        log << "can't open dir: " << dir << '\n';
        return;
    }
    struct dirent *ent;
    while ((ent = readdir(d)) != nullptr) {
        if (ent->d_type != DT_REG)
           continue;
        string fname(ent->d_name);
        // Don't try to load random junk, e.g. reaper .repeaks files.
        if (!endsWith(fname, ".wav"))
            continue;
        // I write .debug.wav for debugging.
        if (endsWith(fname, ".debug.wav"))
            continue;

        if (suffixMatch(mutes, ent->d_name))
            continue;
        fname = dir + "/" + fname;
        log << "stream sample: " << fname << '\n';
        samples.push_back(
            new Stream(log, sampleRate, maxBlockFrames, fname, startOffset));
        // std::unique_ptr<Stream> sample(
        //     new Stream(log, sampleRate, maxBlockFrames, fname, startOffset));
        // samples.push_back(std::move(sample));
    }
    if (samples.empty()) {
        log << "no samples in " << dir << '\n';
    }
    closedir(d);
}


sf_count_t
Samples::read(sf_count_t wantedFrames, float **frames)
{
    if (samples.size() == 0) {
        return 0;
    } else if (samples.size() == 1) {
        // log << "space: " << jack_ringbuffer_read_space(samples[0]->ring)
        //     << " requseted: " << wantedFrames * 8 << '\n';
        return samples[0]->read(wantedFrames, frames);
    } else {
        std::fill(mixBuffer.begin(), mixBuffer.end(), 0);
        float *sFrames = nullptr;
        sf_count_t maxCount = 0;
        for (const auto &sample : samples) {
            sf_count_t count = sample->read(wantedFrames, &sFrames);
            maxCount = std::max(maxCount, count);
            for (sf_count_t frame = 0; frame < count; frame++) {
                mixBuffer[frame*2] += sFrames[frame*2];
                mixBuffer[frame*2 + 1] += sFrames[frame*2 + 1];
            }
        }
        *frames = mixBuffer.data();
        return maxCount;
    }
}
