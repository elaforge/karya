// Copyright 2017 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <algorithm>
#include <dirent.h>
#include "Samples.h"
#include "Sample.h"

#include "util.h"


Samples::Samples(std::ofstream &log, int sampleRate, const char *dir,
    const string &blockId, const std::vector<string> &mutes)
{
    // First try dir/blockId.wav.  If that doesn't exist, look for
    // dir/blockId/*.wav.
    string wholeBlock = string(dir) + blockId + ".wav";
    if (!openSample(sampleRate, wholeBlock)) {
        string subdir = string(dir) + blockId;
        openSamples(log, sampleRate, subdir, mutes);
    }
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

void
Samples::openSamples(std::ofstream &log, int sampleRate, const string &dir,
    const std::vector<string> &mutes)
{
    DIR *d = opendir(dir.c_str());
    if (!d)
        return;
    struct dirent *ent;
    while ((ent = readdir(d)) != nullptr) {
        if (ent->d_type != DT_REG)
           continue;

        if (suffixMatch(mutes, ent->d_name))
            continue;
        string fname = dir + "/" + string(ent->d_name);
        if (!openSample(sampleRate, fname)) {
            errors_.push_back(fname + ": not found");
        }
    }
    closedir(d);
    if (samples.empty())
        errors_.push_back("no samples matching '" + dir + "'");
}

bool
Samples::openSample(int sampleRate, const string &fname)
{
    std::unique_ptr<Sample> s(new Sample(sampleRate, fname));
    switch (s->error) {
    case Sample::NoError:
        samples.push_back(std::move(s));
        filenames_.push_back(fname);
        return true;
    case Sample::ErrorMessage:
        errors_.push_back(fname + ": " + s->errorMessage);
        return false;
    case Sample::FileNotFound:
        return false;
    }
}


const sf_count_t
Samples::read(sf_count_t fromFrame, sf_count_t wantedFrames, float **frames)
{
    if (samples.size() == 0) {
        return 0;
    } else if (samples.size() == 1) {
        return samples[0]->read(fromFrame, frames);
    } else {
        mixBuffer.clear();
        mixBuffer.resize(wantedFrames * 2);
        float *sFrames = nullptr;
        for (const auto &sample : samples) {
            sf_count_t count = sample->read(fromFrame, &sFrames);
            count = std::min(count, wantedFrames);
            for (sf_count_t frame = 0; frame < count; frame++) {
                mixBuffer[frame*2] += sFrames[frame*2];
                mixBuffer[frame*2 + 1] += sFrames[frame*2 + 1];
            }
        }
        *frames = mixBuffer.data();
        return wantedFrames;
    }
}
