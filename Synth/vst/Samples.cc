// Copyright 2017 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <algorithm>
#include <dirent.h>
#include "Samples.h"
#include "Sample.h"

#include "util.h"


Samples::Samples(std::ofstream &log, int sampleRate, const char *dir,
    const string &blockId, const std::vector<string> &muted)
{
    string wholeBlock = string(dir) + blockId + ".wav";
    if (!openSample(sampleRate, wholeBlock)) {
        openSamples(sampleRate, dir, blockId, muted);
    }
}


// See if the fname matches any of the muted instruments.
// The filename should look like blockId,instrument.*
static bool
suffixMatch(const std::vector<string> &muted, const string &fname)
{
    if (muted.empty())
        return false;

    size_t end = fname.rfind('.');
    if (end == string::npos)
        end = fname.size();
    size_t begin = fname.rfind(',', end);
    string instrument = fname.substr(begin, end);
    return std::find(muted.begin(), muted.end(), instrument) != muted.end();
}

void
Samples::openSamples(int sampleRate, const char *dir, const string &blockId,
    const std::vector<string> &muted)
{
    string prefix = string(dir) + blockId;
    DIR *d = opendir(dir);
    ASSERT(d);
    struct dirent *ent;
    while ((ent = readdir(d)) != nullptr) {
        if (ent->d_type != DT_REG)
           continue;
        if (strncmp(blockId.c_str(), ent->d_name, std::min(blockId.length(),
                (size_t) ent->d_namlen)) != 0) {
            continue;
        }

        string fname = string(dir) + "/" + string(ent->d_name);
        if (suffixMatch(muted, fname))
            continue;
        if (!openSample(sampleRate, fname)) {
            errors_.push_back(fname + ": not found");
        }
    }
    closedir(d);
    if (samples.empty())
        errors_.push_back("no samples matching '" + prefix + "'");
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
        mixBuffer.resize(wantedFrames);
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
