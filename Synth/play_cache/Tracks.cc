// Copyright 2018 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <iostream>
#include <sys/stat.h>
#include <dirent.h>

#include "Audio.h"
#include "Sample.h"
#include "Tracks.h"
#include "log.h"


using std::string;

// See if the fname matches any of the muted instruments.
static bool
suffix_match(const std::vector<string> &mutes, const char *fname)
{
    const char *endp = strrchr(fname, '.');
    int end = endp ? endp - fname : strlen(fname);
    for (const string &mute : mutes) {
        if (mute.length() == end && strncmp(mute.c_str(), fname, end) == 0)
            return true;
    }
    return false;
}


static std::vector<string>
dir_samples(
    std::ostream &log, const string &dir, const std::vector<string> &mutes)
{
    std::vector<string> dirs;
    DIR *d = opendir(dir.c_str());
    if (!d) {
        LOG("can't open dir: " << dir);
        return dirs;
    }
    struct dirent *ent;
    while ((ent = readdir(d)) != nullptr) {
        if (ent->d_type != DT_DIR)
           continue;
        string subdir(ent->d_name);
        if (subdir.empty() || subdir[0] == '.')
            continue;
        if (suffix_match(mutes, subdir.c_str()))
            continue;
        subdir = dir + "/" + subdir;
        LOG("play sample dir: " << subdir);
        dirs.push_back(subdir);
    }
    closedir(d);
    if (dirs.empty()) {
        LOG("no sample dirs in " << dir);
    }
    return dirs;
}


Tracks::Tracks(std::ostream &log, int channels, int sample_rate,
    const string &dir, sf_count_t start_offset,
    const std::vector<string> &mutes)
    : log(log)
{
    std::vector<string> dirnames(dir_samples(log, dir, mutes));
    audios.reserve(dirnames.size());
    for (const auto &dirname : dirnames) {
        std::unique_ptr<Audio> sample(
            new SampleDirectory(
                log, channels, sample_rate, dirname, start_offset));
        audios.push_back(std::move(sample));
    }
}


bool
Tracks::read(int channels, sf_count_t frames, float **out)
{
    buffer.resize(frames * channels);
    std::fill(buffer.begin(), buffer.end(), 0);
    bool done = true;
    for (const auto &audio : audios) {
        float *s_buffer;
        if (!audio->read(channels, frames, &s_buffer)) {
            for (sf_count_t i = 0; i < frames * channels; i++) {
                buffer[i] += s_buffer[i];
            }
            done = false;
        }
    }
    *out = buffer.data();
    return done;
}
