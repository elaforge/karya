// Copyright 2018 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <algorithm>
#include <dirent.h>
#include <ostream>
#include <stdio.h>
#include <string.h>
#include <vector>

#include "SampleDirectory.h"
#include "Synth/Shared/config.h"
#include "log.h"
#include "util.h"

using std::string;


static bool
is_sample(const string &str)
{
    // Don't try to load random junk, e.g. reaper .repeaks files.
    // I write .debug.wav for debugging.
    return util::ends_with(str, ".wav") && !util::ends_with(str, ".debug.wav");
}


static std::vector<string>
list_samples(std::ostream &log, const string &dir)
{
    struct dirent *ent;
    std::vector<string> fnames;
    DIR *d = opendir(dir.c_str());
    if (!d) {
        LOG("list_samples: not a dir: " << dir);
        return fnames;
    }
    while ((ent = readdir(d)) != nullptr) {
        if (ent->d_type != DT_REG && ent->d_type != DT_LNK)
            continue;
        string fname(ent->d_name);
        if (is_sample(fname))
            fnames.push_back(fname);
    }
    if (closedir(d) == -1) {
        LOG("closing " << dir << ": " << strerror(errno));
    }
    std::sort(fnames.begin(), fnames.end());
    return fnames;
}


// Open the file at the given offset.  Return nullptr if there was an error,
// or the offset is past the end of the file.
static std::unique_ptr<Wav>
open_wav(
    std::ostream &log, int channels, bool one_channel_ok, int sample_rate,
    const string &fname, Frames offset, int *file_channels)
{
    std::unique_ptr<Wav> wav(new Wav(fname.c_str(), offset));
    if (wav->error()) {
        LOG(fname << ": " << wav->error());
        return nullptr;
    }
    if (!(wav->channels() == channels
        || (one_channel_ok && wav->channels() == 1)))
    {
        LOG(fname << ": expected " << channels << " channels, got "
            << wav->channels());
    } else if (wav->srate() != sample_rate) {
        LOG(fname << ": expected srate of " << sample_rate << ", got "
            << wav->srate());
    } else {
        if (file_channels)
            *file_channels = wav->channels();
        // I used to check if I had seeked off the end of the sample, or if
        // it's a silent block (0 frames, emitted by
        // Audio.File.writeCheckpoints), and if so close it right here.  But
        // it's simpler to not expose frames_remaining and let read() run out
        // when it runs out.
        return wav;
    }
    return nullptr;
}


static const string no_fname;

SampleDirectory::SampleDirectory(
        std::ostream &log, int channels, int sample_rate,
        const string &dir, Frames offset) :
    log(log), sample_rate(sample_rate), dir(dir),
    fnames(list_samples(log, dir)),
    index(offset / (CHUNK_SECONDS * sample_rate)),
    wav(nullptr), frames_left(0)
{
    Frames file_offset = offset % (CHUNK_SECONDS * sample_rate);
    LOG("dir " << dir << ": start at '" << cur_fname() << "' + "
        << file_offset);
    this->open(channels, file_offset);
}


const string &
SampleDirectory::cur_fname() const
{
    return index < fnames.size() ? fnames[index] : no_fname;
}


// Re-read the directory listing, in case the synth has written more chunks
// since the last time.
//
// This gets one shot: it's only called when I run off the end of the listing,
// and if it doesn't find anything then 'index' never advances again, so read()
// stops and never asks again.  So playing ahead of a synth that renders slower
// than realtime truncates the instrument rather than waiting for it.
void
SampleDirectory::refresh()
{
    std::vector<string> found(list_samples(log, dir));
    if (found.size() > fnames.size())
        fnames = std::move(found);
}



bool
SampleDirectory::read(int channels, Frames frames, float **out)
{
    buffer.resize(frames * channels);
    Frames total_read = 0;
    while (index < fnames.size() && frames - total_read > 0) {
        const Frames offset = total_read * channels;
        Frames delta;
        if (wav == nullptr) {
            // File is a silent chunk or otherwise ended early.
            if (frames_left > 0) {
                delta = std::min(frames_left, frames - total_read);
                frames_left -= delta;
                // I could possibly notice when it's all 0 and avoid the work
                // and the mixing, but memset 0 should be fast, and mixing is
                // pretty trivial too.
                std::fill(
                    buffer.begin() + offset,
                    buffer.begin() + offset + delta * channels,
                    0);
            } else {
                break;
            }
        } else {
            // TODO read could fail, handle that
            delta = wav->read(buffer.data() + offset, frames - total_read);
            // delta could be > frames_left if a chunk > CHUNK_SECONDS, which
            // shouldn't happen.  But if it does, the rest will be offset,
            // which hopefully I'll notice.
            frames_left -= std::min(frames_left, delta);
            if (delta < frames - total_read) {
                // Short read, this file is done.
                wav.reset();
            }
        }
        if (frames_left == 0) {
            this->index++;
            this->open(channels, 0);
            LOG(dir << ": next sample: "
                << (index < fnames.size() ? cur_fname() : "<done>"));
        }
        total_read += delta;
    };
    std::fill(buffer.begin() + total_read * channels, buffer.end(), 0);
    *out = buffer.data();
    return total_read == 0;
}


void
SampleDirectory::open(int channels, Frames offset)
{
    if (wav)
        wav.reset();
    // Ran off the end of the listing, but the synth may have written more
    // since I made it.
    if (index >= fnames.size())
        this->refresh();
    if (index < fnames.size()) {
        wav = open_wav(
            log, channels, false, sample_rate, dir + '/' + cur_fname(), offset,
            nullptr);
        // offset should never be > chunk frames.
        this->frames_left = CHUNK_SECONDS * sample_rate - offset;
    }
}
