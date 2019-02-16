// Copyright 2018 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <algorithm>
#include <dirent.h>
#include <ostream>
#include <sndfile.h>
#include <sstream>
#include <vector>

#include "Sample.h"
#include "Synth/Shared/config.h"
#include "log.h"

using std::string;


// util


static bool
ends_with(const string &str, const string &suffix)
{
    return str.compare(
            str.length() - std::min(str.length(), suffix.length()),
            string::npos,
            suffix
        ) == 0;
}


static bool
is_sample(const string &str)
{
    // Don't try to load random junk, e.g. reaper .repeaks files.
    // I write .debug.wav for debugging.
    return ends_with(str, ".wav") && !ends_with(str, ".debug.wav");
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
    std::sort(fnames.begin(), fnames.end());
    return fnames;
}


static string
find_nth_sample(std::ostream &log, const string &dir, int n)
{
    std::vector<string> fnames = list_samples(log, dir);
    return n < fnames.size() ? fnames[n] : "";
}


static string
find_next_sample(std::ostream &log, const string &dir, const string &fname)
{
    std::vector<string> fnames = list_samples(log, dir);
    const auto next = std::find_if(
        fnames.begin(), fnames.end(),
        [&](const string &s) { return s > fname; });
    return next == fnames.end() ?  "" : *next;
}


static SNDFILE *
open_sample(
    std::ostream &log, int channels, bool one_channel_ok, int sample_rate,
    const string &fname, sf_count_t offset, int *file_channels)
{
    SF_INFO info = {0};
    SNDFILE *sndfile = sf_open(fname.c_str(), SFM_READ, &info);
    if (sf_error(sndfile) != SF_ERR_NO_ERROR) {
        LOG(fname << ": " << sf_strerror(sndfile));
    } else if (!(info.channels == channels
        || (one_channel_ok && info.channels == 1)))
    {
        LOG(fname << ": expected " << channels << " channels, got "
            << info.channels);
    } else if (info.samplerate != sample_rate) {
        LOG(fname << ": expected srate of " << sample_rate << ", got "
            << info.samplerate);
    } else if (offset > 0 && sf_seek(sndfile, offset, SEEK_SET) == -1) {
        LOG(fname << ": seek to " << offset << ": " << sf_strerror(sndfile));
    } else {
        if (file_channels)
            *file_channels = info.channels;
        return sndfile;
    }
    sf_close(sndfile);
    return nullptr;
}


// SampleDirectory

SampleDirectory::SampleDirectory(
        std::ostream &log, int channels, int sample_rate,
        const string &dir, sf_count_t offset) :
    log(log), sample_rate(sample_rate), dir(dir), sndfile(nullptr)
{
    int filenum = offset / (CHUNK_SECONDS * sample_rate);
    sf_count_t file_offset = offset % (CHUNK_SECONDS * sample_rate);
    this->fname = find_nth_sample(log, dir, filenum);
    LOG("dir " << dir << ": start at '" << fname << "' + " << file_offset);
    if (!fname.empty()) {
        sndfile = open_sample(
            log, channels, false, sample_rate, dir + '/' + fname, file_offset,
            nullptr);
    }
}


SampleDirectory::~SampleDirectory()
{
    if (sndfile)
        sf_close(sndfile);
}


bool
SampleDirectory::read(int channels, sf_count_t frames, float **out)
{
    buffer.resize(frames * channels);
    sf_count_t total_read = 0;
    while (!fname.empty() && frames - total_read > 0) {
        if (sndfile == nullptr) {
            sndfile = open_sample(
                log, channels, false, sample_rate, dir + '/' + fname, 0,
                nullptr);
            // This means the next read will try again, and maybe spam the log,
            // but otherwise I have to somehow remember this file is bad.
            if (sndfile == nullptr)
                break;
        }
        // TODO read could fail, handle that
        sf_count_t delta = sf_readf_float(
            sndfile, buffer.data() + total_read * channels,
            frames - total_read);
        if (delta < frames - total_read) {
            sf_close(sndfile);
            sndfile = nullptr;
            fname = find_next_sample(log, dir, fname);
            LOG(dir << ": next sample: " << fname);
        }
        total_read += delta;
    };
    std::fill(buffer.begin() + total_read * channels, buffer.end(), 0);
    *out = buffer.data();
    return total_read == 0;
}


// SampleFile

SampleFile::SampleFile(
        std::ostream &log, int channels, bool expand_channels, int sample_rate,
        const string &fname, sf_count_t offset) :
    log(log), expand_channels(expand_channels), fname(fname), sndfile(nullptr),
    file_channels(0)
{
    if (!fname.empty()) {
        LOG(fname << " + " << offset);
        sndfile = open_sample(
            log, channels, expand_channels, sample_rate, fname, offset,
            &this->file_channels);
    }
}


SampleFile::~SampleFile()
{
    if (sndfile)
        sf_close(sndfile);
}


bool
SampleFile::read(int channels, sf_count_t frames, float **out)
{
    // LOG("read " << frames);
    buffer.resize(frames * channels);
    sf_count_t read;
    if (expand_channels && file_channels == 1 && channels != 1) {
        expand_buffer.resize(frames);
        read = sf_readf_float(sndfile, expand_buffer.data(), frames);
        for (sf_count_t f = 0; f < read; f++) {
            for (int c = 0; c < channels; c++) {
                buffer[f*channels + c] = expand_buffer[f];
            }
        }
    } else {
        read = sf_readf_float(sndfile, buffer.data(), frames);
    }
    // sf_readf_float only reads less than asked if the file ended.
    if (read < frames) {
        sf_close(sndfile);
        sndfile = nullptr;
    }
    std::fill(buffer.begin() + read * channels, buffer.end(), 0);
    *out = buffer.data();
    return read == 0;
}