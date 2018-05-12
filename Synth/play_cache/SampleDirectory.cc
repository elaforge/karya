// Copyright 2018 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <algorithm>
#include <dirent.h>
#include <sndfile.h>
#include <ostream>
#include <sstream>
#include <vector>

#include "SampleDirectory.h"
#include "Synth/Shared/config.h"
#include "log.h"

using std::string;


// util

static bool
endsWith(const string &str, const string &suffix)
{
    return str.compare(
            str.length() - std::min(str.length(), suffix.length()),
            string::npos,
            suffix
        ) == 0;
}


static bool
isSample(const string &str)
{
    // Don't try to load random junk, e.g. reaper .repeaks files.
    // I write .debug.wav for debugging.
    return endsWith(str, ".wav") && !endsWith(str, ".debug.wav");
}


static std::vector<string>
listSamples(const string &dir)
{
    DIR *d = opendir(dir.c_str());
    struct dirent *ent;
    std::vector<string> fnames;
    while ((ent = readdir(d)) != nullptr) {
        if (ent->d_type != DT_REG)
            continue;
        string fname(ent->d_name);
        if (isSample(fname))
            fnames.push_back(fname);
    }
    std::sort(fnames.begin(), fnames.end());
    return fnames;
}


static string
findNthSample(const string &dir, int n)
{
    std::vector<string> fnames = listSamples(dir);
    return n < fnames.size() ? fnames[n] : "";
}


static string
findNextSample(const string &dir, const string &fname)
{
    std::vector<string> fnames = listSamples(dir);
    const auto next = std::find_if(
        fnames.begin(), fnames.end(),
        [&](const string &s) { return s > fname; });
    return next == fnames.end() ?  "" : *next;
}


// static SNDFILE *
// openSample(
//     std::ostream &log, int channels, int sampleRate,
//     const std::string &fname, sf_count_t startOffset)
// {
//     SF_INFO info;
//     SNDFILE *sndfile = sf_open(fname.c_str(), SFM_READ, &info);
//     // std::stringstream errors;
//     if (sf_error(sndfile) == SF_ERR_NO_ERROR) {
//
//     } else if (info.channels != channels) {
//         LOG(fname << : "expected " << channels << " channels, got "
//             << info.channels);
//         // errors << "expected " << channels << " channels, got " << info.channels;
//     } else if (info.samplerate != sampleRate) {
//         LOG(fname << ": expected srate of " << sampleRate << ", got "
//             << info.samplerate;
//         // errors << "expected srate of " << sampleRate << ", got "
//         //     << info.samplerate;
//     }
//     if (!errors.str().empty()) {
//         LOG(fname << ": " << errors.str() << std::endl);
//         sf_close(sndfile);
//         return nullptr;
//     } else if (startOffset > 0) {
//         if (sf_seek(sndfile, startOffset, SEEK_SET) == -1) {
//             // Likely the sample isn't long enough.
//             sf_close(sndfile);
//             return nullptr;
//         }
//     }
//     return sndfile;
// }

static SNDFILE *
openSample(
    std::ostream &log, int channels, int sampleRate,
    const string &dir, const string &fname, sf_count_t offset)
{
    SF_INFO info;
    string path = dir + '/' + fname;
    SNDFILE *sndfile = sf_open(path.c_str(), SFM_READ, &info);
    if (sf_error(sndfile) != SF_ERR_NO_ERROR) {
        LOG(path << ": " << sf_strerror(sndfile));
    } else if (info.channels != channels) {
        LOG(path << ": expected " << channels << " channels, got "
            << info.channels);
    } else if (info.samplerate != sampleRate) {
        LOG(path << ": expected srate of " << sampleRate << ", got "
            << info.samplerate);
    } else if (offset > 0 && sf_seek(sndfile, offset, SEEK_SET) == -1) {
        LOG(path << ": seek to " << offset << ": " << sf_strerror(sndfile));
    } else {
        return sndfile;
    }
    sf_close(sndfile);
    return nullptr;
}


// SampleDirectory

SampleDirectory::SampleDirectory(
        std::ostream &log, int channels, int sampleRate,
        const string &dir, sf_count_t offset) :
    log(log), channels(channels), sampleRate(sampleRate), dir(dir),
    sndfile(nullptr)
{
    int filenum = offset / (CHECKPOINT_SECONDS * sampleRate);
    sf_count_t fileOffset = offset % (CHECKPOINT_SECONDS * sampleRate);
    this->fname = findNthSample(dir, filenum);
    LOG("dir " << dir << ": start at " << fname << " + " << fileOffset);
    if (!fname.empty()) {
        sndfile = openSample(log, channels, sampleRate, dir, fname, fileOffset);
    }
}


SampleDirectory::~SampleDirectory()
{
    if (sndfile)
        sf_close(sndfile);
}


sf_count_t
SampleDirectory::read(sf_count_t frames, float **out)
{
    buffer.resize(0);
    buffer.resize(frames * channels);
    sf_count_t read = 0;
    do {
        if (fname.empty())
            break;
        if (sndfile == nullptr) {
            sndfile = openSample(log, channels, sampleRate, dir, fname, 0);
            // This means the next read will try again, and maybe spam the log,
            // but otherwise I have to somehow remember this file is bad.
            if (sndfile == nullptr)
                break;
        }
        // TODO read could fail, handle that
        sf_count_t delta =
            sf_readf_float(sndfile, buffer.data() + read, frames - read);
        LOG("read " << fname << ": " << delta << "/" << frames);
        if (delta < frames - read) {
            sf_close(sndfile);
            sndfile = nullptr;
            fname = findNextSample(dir, fname);
            LOG("next sample: " << fname);
        }
        read += delta;
    } while (read < frames);
    *out = buffer.data();
    return read;
}
