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
listSamples(std::ostream &log, const string &dir)
{
    struct dirent *ent;
    std::vector<string> fnames;
    DIR *d = opendir(dir.c_str());
    if (!d) {
        LOG("listSamples: not a dir: " << dir);
        return fnames;
    }
    while ((ent = readdir(d)) != nullptr) {
        if (ent->d_type != DT_REG && ent->d_type != DT_LNK)
            continue;
        string fname(ent->d_name);
        if (isSample(fname))
            fnames.push_back(fname);
    }
    std::sort(fnames.begin(), fnames.end());
    return fnames;
}


static string
findNthSample(std::ostream &log, const string &dir, int n)
{
    std::vector<string> fnames = listSamples(log, dir);
    return n < fnames.size() ? fnames[n] : "";
}


static string
findNextSample(std::ostream &log, const string &dir, const string &fname)
{
    std::vector<string> fnames = listSamples(log, dir);
    const auto next = std::find_if(
        fnames.begin(), fnames.end(),
        [&](const string &s) { return s > fname; });
    return next == fnames.end() ?  "" : *next;
}


static SNDFILE *
openSample(
    std::ostream &log, int channels, int sampleRate,
    const string &fname, sf_count_t offset)
{
    SF_INFO info;
    memset(&info, 0, sizeof info);
    SNDFILE *sndfile = sf_open(fname.c_str(), SFM_READ, &info);
    if (sf_error(sndfile) != SF_ERR_NO_ERROR) {
        LOG(fname << ": " << sf_strerror(sndfile));
    } else if (info.channels != channels) {
        LOG(fname << ": expected " << channels << " channels, got "
            << info.channels);
    } else if (info.samplerate != sampleRate) {
        LOG(fname << ": expected srate of " << sampleRate << ", got "
            << info.samplerate);
    } else if (offset > 0 && sf_seek(sndfile, offset, SEEK_SET) == -1) {
        LOG(fname << ": seek to " << offset << ": " << sf_strerror(sndfile));
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
    this->fname = findNthSample(log, dir, filenum);
    LOG("dir " << dir << ": start at '" << fname << "' + " << fileOffset);
    if (!fname.empty()) {
        sndfile = openSample(
            log, channels, sampleRate, dir + '/' + fname, fileOffset);
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
    buffer.resize(frames * channels);
    sf_count_t totalRead = 0;
    do {
        if (fname.empty())
            break;
        if (sndfile == nullptr) {
            sndfile = openSample(
                log, channels, sampleRate, dir + '/' + fname, 0);
            // This means the next read will try again, and maybe spam the log,
            // but otherwise I have to somehow remember this file is bad.
            if (sndfile == nullptr)
                break;
        }
        // TODO read could fail, handle that
        sf_count_t delta = sf_readf_float(
            sndfile, buffer.data() + totalRead * channels, frames - totalRead);
        if (delta < frames - totalRead) {
            sf_close(sndfile);
            sndfile = nullptr;
            fname = findNextSample(log, dir, fname);
            LOG(dir << ": next sample: " << fname);
        }
        totalRead += delta;
    } while (totalRead < frames);
    *out = buffer.data();
    return totalRead;
}


// SampleFile

SampleFile::SampleFile(
        std::ostream &log, int channels, int sampleRate,
        const string &fname, sf_count_t offset) :
    log(log), channels(channels), fname(fname), sndfile(nullptr)
{
    sf_count_t fileOffset = offset % (CHECKPOINT_SECONDS * sampleRate);
    LOG(fname << " + " << fileOffset);
    if (!fname.empty()) {
        sndfile = openSample(log, channels, sampleRate, fname, fileOffset);
    }
}


SampleFile::~SampleFile()
{
    if (sndfile)
        sf_close(sndfile);
}


sf_count_t
SampleFile::read(sf_count_t frames, float **out)
{
    buffer.resize(frames * channels);
    sf_count_t totalRead = 0;
    do {
        if (!sndfile)
            break;
        sf_count_t delta = sf_readf_float(
            sndfile, buffer.data() + totalRead * channels, frames - totalRead);
        if (delta < frames - totalRead) {
            sf_close(sndfile);
            sndfile = nullptr;
        }
        totalRead += delta;
    } while (totalRead < frames);
    *out = buffer.data();
    return totalRead;
}
