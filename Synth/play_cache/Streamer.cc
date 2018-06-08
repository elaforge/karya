// Copyright 2018 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <dirent.h>
#include <ostream>
#include <string.h>

#include "Streamer.h"
#include "log.h"
#include "ringbuffer.h"


using std::string;

enum {
    // This many maxFrames in the ring.
    // jack_ringbuffer_create will round up to the next power of 2.
    ringBlocks = 4,
    // Read this many frames at a time.  Should be smaller than ringBlocks *
    // maxFrames!
    readFrames = 512
};


Streamer::Streamer(
        std::ostream &log, int channels, int sampleRate,
        int maxFrames)
    : channels(channels), sampleRate(sampleRate), maxFrames(maxFrames),
        log(log), threadQuit(false), mixDone(false), restart(false),
        debt(0)
{
    ring = jack_ringbuffer_create(ringBlocks * maxFrames * channels);
    jack_ringbuffer_mlock(ring);
    outputBuffer.resize(maxFrames * channels);
    // Assume file path and number of muted tracks won't go above this, so
    // start() doesn't allocate.
    state.dir.reserve(4096);
    state.mutes.reserve(64);

    streamThread.reset(new std::thread(&Streamer::streamLoop, this));
}


Streamer::~Streamer()
{
    LOG("stop");
    threadQuit.store(true);
    ready.post();
    streamThread->join();
    jack_ringbuffer_free(ring);
}


void
Streamer::start(const string &dir, sf_count_t startOffset,
    const std::vector<string> &mutes)
{
    // I think the atomic restart.store with memory_order_seq_cst should cause
    // these mutations to become visible to streamThread.
    state.dir.assign(dir);
    state.startOffset = startOffset;
    state.mutes.assign(mutes.begin(), mutes.end());
    restart.store(true);
    // LOG("start: " << dir);
    ready.post();
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


static std::vector<string>
dirSamples(
    std::ostream &log, const string &dir, const std::vector<string> &mutes)
{
    std::vector<string> fnames;
    DIR *d = opendir(dir.c_str());
    if (!d) {
        LOG("can't open dir: " << dir);
        return fnames;
    }
    struct dirent *ent;
    while ((ent = readdir(d)) != nullptr) {
        if (ent->d_type != DT_DIR)
           continue;
        string subdir(ent->d_name);
        if (subdir.empty() || subdir[0] == '.')
            continue;
        if (suffixMatch(mutes, subdir.c_str()))
            continue;
        subdir = dir + "/" + subdir;
        LOG("play sample dir: " << subdir);
        fnames.push_back(subdir);
    }
    closedir(d);
    if (fnames.empty()) {
        LOG("no matching samples in " << dir);
    }
    return fnames;
}


void
Streamer::streamLoop()
{
    while (!threadQuit.load()) {
        LOG("streamLoop wait");
        ready.wait();
        if (threadQuit.load())
            break;
        if (restart.load()) {
            LOG("restart: " << state.dir);
            std::vector<string> dirnames =
                dirSamples(log, state.dir, state.mutes);
            mix.reset(new Mix(
                log, channels, sampleRate, dirnames, state.startOffset));
            // This is not safe, but since restart is true, read() shouldn't
            // touch it.
            jack_ringbuffer_reset(ring);
            restart.store(false);
            mixDone.store(false);
        }
        stream();
    }
}

void
Streamer::stream()
{
    bool done = false;
    while (!done) {
        // LOG("stream wait");
        ready.wait();
        // LOG("stream resume");
        if (restart.load() || threadQuit.load())
            break;
        float *buffer;
        sf_count_t available;
        while ((available = jack_ringbuffer_write_space(ring) / channels)
            > readFrames)
        {
            // LOG("stream avail " << available);
            done = mix->read(readFrames, &buffer);
            if (done)
                break;
            else
                jack_ringbuffer_write(ring, buffer, readFrames * channels);
        }
    }
    if (done)
        mixDone.store(true);
}


bool
Streamer::read(sf_count_t frames, float **out)
{
    size_t samples;
    if (restart.load()) {
        // This means streamLoop is restarting and will reset the ring.
        // So don't read stale samples, but also don't abort the play.
        samples = 0;
    } else {
        // Try to catch up.
        if (debt > 0) {
            size_t paid = jack_ringbuffer_read(
                ring, outputBuffer.data(), debt * channels);
            debt -= paid / channels;
            // LOG("discharge debt " << debt << " - " << (paid/channels));
        }
        samples = jack_ringbuffer_read(
            ring, outputBuffer.data(), frames * channels);
        if (samples == 0 && mixDone.load())
            return false;
    }
    debt += frames - (samples / channels);
    // LOG("read debt " << debt << " frames " << samples/channels);
    std::fill(outputBuffer.begin() + samples, outputBuffer.end(), 0);
    *out = outputBuffer.data();
    ready.post();
    return true;
}
