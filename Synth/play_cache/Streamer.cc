// Copyright 2018 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <ostream>
#include <string.h>
#include <thread>

#include "Resample.h"
#include "Streamer.h"
#include "Sample.h"
#include "Tracks.h"
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
        const char *name, std::ostream &log, int channels, int sampleRate,
        int maxFrames, bool synchronized)
    : channels(channels), sampleRate(sampleRate), maxFrames(maxFrames),
        name(name), log(log), threadQuit(false), audioDone(false),
        restarting(false), ready(0), synchronized(synchronized), debt(0)
{
    ring = jack_ringbuffer_create(ringBlocks * maxFrames * channels);
    jack_ringbuffer_mlock(ring);
    outputBuffer.resize(maxFrames * channels);

    streamThread.reset(new std::thread(&Streamer::streamLoop, this));
}


Streamer::~Streamer()
{
    LOG(name << ": stop");
    threadQuit.store(true);
    ready.post();
    streamThread->join();
    jack_ringbuffer_free(ring);
}


void
Streamer::restart()
{
    restarting.store(true);
    ready.post();
}


void
Streamer::streamLoop()
{
    while (!threadQuit.load()) {
        // LOG(name << ": streamLoop wait");
        ready.wait();
        // LOG(name << ": streamLoop resume");
        if (threadQuit.load())
            break;
        if (restarting.load()) {
            LOG(name << ": restarting");
            audio.reset(this->initialize());
            // This is not safe, but since restart is true, read() shouldn't
            // touch it.
            jack_ringbuffer_reset(ring);
            restarting.store(false);
            audioDone.store(false);
        }
        stream();
    }
}


// Fill up the ringbuffer.
void
Streamer::stream()
{
    sf_count_t available;
    while ((available = jack_ringbuffer_write_space(ring) / channels)
        > readFrames)
    {
        float *buffer;
        // If start() hasn't been called yet, audio hasn't been initialized.
        bool done =
            bool(audio) ? audio->read(channels, readFrames, &buffer) : true;
        // LOG(name << ": stream avail " << available << " done:" << done);
        if (done) {
            audioDone.store(true);
            break;
        } else {
            jack_ringbuffer_write(ring, buffer, readFrames * channels);
        }
    }
}


bool
Streamer::read(int channels, sf_count_t frames, float **out)
{
    size_t samples;
    if (restarting.load()) {
        // This means streamLoop is restarting and will reset the ring.
        // So don't read stale samples, but also don't abort the play.
        samples = 0;
    } else {
        // Try to catch up.
        if (synchronized && debt > 0) {
            size_t paid = jack_ringbuffer_read(
                ring, outputBuffer.data(), debt * channels);
            debt -= paid / channels;
            // LOG("discharge debt " << debt << " - " << (paid/channels));
        }
        samples = jack_ringbuffer_read(
            ring, outputBuffer.data(), frames * channels);
        if (samples == 0 && audioDone.load())
            return true;
    }
    debt += frames - (samples / channels);
    // LOG("read debt " << debt << " frames " << samples/channels);
    std::fill(outputBuffer.begin() + samples, outputBuffer.end(), 0);
    *out = outputBuffer.data();
    // Tell the stream thread there might be room for more samples.
    // If audioDone is true, then this will cause another stream() call even
    // though it will definitely not find any more samples.  So I could not
    // call ready.post() in that case, but I don't think a few extra stream()
    // checks hurt anything.
    ready.post();
    return false;
}


// TracksStreamer

TracksStreamer::TracksStreamer(
        std::ostream &log, int channels, int sampleRate, int maxFrames)
    : Streamer("tracks", log, channels, sampleRate, maxFrames, true)
{
    // Assume file path and number of muted tracks won't go above this, so
    // start() doesn't allocate.
    args.dir.reserve(4096);
    args.mutes.reserve(64);
}


void
TracksStreamer::start(const string &dir, sf_count_t startOffset,
    const std::vector<string> &mutes)
{
    // I think the atomic restarting.store with memory_order_seq_cst should
    // cause these mutations to become visible to streamThread.
    args.dir.assign(dir);
    args.startOffset = startOffset;
    args.mutes.assign(mutes.begin(), mutes.end());
    this->restart();
}


Audio *
TracksStreamer::initialize()
{
    // LOG("Tracks restart: " << args.dir);
    return new Tracks(
        log, channels, sampleRate, args.dir, args.startOffset, args.mutes);
}


// ResampleStreamer

ResampleStreamer::ResampleStreamer(
        std::ostream &log, int channels, int sampleRate, int maxFrames)
    : Streamer("osc", log, channels, sampleRate, maxFrames, false)
{
    fname.reserve(4096);
}

void
ResampleStreamer::start(const string &fname, double ratio)
{
    this->fname.assign(fname);
    this->ratio = ratio;
    this->restart();
}


void
ResampleStreamer::stop()
{
    this->fname.assign("");
    this->restart();
}


Audio *
ResampleStreamer::initialize()
{
    Audio *audio;
    if (fname.empty()) {
        audio = new AudioEmpty();
    } else {
        audio = new SampleFile(log, channels, true, sampleRate, fname, 0);
        if (ratio != 1)
            audio = new Resample(log, channels, ratio, audio);
    }
    return audio;
}
