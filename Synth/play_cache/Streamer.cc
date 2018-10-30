// Copyright 2018 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <ostream>
#include <string.h>
#include <thread>

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
        std::ostream &log, int channels, int sampleRate,
        int maxFrames, bool synchronized)
    : channels(channels), sampleRate(sampleRate), maxFrames(maxFrames),
        log(log), threadQuit(false), audioDone(false), restarting(false),
        synchronized(synchronized), debt(0)
{
    ring = jack_ringbuffer_create(ringBlocks * maxFrames * channels);
    jack_ringbuffer_mlock(ring);
    outputBuffer.resize(maxFrames * channels);

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
Streamer::restart()
{
    restarting.store(true);
    ready.post();
}


void
Streamer::streamLoop()
{
    while (!threadQuit.load()) {
        LOG("streamLoop wait");
        ready.wait();
        if (threadQuit.load())
            break;
        if (restarting.load()) {
            audio.reset(this->initialize());
            // This is not safe, but since restart is true, read() shouldn't
            // touch it.
            jack_ringbuffer_reset(ring);
            restarting.store(false);
            audioDone.store(false);
            // I just cued up a new Audio, so stream() should be able to
            // immediately read from it.
            ready.post();
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
        if (restarting.load() || threadQuit.load())
            break;
        float *buffer;
        sf_count_t available;
        while ((available = jack_ringbuffer_write_space(ring) / channels)
            > readFrames)
        {
            // LOG("stream avail " << available);
            // If start() hasn't been called yet, audio hasn't been initialized.
            done = bool(audio)
                ? audio->read(channels, readFrames, &buffer) : true;
            if (done)
                break;
            else
                jack_ringbuffer_write(ring, buffer, readFrames * channels);
        }
    }
    if (done)
        audioDone.store(true);
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
    ready.post();
    return false;
}


// TracksStreamer

TracksStreamer::TracksStreamer(
        std::ostream &log, int channels, int sampleRate, int maxFrames)
    : Streamer(log, channels, sampleRate, maxFrames, true)
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
    // LOG("restart: " << args.dir);
    return new Tracks(
        log, channels, sampleRate, args.dir, args.startOffset, args.mutes);
}


// ResampleStreamer

void
ResampleStreamer::start(const string &fname)
{
    this->fname = fname;
    this->restart();
}


void
ResampleStreamer::stop()
{
    this->fname = "";
    this->restart();
}


Audio *
ResampleStreamer::initialize()
{
    // TODO Resample(SampleFile(...))
    return new SampleFile(log, channels, sampleRate, fname, 0);
}
