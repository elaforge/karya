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
    // This many max_frames in the ring.
    // jack_ringbuffer_create will round up to the next power of 2.
    ring_blocks = 4,
    // Read this many frames at a time.  Should be smaller than ring_blocks *
    // max_frames!
    read_frames = 512
};


Streamer::Streamer(
        const char *name, std::ostream &log, int channels, int sample_rate,
        int max_frames, bool synchronized)
    : channels(channels), sample_rate(sample_rate), max_frames(max_frames),
        name(name), log(log), thread_quit(false), audio_done(false),
        restarting(false), ready(0), synchronized(synchronized), debt(0)
{
    ring = jack_ringbuffer_create(ring_blocks * max_frames * channels);
    jack_ringbuffer_mlock(ring);
    output_buffer.resize(max_frames * channels);

    stream_thread.reset(new std::thread(&Streamer::stream_loop, this));
}


Streamer::~Streamer()
{
    LOG(name << ": stop");
    thread_quit.store(true);
    ready.post();
    stream_thread->join();
    jack_ringbuffer_free(ring);
}


void
Streamer::restart()
{
    restarting.store(true);
    ready.post();
}


void
Streamer::stream_loop()
{
    while (!thread_quit.load()) {
        // LOG(name << ": stream_loop wait");
        ready.wait();
        // LOG(name << ": stream_loop resume");
        if (thread_quit.load())
            break;
        if (restarting.load()) {
            // LOG(name << ": restarting");
            audio.reset(this->initialize());
            // This is not safe, but since restart is true, read() shouldn't
            // touch it.
            jack_ringbuffer_reset(ring);
            restarting.store(false);
            audio_done.store(false);
        }
        stream();
    }
}


// Fill up the ringbuffer.
void
Streamer::stream()
{
    Frames available;
    while ((available = jack_ringbuffer_write_space(ring) / channels)
        > read_frames)
    {
        float *buffer;
        // If start() hasn't been called yet, audio hasn't been initialized.
        bool done =
            bool(audio) ? audio->read(channels, read_frames, &buffer) : true;
        // LOG(name << ": stream avail " << available << " done:" << done);
        if (done) {
            audio_done.store(true);
            break;
        } else {
            jack_ringbuffer_write(ring, buffer, read_frames * channels);
        }
    }
}


bool
Streamer::read(int channels, Frames frames, float **out)
{
    size_t samples;
    if (restarting.load()) {
        // This means stream_loop is restarting and will reset the ring.
        // So don't read stale samples, but also don't abort the play.
        samples = 0;
    } else {
        // Try to catch up.
        if (synchronized && debt > 0) {
            size_t paid = jack_ringbuffer_read(
                ring, output_buffer.data(), debt * channels);
            debt -= paid / channels;
            // LOG("discharge debt " << debt << " - " << (paid/channels));
        }
        samples = jack_ringbuffer_read(
            ring, output_buffer.data(), frames * channels);
        if (samples == 0 && audio_done.load())
            return true;
    }
    debt += frames - (samples / channels);
    // LOG("read debt " << debt << " frames " << samples/channels);
    std::fill(output_buffer.begin() + samples, output_buffer.end(), 0);
    *out = output_buffer.data();
    // Tell the stream thread there might be room for more samples.
    // If audio_done is true, then this will cause another stream() call even
    // though it will definitely not find any more samples.  So I could not
    // call ready.post() in that case, but I don't think a few extra stream()
    // checks hurt anything.
    ready.post();
    return false;
}


// TracksStreamer

TracksStreamer::TracksStreamer(
        std::ostream &log, int channels, int sample_rate, int max_frames)
    : Streamer("tracks", log, channels, sample_rate, max_frames, true)
{
    // Assume file path and number of muted tracks won't go above this, so
    // start() doesn't allocate.
    args.dir.reserve(4096);
    args.mutes.reserve(64);
}


void
TracksStreamer::start(const string &dir, Frames start_offset,
    const std::vector<string> &mutes)
{
    // I think the atomic restarting.store with memory_order_seq_cst should
    // cause these mutations to become visible to stream_thread.
    args.dir.assign(dir);
    args.start_offset = start_offset;
    args.mutes.assign(mutes.begin(), mutes.end());
    this->restart();
}


Audio *
TracksStreamer::initialize()
{
    // LOG("Tracks restart: " << args.dir);
    return new Tracks(
        log, channels, sample_rate, args.dir, args.start_offset, args.mutes);
}


// ResampleStreamer

ResampleStreamer::ResampleStreamer(
        std::ostream &log, int channels, int sample_rate, int max_frames)
    : Streamer("thru", log, channels, sample_rate, max_frames, false)
{
    fname.reserve(4096);
}

void
ResampleStreamer::start(const string &fname, int64_t offset, double ratio)
{
    this->fname.assign(fname);
    this->offset = offset;
    this->ratio = ratio;
    this->restart();
}


void
ResampleStreamer::stop()
{
    if (!fname.empty()) {
        this->fname.assign("");
        this->restart();
    }
}


Audio *
ResampleStreamer::initialize()
{
    Audio *audio;
    if (fname.empty()) {
        audio = new AudioEmpty();
    } else {
        audio = new SampleFile(log, channels, true, sample_rate, fname, offset);
        if (ratio != 1)
            audio = new Resample(log, channels, ratio, audio);
    }
    return audio;
}


// MixStreamer

MixStreamer::MixStreamer(
        int max_voices, std::ostream &log, int channels, int sample_rate,
        int max_frames)
{
    for (int i = 0; i < max_voices; i++) {
        std::unique_ptr<ResampleStreamer> p(
            new ResampleStreamer(log, channels, sample_rate, max_frames));
        this->voices.push_back(std::move(p));
        this->volumes.push_back(1);
    }
    // Ensure read() doesn't have to allocate.
    buffer.resize(channels * max_frames);
}


void
MixStreamer::start(int voice, const std::string &fname, int64_t offset,
    double ratio, float volume)
{
    voice = voice % voices.size();
    voices[voice]->start(fname, offset, ratio);
    volumes[voice] = volume;
}


void
MixStreamer::stop()
{
    for (auto &streamer : voices) {
        streamer->stop();
    }
}


// Below is copy pasted from Tracks.cc.  I experimented with a reusable mixer,
// but it's a bit annoying because it needs vector<Audio> to be generic, but
// this is vector<Streamer>, and I don't know how to do covariant downcasting
// in C++, so I guess it could be a template, but it's too much bother.

static void
mix(int channels, Frames frames,
    // In theory restrict lets it optimize better because it knows there's no
    // dependency between the pointers.
    float * __restrict__ output, const float * __restrict__ input)
{
    for (Frames i = 0; i < frames * channels; i++) {
        output[i] += input[i];
    }
}


bool
MixStreamer::read(int channels, Frames frames, float **out)
{
    buffer.resize(frames * channels);
    std::fill(buffer.begin(), buffer.end(), 0);
    bool done = true;
    int voice = 0;
    for (const auto &audio : voices) {
        float *s_buffer;
        if (!audio->read(channels, frames, &s_buffer)) {
            if (volumes[voice] != 1) {
                for (int i = 0; i < channels * frames; i++) {
                    s_buffer[i] *= volumes[voice];
                }
            }
            mix(channels, frames, buffer.data(), s_buffer);
            done = false;
        }
        voice++;
    }
    *out = buffer.data();
    return done;
}
