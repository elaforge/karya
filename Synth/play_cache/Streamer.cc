// Copyright 2018 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <algorithm>
#include <chrono>
#include <ostream>
#include <string.h>
#include <thread>

#ifdef __APPLE__
#include <pthread.h>
#include <sys/qos.h>
#endif

#include "Synth/Shared/config.h"

#include "Resample.h"
#include "Streamer.h"
#include "SampleFile.h"
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
        restarting(false), restart_ns(0), ready(0),
        synchronized(synchronized), debt(0), debt_max(0)
{
    // start_offset only reaches 0, and so read() only asks for a nonzero
    // number of frames, on the process() call *after* the one that started the
    // play.  If the latency is less than a block, read() gets called in the
    // same process() that posted the restart, microseconds later, and takes on
    // debt before the stream thread has run at all.
    if (synchronized && max_frames > START_LATENCY_FRAMES) {
        LOG(name << ": host block size " << max_frames
            << " is larger than START_LATENCY_FRAMES " << START_LATENCY_FRAMES
            << ", so the stream thread gets no time at all, and plays will"
            << " start up to a block late depending on where the NoteOn falls"
            << " in the block.  START_LATENCY_FRAMES should be a multiple of"
            << " the block size.");
    }

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
    // Called from the audio thread.  steady_clock is mach_absolute_time on OSX,
    // so no syscall and no lock.
    restart_ns.store(
        std::chrono::steady_clock::now().time_since_epoch().count(),
        std::memory_order_relaxed);
    restarting.store(true);
    ready.post();
}


std::chrono::steady_clock::time_point
Streamer::restart_posted() const
{
    return std::chrono::steady_clock::time_point(
        std::chrono::steady_clock::duration(
            restart_ns.load(std::memory_order_relaxed)));
}


// Milliseconds since 'start', for logging how long startup takes.
static double
ms_since(std::chrono::steady_clock::time_point start)
{
    return std::chrono::duration<double, std::milli>(
        std::chrono::steady_clock::now() - start).count();
}


void
Streamer::stream_loop()
{
#ifdef __APPLE__
    // This is not the audio thread, which is why it can do IO, but we
    // still want to get it done ASAP, because we only have block size to do it.
    pthread_set_qos_class_self_np(QOS_CLASS_USER_INITIATED, 0);
    // pthread_set_qos_class_self_np(QOS_CLASS_USER_INTERACTIVE, 0);
#endif
    Frames logged_debt = 0;
    while (!thread_quit.load()) {
        // LOG(name << ": stream_loop wait");
        ready.wait();
        // LOG(name << ": stream_loop resume");
        if (thread_quit.load())
            break;
        if (restarting.load()) {
            // LOG(name << ": restarting");
            const auto woke = std::chrono::steady_clock::now();
            audio.reset(this->initialize());
            // This is not safe, but since restart is true, read() shouldn't
            // touch it.
            jack_ringbuffer_reset(ring);
            audio_done.store(false);
            debt_max.store(0);
            logged_debt = 0;
            const double init_ms = ms_since(woke);
            // Fill the ring *before* clearing restarting, otherwise read() can
            // see restarting false with an empty ring, and take on debt that
            // it then pays by discarding the beginning of the play.
            stream();
            const double fill_ms = ms_since(woke);
            restarting.store(false);
            // Measure from when the audio thread posted, not from when this
            // thread woke up: scheduling latency counts against the budget
            // just as much as the work does, and it's the part that varies.
            const double wake_ms = std::chrono::duration<double, std::milli>(
                woke - restart_posted()).count();
            // The deadline is how many process() calls go by before read()
            // first asks for a nonzero number of frames.  A partial block at
            // the end is no use, since read() is called at the start of the
            // block that contains it, and the NoteOn's offset within its block
            // only ever makes this larger, so this is the worst case.
            const Frames budget =
                (START_LATENCY_FRAMES / max_frames) * max_frames;
            LOG(name << ": wakeup " << wake_ms << "ms, initialize " << init_ms
                << "ms, first fill " << fill_ms << "ms; budget "
                << (budget * 1000.0 / sample_rate) << "ms (" << budget
                << " frames) from latency " << START_LATENCY_FRAMES
                << " and host block " << max_frames);
            continue;
        }
        stream();
        // Nonzero means samples were dropped to stay in sync, i.e.
        // START_LATENCY_FRAMES is too low.
        const Frames debt = debt_max.load();
        if (debt > logged_debt) {
            logged_debt = debt;
            LOG(name << ": underrun, dropped " << debt << " frames ("
                << (debt * 1000.0 / sample_rate) << "ms)");
        }
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
        // Debt is paid by discarding samples, so any debt still around when
        // the new play starts comes straight off its attack.  There are two
        // sources: the previous play, which always ends with a partial read as
        // the ring drains, and this restart window itself.  Neither one says
        // anything about whether the new play is keeping up, so clear it.
        debt = 0;
    } else {
        // Try to catch up.
        if (synchronized && debt > 0) {
            // output_buffer only has room for max_frames, while debt can grow
            // past that, and the ring can have more than that available.
            size_t paid = jack_ringbuffer_read(
                ring, output_buffer.data(),
                std::min(debt, Frames(max_frames)) * channels);
            debt -= paid / channels;
            // LOG("discharge debt " << debt << " - " << (paid/channels));
        }
        samples = jack_ringbuffer_read(
            ring, output_buffer.data(), frames * channels);
        if (samples == 0 && audio_done.load())
            return true;
        debt += frames - (samples / channels);
        // Unsynchronized streamers never discharge debt, so it's not
        // meaningful.
        if (synchronized && debt > debt_max.load(std::memory_order_relaxed))
            debt_max.store(debt, std::memory_order_relaxed);
    }
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
        audio = new SampleFile(log, channels, sample_rate, fname, offset);
        if (ratio != 1)
            audio = new Resample(log, channels, ratio, audio);
    }
    return audio;
}


// MixStreamer

MixStreamer::MixStreamer(
        int max_voices, std::ostream &log, int channels, int sample_rate,
        int max_frames)
    : fading(false), fade_remaining(0)
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
MixStreamer::stop_fade()
{
    if (voices.size() == 0)
        return;
    if (!fading) {
        fade_remaining = fade_sec * voices[0]->sample_rate;
        fading = true;
    }
}

void
MixStreamer::stop()
{
    for (auto &streamer : voices) {
        streamer->stop();
    }
    fading = false;
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
    for (int voice = 0; voice < voices.size(); voice++) {
        const auto &audio = voices[voice];
        float *s_buffer;
        if (!audio->read(channels, frames, &s_buffer)) {
            float volume = volumes[voice];
            const float fade_dur = fade_sec * audio->sample_rate;
            if (volume != 1 || fading) {
                const Frames remaining = fade_remaining.load();
                for (Frames frame = 0; frame < frames; frame++) {
                    float fade = 1;
                    if (fading) {
                        fade = static_cast<float>(
                            remaining - std::min(remaining, frame)) / fade_dur;
                    }
                    for (Frames c = 0; c < channels; c++) {
                        s_buffer[frame * channels + c] *= volume * fade;
                    }
                }
            }
            mix(channels, frames, buffer.data(), s_buffer);
            done = false;
        }
    }
    if (fading) {
        if (fade_remaining > 0) {
            fade_remaining -= std::min(fade_remaining.load(), frames);
        } else {
            fading = false;
            for (auto &audio : voices)
                audio->stop();
        }
    }
    *out = buffer.data();
    return done;
}
