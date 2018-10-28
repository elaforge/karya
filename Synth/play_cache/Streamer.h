// Copyright 2018 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

#include <atomic>
#include <string>
#include <memory>
#include <thread>
#include <vector>

#include <sndfile.h>

#include "Mix.h"
#include "Semaphore.h"
#include "ringbuffer.h"


// Stream samples from disk.
//
// This has a realtime and a non-realtime API.  The class must be created in a
// non-realtime context, at which point it starts up streamThread which will
// handle non-realtime work.  The public methods, specifically read(), should
// then be realtime-safe.
class Streamer {
public:
    Streamer(std::ostream &log, int channels, int sampleRate, int maxFrames,
        bool synchronized);
    ~Streamer();

    // Thees functions are realtime-safe.
    void start(const std::string &dir, sf_count_t startOffset,
        const std::vector<std::string> &mutes);
    // Return true if the read is done, and there are no samples in 'out'.
    bool read(sf_count_t frames, float **out);

    const int channels;
    const int sampleRate;
    const int maxFrames;
private:
    std::ostream &log;
    // This is true for streaming from the cache and false for the OSC "MIDI
    // thru" mechanism.  For thru, it doesn't matter that samples are
    // synchronized to any start time, I just start streaming them when I get
    // them.
    const bool synchronized;

    // ** stream thread state
    void streamLoop();
    void stream();
    std::unique_ptr<std::thread> streamThread;
    std::unique_ptr<Mix> mix;

    // ** communication with streamThread.
    // Statically allocated state start() passes to streamLoop().
    struct {
        std::string dir;
        sf_count_t startOffset;
        std::vector<std::string> mutes;
    } state;
    std::atomic<bool> threadQuit;
    // Goes to true when the Mix has run out of data.
    std::atomic<bool> mixDone;
    // Set to true to have the streamThread reload mix.
    std::atomic<bool> restarting;
    jack_ringbuffer_t *ring;
    // ring needs more data.
    Semaphore ready;

    // ** read() state
    // Keep track if read() position gets ahead of what ring was able to
    // provide.
    sf_count_t debt;
    std::vector<float> outputBuffer;
};
