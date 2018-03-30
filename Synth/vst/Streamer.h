// Copyright 2018 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

#include <atomic>
#include <string>
#include <memory>
#include <thread>
#include <vector>

#include "Mix.h"
#include "Semaphore.h"
#include "ringbuffer.h"


// Stream samples from disk.
//
// This has a realtime and a non-realtime API.  The class must be created in a
// non-realtime context, at which point it starts up streamThread which will
// handle non-realtime work.  The public methods should then be realtime-safe.
class Streamer {
public:
    Streamer(std::ostream &log, int channels, int sampleRate, int maxFrames);
    ~Streamer();

    // Thees functions are realtime-safe.
    void start(const std::string &dir, sf_count_t startOffset,
        const std::vector<std::string> &mutes);
    bool read(sf_count_t frames, float **out);

    const int channels;
    const int sampleRate;
    const int maxFrames;
private:
    // constant config
    std::ostream &log;

    // Statically allocated state to configure streamLoop().
    struct {
        std::string dir;
        sf_count_t startOffset;
        std::vector<std::string> mutes;
    } state;
    void streamLoop();
    void stream();
    std::unique_ptr<std::thread> streamThread;
    std::atomic<bool> threadQuit;

    // Set to true to have the streamThread reload mix.
    std::atomic<bool> restart;
    std::unique_ptr<Mix> mix;

    // ring needs more data.
    Semaphore ready;

    // read() state
    sf_count_t debt;
    jack_ringbuffer_t *ring;
    // Goes to true when the Mix has run out of data.
    std::atomic<bool> mixDone;
    std::vector<float> outputBuffer;
};
