// Copyright 2018 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

#include <atomic>
#include <memory>
#include <string>
#include <thread>

#include <sndfile.h>

#include "Semaphore.h"
#include "ringbuffer.h"


// Stream a sample from a separate thread.  read() should be realtime-safe.
class Stream {
public:
    Stream(std::ostream &log, int sampleRate, sf_count_t blockFrames,
        const std::string &fname, sf_count_t startOffset);
    ~Stream();

    // Put read samples in the output, and return frames read.
    sf_count_t read(sf_count_t frames, float **output);

private:
    void stream(std::ostream *log, SNDFILE *sndfile);
    std::unique_ptr<std::thread> streaming;
    // Start the streaming thread.  It will fill up the ring buffer and keep
    // it full as long as read() is called.  Erorrs are logged.
    void start(std::ostream &log, int sampleRate, const std::string &fname,
        sf_count_t startOffset);
    void stop();

    // read's wantedFrames will never exceed this value.
    const sf_count_t blockFrames;
    Semaphore ready;
    // Signal to quit the streaming thread.
    std::atomic<bool> quit;

    // This is always written to and read from in chunks of blockFrames *
    // frameSize.
    jack_ringbuffer_t *ring;
    float *block;
};
