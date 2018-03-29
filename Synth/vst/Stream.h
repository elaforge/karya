// Copyright 2018 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

#include <atomic>
#include <memory>
#include <string>

#include <sndfile.h>

#include "Semaphore.h"
#include "ringbuffer.h"


// Stream a sample from a separate thread.  read() should be realtime-safe.
class Stream {
public:
    Stream(sf_count_t blockFrames);
    ~Stream();

    // Start the streaming thread.  It will fill up the ring buffer and keep
    // it full as long as read() is called.  An error with the file will be
    // logged and read() will return nullptr.
    void start(std::ostream &log, int sampleRate, const std::string &fname);
    // Stop the streaming thread.  Call before destruction.
    void stop();

    // Return a pointer to blockFrames of frames, or nullptr if there are
    // none left.  There is no in-between because the buffer will be 0 padded.
    float *read();

private:
    void stream(SNDFILE *sndfile);
    // read's wantedFrames will never exceed this value.
    const sf_count_t blockFrames;
    Semaphore ready;
    // Signal to quit the streaming thread.
    std::atomic<bool> quit;

    // This is always written to and read from in chunks of blockFrames *
    // frameSize.
    jack_ringbuffer_t *ring;
};
