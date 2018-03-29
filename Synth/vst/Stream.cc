// Copyright 2018 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <memory>
#include <sstream>
#include <thread>

#include <sndfile.h>

#include "Stream.h"
#include "Semaphore.h"

// TODO for ASSERT, but I should have a better place for it
#include "fltk/util.h"


enum {
    channels = 2,
    frameSize = sizeof(float) * channels,
    // This many blockFrames*frameSize chunks in the ring.
    ringSize = 8
};


Stream::Stream(sf_count_t blockFrames) : blockFrames(blockFrames)
{
    ring = jack_ringbuffer_create(ringSize * blockFrames * frameSize);
}


Stream::~Stream()
{
    jack_ringbuffer_free(ring);
}


void
Stream::start(std::ostream &log, int sampleRate,
    const std::string &fname)
{
    SF_INFO info;
    SNDFILE *sndfile = sf_open(fname.c_str(), SFM_READ, &info);
    std::stringstream errors;
    if (sf_error(sndfile) != SF_ERR_NO_ERROR) {
    } else if (info.channels != channels) {
        errors << "expected " << channels << " channels, got " << info.channels;
    } else if (info.samplerate != sampleRate) {
        errors << "expected srate of " << sampleRate << ", got "
            << info.samplerate;
    }
    if (!errors.str().empty()) {
        log << fname << ": " << errors.str() << std::endl;
        sf_close(sndfile);
        return;
    }

    for (int i = 0; i < ringSize; i++) {
        ready.post();
    }
    std::thread(&Stream::stream, this, sndfile);
}


void
Stream::stream(SNDFILE *sndfile)
{
    // It wants 2 for when the read wraps around, but that shouldn't happen
    // because I read and write in multiples of blockFrames.
    jack_ringbuffer_data_t vec[2];
    for (;;) {
        ready.wait();
        if (quit.load())
            break;
        jack_ringbuffer_get_write_vector(ring, vec);
        sf_count_t read = sf_readf_float(
            sndfile, reinterpret_cast<float *>(vec[0].buf), blockFrames);
        if (read == 0) {
            break;
        } else if (read < blockFrames) {
            memset(vec[0].buf + read * frameSize, 0,
                (blockFrames - read) * frameSize);
        }
        jack_ringbuffer_write_advance(ring, blockFrames * frameSize);
    }
    sf_close(sndfile);
}


void
Stream::stop()
{
    quit.store(true);
    ready.post();
}


float *
Stream::read()
{
    jack_ringbuffer_data_t vec[2];
    jack_ringbuffer_get_read_vector(ring, vec);

    if (vec[0].len == 0) {
        return nullptr;
    } else {
        ASSERT(vec[0].len == blockFrames * frameSize);
    }
    float *frames = reinterpret_cast<float *>(vec[0].buf);
    jack_ringbuffer_read_advance(ring, blockFrames * frameSize);
    ready.post();
    return frames;
}
