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


Stream::Stream(
        std::ostream &log, int sampleRate, sf_count_t blockFrames,
        const std::string &fname, sf_count_t startOffset
    ) : blockFrames(blockFrames)
{
    ring = jack_ringbuffer_create(ringSize * blockFrames * frameSize);
    block = new float[blockFrames * frameSize];
    start(log, sampleRate, fname, startOffset);
}


Stream::~Stream()
{
    stop();
    jack_ringbuffer_free(ring);
    delete[] block;
}


void
Stream::start(std::ostream &log, int sampleRate, const std::string &fname,
    sf_count_t startOffset)
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

    if (startOffset > 0) {
        if (sf_seek(sndfile, startOffset, SEEK_SET) == -1) {
            // Likely the sample isn't that long.
            sf_close(sndfile);
            return;
        }
    }
    for (int i = 0; i < ringSize; i++) {
        ready.post();
    }
    this->streaming.reset(new std::thread(&Stream::stream, this, sndfile));
}


void
Stream::stream(SNDFILE *sndfile)
{
    float block[blockFrames * channels];
    for (;;) {
        ready.wait();
        if (quit.load())
            break;
        // TODO read could also fail, handle that
        sf_count_t read = sf_readf_float(sndfile, block, blockFrames);
        if (read == 0) {
            break;
        }
        jack_ringbuffer_write(
            ring, reinterpret_cast<char *>(block), sizeof(block));
    }
    sf_close(sndfile);
}


void
Stream::stop()
{
    quit.store(true);
    ready.post();
    // Make sure it really quit before I delete the object.
    if (streaming.get())
        streaming->join();
}

sf_count_t
Stream::read(sf_count_t frames, float **output)
{
    // I think this float * -> char * cast is ok because the char * doesn't
    // have an alignment restriction.  The floats do, but memcpy should be able
    // to copy bytewise into a float array.
    //
    // TODO but I'm not sure.  If it's not ok, I could modify ringbuffer.c to
    // typedef the pointer type and change it to float.
    *output = block;
    sf_count_t read = jack_ringbuffer_read(
        ring, reinterpret_cast<char *>(&block), frames * frameSize);
    ready.post();
    return read / frameSize;
}
