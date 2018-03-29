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
    // frameSize = channels,

    // This many blockFrames*frameSize chunks in the ring.
    // jack_ringbuffer_create will round up to the next power of 2, so 4
    // should actually wind up being 8 - 1 byte.
    ringBlocks = 4
};


// static void
// showRing(const char *msg, jack_ringbuffer_t *ring)
// {
//     size_t r = jack_ringbuffer_read_space(ring);
//     size_t w = jack_ringbuffer_write_space(ring);
//     DEBUG(msg << ": read: " << r << " (" << r/frameSize << "), write: "
//         << w << " (" << w/frameSize << ")");
// }


Stream::Stream(
        std::ostream &log, int sampleRate, sf_count_t blockFrames,
        const std::string &fname, sf_count_t startOffset
    ) : blockFrames(blockFrames), quit(false)
{
    // +1 because the ringbuffer has one less byte available than allocated,
    // presumably to keep the pointers from touching.
    ring = jack_ringbuffer_create(ringBlocks * blockFrames * frameSize + 1);
    // showRing("init", ring);
    jack_ringbuffer_mlock(ring);
    outputBlock = new float[blockFrames * frameSize];
    start(log, sampleRate, fname, startOffset);
}


Stream::~Stream()
{
    stop();
    jack_ringbuffer_free(ring);
    delete[] outputBlock;
}


void
Stream::start(std::ostream &log, int sampleRate, const std::string &fname,
    sf_count_t startOffset)
{
    log << "stream start\n";
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

    log << "Stream: opened " << fname << '\n';
    if (startOffset > 0) {
        if (sf_seek(sndfile, startOffset, SEEK_SET) == -1) {
            // Likely the sample isn't that long.
            sf_close(sndfile);
            return;
        }
    }
    ready.post();
    // DEBUG(fname << ": start thread");
    this->streaming.reset(
        new std::thread(&Stream::stream, this, &log, sndfile));
}


void
Stream::stream(std::ostream *log, SNDFILE *sndfile)
{
    float block[blockFrames * channels];
    bool done = false;
    while (!done) {
        // *log << "wait\n";
        ready.wait();
        if (quit.load())
            break;
        sf_count_t blocks =
            jack_ringbuffer_write_space(ring) / frameSize / blockFrames;
        // *log << "stream blocks: " << blocks << "\n";
        // showRing("stream", ring);
        // DEBUG("stream blocks: " << blocks);
        while (!done && blocks--) {
            // TODO read could also fail, handle that
            sf_count_t read = sf_readf_float(sndfile, block, blockFrames);
            // DEBUG("stream write " << read);
            // *log << "stream write: " << read << "\n";
            jack_ringbuffer_write(
                ring, reinterpret_cast<char *>(block), read * frameSize);
            // jack_ringbuffer_write(ring, block, read * frameSize);
            // showRing("stream write", ring);
            if (read < blockFrames)
                done = true;
        }
    }
    sf_close(sndfile);
    // Tell the outside world I've completed.
    quit.store(true);
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
    // TODO return empty samples if stream() hasn't started yet
    // ASSERT(jack_ringbuffer_read_space(ring) >= frames * frameSize);
    if (jack_ringbuffer_read_space(ring) < frames * frameSize) {
        return 0;
    }
    // I think this float * -> char * cast is ok because the char * doesn't
    // have an alignment restriction.  The floats do, but memcpy should be able
    // to copy bytewise into a float array.
    //
    // TODO but I'm not sure.  If it's not ok, I could modify ringbuffer.c to
    // typedef the pointer type and change it to float.
    // DEBUG("reading " << frames*frameSize);
    // showRing("before read", ring);
    size_t bytes = jack_ringbuffer_read(
        ring, reinterpret_cast<char *>(outputBlock), frames * frameSize);
    // size_t bytes = jack_ringbuffer_read(
    //     ring, outputBlock, frames * frameSize);
    // showRing("after read", ring);
    *output = outputBlock;
    ready.post();
    return bytes / frameSize;
}
