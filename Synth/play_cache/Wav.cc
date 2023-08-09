// Copyright 2021 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <arpa/inet.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/errno.h>

#include "Wav.h"

#include <fltk/util.h>


// Reference from http://soundfile.sapp.org/doc/WaveFormat/

struct __attribute__((__packed__)) RiffHeader {
    uint32_t id;
    uint32_t size;
    uint32_t format;
};

struct __attribute__((__packed__)) Fmt {
    uint16_t format;
    uint16_t channels;
    uint32_t srate;
    uint32_t byte_rate;
    uint16_t block_align;
    uint16_t bits;
};

enum Format {
    PCM = 1,
    FLOAT32 = 3
};

struct __attribute__((__packed__)) ChunkHeader {
    uint32_t id;
    uint32_t size;
};

static bool
find_chunk(uint32_t id, FILE *fp)
{
    for (;;) {
        ChunkHeader chunk;
        if (fread(&chunk, sizeof(ChunkHeader), 1, fp) != 1) {
            return false;
        } else if (chunk.id == htonl(id)) {
            return true;
        } else {
            if (chunk.size == 0 || fseek(fp, chunk.size, SEEK_CUR) != 0)
                return false;
        }
    }
}

Wav::Wav(const char *fname, Frames offset) : _error(nullptr)
{
    fp = fopen(fname, "rb");
    if (fp == nullptr) {
        _error = strerror(errno);
        return;
    }
    RiffHeader riff;
    if (fread(&riff, sizeof(RiffHeader), 1, fp) != 1) {
        _error = strerror(errno);
        return;
    }
    if (riff.id != htonl('RIFF') || riff.format != htonl('WAVE')) {
        _error = "Not a wav file";
        return;
    }

    if (!find_chunk('fmt ', fp)) {
        _error = "didn't find fmt chunk";
        return;
    }
    Fmt fmt;
    static_assert(sizeof(Fmt) == 16, "sizeof(Fmt) == 16");
    if (fread(&fmt, sizeof(Fmt), 1, fp) != 1) {
        _error = strerror(errno);
        return;
    }
    // DEBUG("format: " << fmt.format << " chan:" << fmt.channels
    //     << " srate:" << fmt.srate << " brate:" << fmt.byte_rate
    //     << " block_align:" << fmt.block_align << " bits:" << fmt.bits);
    if (fmt.format != FLOAT32) {
        _error = "Not a float32 wav";
        return;
    }
    if (!find_chunk('data', fp)) {
        _error = "can't find data chunk";
        return;
    }
    if (offset > 0) {
        // TODO I used to check if it's an unexpected large seek, should I?
        // There is a special case where 0 frames is like a full chunk of 0s.
        if (fseek(fp, sizeof(float) * fmt.channels * offset, SEEK_CUR) != 0) {
            _error = strerror(errno);
            return;
        }
    }
    _channels = fmt.channels;
    _srate = fmt.srate;
}

void
Wav::close()
{
    if (fclose(this->fp) != 0) {
        // Well?
    }
    this->fp = nullptr;
}

Wav::Frames
Wav::read(float *samples, Wav::Frames frames)
{
    return fread(samples, sizeof(float) * this->channels(), frames, this->fp);
}
