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

Wav::Error
Wav::open(const char *fname, Wav **wav, Frames offset)
{
    *wav = nullptr;
    FILE *fp = fopen(fname, "rb");
    if (fp == nullptr) {
        return strerror(errno);
    }
    RiffHeader riff;
    if (fread(&riff, sizeof(RiffHeader), 1, fp) != 1)
        goto on_c_error;
    if (riff.id != htonl('RIFF') || riff.format != htonl('WAVE')) {
        fclose(fp);
        return "Not a wav file";
    }

    if (!find_chunk('fmt ', fp))
        goto on_c_error;
    Fmt fmt;
    static_assert(sizeof(Fmt) == 16, "sizeof(Fmt) == 16");
    if (fread(&fmt, sizeof(Fmt), 1, fp) != 1)
        goto on_c_error;
    // DEBUG("format: " << fmt.format << " chan:" << fmt.channels
    //     << " srate:" << fmt.srate << " brate:" << fmt.byte_rate
    //     << " block_align:" << fmt.block_align << " bits:" << fmt.bits);
    if (fmt.format != FLOAT32) {
        fclose(fp);
        return "Not a float32 wav";
    }
    if (!find_chunk('data', fp))
        goto on_c_error;
    if (offset > 0) {
        // TODO I used to check if it's an unexpected large seek, should I?
        // There is a special case where 0 frames is like a full chunk of 0s.
        if (fseek(fp, sizeof(float) * fmt.channels * offset, SEEK_CUR) != 0)
            goto on_c_error;
    }

    *wav = new Wav(fp, fmt.channels, fmt.srate);
    return nullptr;

on_c_error:
    fclose(fp);
    return strerror(errno);
}

Wav::~Wav()
{
    fclose(fp);
}

Wav::Error
Wav::close()
{
    if (fclose(this->fp) != 0) {
        return strerror(errno);
    }
    this->fp = nullptr;
    return nullptr;
}

Wav::Frames
Wav::read(float *samples, Wav::Frames frames)
{
    return fread(samples, sizeof(float) * this->channels(), frames, this->fp);
}
