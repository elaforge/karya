// Copyright 2023 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

// Simple flac support.
//
// This only supports reading, and float format.

#pragma once

#include <memory>
#include <vector>
#include <FLAC++/decoder.h>

#include "AudioFile.h"


class Flac : private FLAC::Decoder::File, public AudioFile {
public:
    typedef size_t Frames;

    Flac(const char *fname, Frames offset);
    ~Flac() { close(); } // probably ~File() already does this.
    Frames read(float *samples, Frames frames) override;
    void close() override { finish(); }
    int channels() const override { return _channels; };
    int srate() const override { return _srate; };
    int bits() const override { return _bits; };
    const char *error() const override { return _error; };

private:
    FLAC__StreamDecoderWriteStatus write_callback(
        const FLAC__Frame *frame, const FLAC__int32 *const buffer[]
    ) override;

    void metadata_callback(const FLAC__StreamMetadata *metadata) override;
    void error_callback(FLAC__StreamDecoderErrorStatus status) override;
    const char *_error;
    int32_t _srate;
    int32_t _channels;
    int32_t _bits;
    int64_t _total_samples;

    std::vector<float> buffer;
};
