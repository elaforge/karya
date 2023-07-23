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


class Flac : private FLAC::Decoder::File {
public:
    typedef const char *Error;
    typedef size_t Frames;

    static std::unique_ptr<Flac> open(const char *fname, Frames offset);
    ~Flac() { close(); } // probably ~File() already does this.
    Frames read(float *samples, Frames frames);
    void close() { finish(); }
    int channels() const { return _channels; };
    int srate() const { return _srate; };
    const char *error() const { return _error; };

private:
    Flac() : _error(nullptr), _srate(0), _channels(0), _bits(0),
        _total_samples(0)
    {}
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
