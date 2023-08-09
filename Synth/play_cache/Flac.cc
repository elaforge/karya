// Copyright 2023 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt


#include <memory>
#include <stdio.h>
#include <sys/errno.h>

#include "Flac.h"

#include <fltk/util.h>


static inline float
to_float(FLAC__int32 i, float max)
{
    return static_cast<float>(i) / max;
}

FLAC__StreamDecoderWriteStatus
Flac::write_callback(
    const FLAC__Frame *frame, const FLAC__int32 *const channel[])
{
    size_t samples = frame->header.blocksize;
    size_t end = buffer.size();
    buffer.resize(end + samples * 2);
    // There's no standard way to convert to float, but using 2^(bits-1)
    // matches libsndfile.  It lets us reach -1 but not quite +1.
    float max = 1 << (bits() - 1);
    // interleave samples, convert to float
    if (channels() == 1) {
        for (size_t i = 0; i < samples; i++) {
            buffer[end + i*2] = to_float(channel[0][i], max);
            buffer[end + i*2 + 1] = to_float(channel[0][i], max);
        }
    } else if (channels() == 2) {
        for (size_t i = 0; i < samples; i++) {
            buffer[end + i*2] = to_float(channel[0][i], max);
            buffer[end + i*2 + 1] = to_float(channel[1][i], max);
        }
    }
    return FLAC__STREAM_DECODER_WRITE_STATUS_CONTINUE;
}

void Flac::metadata_callback(const FLAC__StreamMetadata *metadata)
{
    if (metadata->type == FLAC__METADATA_TYPE_STREAMINFO) {
        _srate = metadata->data.stream_info.sample_rate;
        _channels = metadata->data.stream_info.channels;
        _bits = metadata->data.stream_info.bits_per_sample;
        _total_samples = metadata->data.stream_info.total_samples;
    }
}

void
Flac::error_callback(FLAC__StreamDecoderErrorStatus status)
{
    this->_error = FLAC__StreamDecoderErrorStatusString[status];
}

Flac::Flac(const char *fname, Frames offset)
    : _error(nullptr), _srate(0), _channels(0), _bits(0), _total_samples(0)
{
    set_md5_checking(false); // don't care if it got corrupted.
    FLAC__StreamDecoderInitStatus init_status = init(fname);
    if (init_status != FLAC__STREAM_DECODER_INIT_STATUS_OK) {
        _error = FLAC__StreamDecoderInitStatusString[init_status];
        return;
    }
    process_until_end_of_metadata();
    if (error())
        return;
    if (offset >= _total_samples) {
        close();
    } else if (!seek_absolute(offset)) {
        _error = FLAC__StreamDecoderStateString[get_state()];
    }
}

// If it returns < frames, then the file ended.
//
// If I want to directly return the buffer I'd have to return less.
// But the caller requires a buffer of the right size, so it would have
// to patch it together anyway, so may as well do that here.  As long as
// I'm going to do it some of the time, may as well do it all the time,
// and copying is cheap.  Even though both frame sizes are likely to be
// powers of 2, and hence exactly divisible.
Flac::Frames
Flac::read(float *samples, Flac::Frames frames)
{
    if (get_state() == FLAC__STREAM_DECODER_UNINITIALIZED)
        return 0; // already closed, due to seek past the end
    // DEBUG("read " << frames << " buf " << buffer.size() << " state "
    //     << FLAC__StreamDecoderStateString[get_state()]);
    while (buffer.size() < frames * 2
        && get_state() == FLAC__STREAM_DECODER_SEARCH_FOR_FRAME_SYNC)
    {
        if (!process_single())
            break;
    }
    // There is also FLAC__STREAM_DECODER_READ_FRAME, but it doesn't seem to
    // happen outside the callbacks.
    if (get_state() != FLAC__STREAM_DECODER_END_OF_STREAM
        && get_state() != FLAC__STREAM_DECODER_SEARCH_FOR_FRAME_SYNC)
    {
        _error = FLAC__StreamDecoderStateString[get_state()];
        return 0;
    }
    // copy frames to *samples
    // delete frames from front of buffer
    Flac::Frames read = std::min(frames, buffer.size() / 2);
    memcpy(samples, buffer.data(), sizeof(float) * read * 2);
    buffer.erase(buffer.begin(), buffer.begin() + read * 2);
    return read;
}
