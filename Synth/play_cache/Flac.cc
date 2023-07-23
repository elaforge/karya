// Copyright 2023 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt


#include <stdio.h>
#include <sys/errno.h>

#include "Flac.h"

#include <fltk/util.h>


static inline float
to_float(FLAC__int16 i)
{
    // This is chosen to match libsndfile.  This lets us reach -1, but not
    // quite +1.
    return static_cast<float>(i) / 32768;
}

FLAC__StreamDecoderWriteStatus
Flac::write_callback(
    const FLAC__Frame *frame, const FLAC__int32 *const channel[])
{
    size_t samples = frame->header.blocksize;
    size_t end = buffer.size();
    buffer.resize(end + samples * 2);
    // interleave samples, convert to float
    // TODO flac gives me int32 but its example assumes int16 samples?
    if (_channels == 1) {
        for (size_t i = 0; i < samples; i++) {
            buffer[end + i*2] = to_float(channel[0][i]);
            buffer[end + i*2 + 1] = to_float(channel[0][i]);
        }
    } else if (_channels == 2) {
        for (size_t i = 0; i < samples; i++) {
            buffer[end + i*2] = to_float(channel[0][i]);
            buffer[end + i*2 + 1] = to_float(channel[1][i]);
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
    this->error = FLAC__StreamDecoderErrorStatusString[status];
}

Flac::Error
Flac::open(const char *fname, Flac **flacp, Frames offset)
{
    *flacp = nullptr;

    // TODO unique_ptr to ensure deletion
    // why not hand it to the caller as unique_ptr?
    Flac *flac = new Flac();
    if (!flac->is_valid()) {
        DEBUG("invalid");
        return FLAC__StreamDecoderStateString[flac->get_state()];
    }
    flac->set_md5_checking(false); // don't care if it got corrupted.
    FLAC__StreamDecoderInitStatus init_status = flac->init(fname);
    if (init_status != FLAC__STREAM_DECODER_INIT_STATUS_OK) {
        return FLAC__StreamDecoderInitStatusString[init_status];
    }
    flac->process_until_end_of_metadata();
    if (flac->_bits != 16) {
        return "not 16 bit";
    }
    if (!(flac->_channels == 1 || flac->_channels == 2)) {
        return "not 1 or 2 channels";
    }

    if (offset >= flac->_total_samples) {
        flac->close();
    } else if (!flac->seek_absolute(offset)) {
        return FLAC__StreamDecoderStateString[flac->get_state()];
    }
    *flacp = flac;
    return nullptr;
}

// if it returns < frames, then the file ended
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
        DEBUG("ERROR: " << FLAC__StreamDecoderStateString[get_state()]);
    }
    // copy frames to *samples
    // delete frames from front of buffer
    Flac::Frames read = std::min(frames, buffer.size() / 2);
    memcpy(samples, buffer.data(), sizeof(float) * read * 2);
    buffer.erase(buffer.begin(), buffer.begin() + read * 2);
    return read;
}

Flac::Error
Flac::close()
{
    this->finish();
}
