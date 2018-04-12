// Copyright 2018 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <sndfile.h>
#include <ostream>
#include <sstream>

#include "Sample.h"


Sample::Sample(
        std::ostream &log, int channels, int sampleRate,
        const std::string &fname, sf_count_t startOffset)
    : channels(channels)
{
    SF_INFO info;
    this->sndfile = sf_open(fname.c_str(), SFM_READ, &info);
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
        sndfile = nullptr;
        return;
    }

    if (startOffset > 0) {
        if (sf_seek(sndfile, startOffset, SEEK_SET) == -1) {
            // Likely the sample isn't that long.
            sf_close(sndfile);
            sndfile = nullptr;
        }
    }
}

Sample::~Sample()
{
    if (sndfile)
        sf_close(sndfile);
}

sf_count_t
Sample::read(sf_count_t frames, float **out)
{
    if (!sndfile)
        return 0;
    buffer.resize(frames * channels);
    sf_count_t read = sf_readf_float(sndfile, buffer.data(), frames);
    // TODO read could fail, handle that
    if (read < frames) {
        sf_close(sndfile);
        sndfile = nullptr;
    }
    *out = buffer.data();
    return read;
}
