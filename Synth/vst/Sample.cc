// Copyright 2016 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <memory>
#include <sndfile.h>

#include "Sample.h"
#include "../Shared/config.h"


Sample::Sample(const char *fname)
{
    this->sndfile = sf_open(fname, SFM_READ, &this->info);
    std::stringstream errors;
    if (sf_error(sndfile) != SF_ERR_NO_ERROR) {
        errors << sf_strerror(sndfile);
    } else if (info.channels != 2) {
        errors << "expected 2 channels, got " << info.channels;
    } else if (info.samplerate != SAMPLING_RATE) {
        errors << "expected srate of " << SAMPLING_RATE << ", got "
            << info.samplerate;
    } else {
        this->frames.reset(new float[info.frames * info.channels]);
        sf_count_t read = sf_readf_float(sndfile, frames.get(), info.frames);
        if (read != info.frames) {
            errors << "expected " << info.frames << " frames, but read "
                << read;
        }
    }
    if (!errors.str().empty()) {
        error_ = std::string(fname) + ": " + errors.str();
    }
}

const sf_count_t Sample::read(sf_count_t fromFrame, float **frames) const {
    if (!error().empty() || fromFrame >= info.frames)
        return 0;

    *frames = this->frames.get() + fromFrame * info.channels;
    return info.frames - fromFrame;
}

Sample::~Sample()
{
    sf_close(this->sndfile);
    // if (int r = (sf_close(snd) != 0)) {
    //     const char *err = sf_error_number(r);
    // }
}
