// Copyright 2016 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <memory>
#include <sstream>
#include <sndfile.h>

#include "Sample.h"
#include "../Shared/config.h"


Sample::Sample(int sampleRate, const std::string &fname)
{
    SNDFILE *sndfile = sf_open(fname.c_str(), SFM_READ, &this->info);
    std::stringstream errors;
    if (sf_error(sndfile) != SF_ERR_NO_ERROR) {
        error = FileNotFound;
    } else if (info.channels != 2) {
        errors << "expected 2 channels, got " << info.channels;
        error = ErrorMessage;
    } else if (info.samplerate != sampleRate) {
        errors << "expected srate of " << sampleRate << ", got "
            << info.samplerate;
        error = ErrorMessage;
    } else {
        this->frames.reset(new float[info.frames * info.channels]);
        sf_count_t read = sf_readf_float(sndfile, frames.get(), info.frames);
        if (read != info.frames) {
            errors << "expected " << info.frames << " frames, but read "
                << read;
        }
        error = NoError;
    }
    if (!errors.str().empty()) {
        errorMessage = fname + ": " + errors.str();
    }

    sf_close(sndfile);
    // if (int r = (sf_close(snd) != 0)) {
    //     const char *err = sf_error_number(r);
    // }
}


const sf_count_t
Sample::read(sf_count_t fromFrame, float **frames) const
{
    if (error != NoError || fromFrame >= info.frames)
        return 0;

    *frames = this->frames.get() + fromFrame * info.channels;
    return info.frames - fromFrame;
}
