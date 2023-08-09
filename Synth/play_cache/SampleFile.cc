// Copyright 2023 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <ostream>
#include <vector>

#include "AudioFile.h"
#include "Flac.h"
#include "SampleFile.h"
#include "Synth/Shared/config.h"
#include "Wav.h"
#include "log.h"
#include "util.h"

using std::string;


SampleFile::SampleFile(
    std::ostream &log, int _channels, int sample_rate,
    const string &fname, Frames offset
) : log(log), fname(fname)
{
    // TODO channels is always 2, it's hardcoded in PlayCache.  In theory I
    // should use it to possibly merge or expand file channels, but the chance
    // of it ever being used is not high.
    if (fname.empty())
        return;
    LOG(fname << " + " << offset);
    if (util::ends_with(fname, ".flac")) {
        file.reset(new Flac(fname.c_str(), offset));
    } else if (util::ends_with(fname, ".wav")) {
        file.reset(new Wav(fname.c_str(), offset));
    } else {
        LOG("neither .flac nor .wav: " << fname);
        return;
    }

    if (file->error()) {
        LOG(fname << ": error opening: " << file->error());
        file.reset();
    } else if (file->srate() != sample_rate) {
        // TODO I could support non 41.1k srate, would have to add it to
        // the resample ratio.
        LOG(fname << ": expected srate " << sample_rate << ", got "
            << file->srate());
        file.reset();
    } else if (file->channels() != 1 && file->channels() != 2) {
        LOG(fname << ": expected 1 or 2 channels, got "
            << file->channels());
        file.reset();
    }
}


bool
SampleFile::read(int channels, Frames frames, float **out)
{
    if (!file) {
        return true;
    }
    buffer.resize(frames * channels);
    Frames read = file->read(buffer.data(), frames);
    // read only reads less than asked on error or if the file ended.
    if (read < frames) {
        if (file->error())
            LOG(fname << ": error reading: " << file->error());
        file.reset();
    }
    std::fill(buffer.begin() + read * channels, buffer.end(), 0);
    *out = buffer.data();
    return read == 0;
}
