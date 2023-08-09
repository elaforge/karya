// Copyright 2023 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <ostream>
#include <vector>

#include "SampleFile.h"
#include "Synth/Shared/config.h"
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
        flac.reset(new Flac(fname.c_str(), offset));
        // TODO I could support non 41.1k srate, would have to add it to the
        // resample ratio.
        if (flac->error()) {
            LOG(fname << ": error opening: " << flac->error());
            flac.reset();
        } else if (flac->srate() != sample_rate) {
            LOG(fname << ": expected srate " << sample_rate << ", got "
                << flac->srate());
            flac.reset();
        } else if (flac->channels() != 1 && flac->channels() != 2) {
            LOG(fname << ": expected 1 or 2 channels, got "
                << flac->channels());
            flac.reset();
        }
    } else {
        LOG("not .flac: " << fname);
        return;
    }
}


bool
SampleFile::read(int channels, Frames frames, float **out)
{
    if (!flac) {
        return true;
    }
    buffer.resize(frames * channels);
    Frames read = flac->read(buffer.data(), frames);
    // read only reads less than asked on error or if the file ended.
    if (read < frames) {
        if (flac->error())
            LOG(fname << ": error reading: " << flac->error());
        flac.reset();
    }
    std::fill(buffer.begin() + read * channels, buffer.end(), 0);
    *out = buffer.data();
    return read == 0;
}
