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
    if (!fname.empty()) {
        if (!util::ends_with(fname, ".flac")) {
            LOG("only flac supported yet: " << fname);
            return;
        }
        LOG(fname << " + " << offset);
        file = Flac::open(fname.c_str(), offset);
        // TODO I could support non 41.1k srate, would have to add it to the
        // resample ratio.
        if (file->error())
            LOG(fname << ": error opening: " << file->error());
        else if (file->srate() != sample_rate)
            LOG(fname << ": expected srate " << sample_rate << ", got "
                << file->srate());
        else if (file->channels() != 1 && file->channels() != 2)
            LOG(fname << ": expected 1 or 2 channels, got "
                << file->channels());
        else
            return; // file is ok
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
