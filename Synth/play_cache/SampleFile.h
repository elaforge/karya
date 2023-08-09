// Copyright 2023 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

#include <memory>
#include <string>
#include <vector>

#include "Audio.h"
#include "Flac.h"



// Just stream a single sample file.  This is used by ResampleStreamer for
// playing preview.
//
// At the moment this sample_rate just asserts that the file has that
// sample_rate.  If thru wants to tweak the frequency it may do a resample,
// so I could hook this up to play a file of any sample_rate and adjust it
// to suit, but so far I've just offline resampled everything to 44.1k.
class SampleFile : public Audio {
public:
    SampleFile(std::ostream &log, int channels, int sample_rate,
        const std::string &fname, Frames offset);
    ~SampleFile() {}
    bool read(int channels, Frames frames, float **out) override;

private:
    std::ostream &log;
    const std::string fname;
    std::unique_ptr<Flac> file;
    std::vector<float> buffer;
};
