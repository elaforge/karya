// Copyright 2018 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

#include <atomic>
#include <string>
#include <memory>
#include <vector>

#include "SampleDirectory.h"


// Read and mix together a list of samples.
class Mix {
public:
    Mix(std::ostream &log, int channels, int sampleRate,
        const std::vector<std::string> &dirnames, sf_count_t startOffset);

    // Read the number of frames into an internal static buffer and put it in
    // out.  There are no partial reads, return all requseted frames and false,
    // or true.
    bool read(sf_count_t frames, float **out);

private:
    std::vector<std::unique_ptr<SampleDirectory>> sampleDirs;
    std::vector<float> buffer;
    std::ostream &log;
    const int channels;
};
