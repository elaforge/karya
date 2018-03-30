// Copyright 2018 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

#include <string>
#include <vector>

#include <sndfile.h>


// Read a single sample and keep an internal static buffer.  Automatically
// close when deleted, or when reading past the end.
class Sample {
public:
    Sample(std::ostream &log, int channels, int sampleRate,
        const std::string &fname, sf_count_t startOffset);
    ~Sample();

    // Read the number of frames into an internal static buffer and put it in
    // out.
    sf_count_t read(sf_count_t frames, float **out);

private:
    SNDFILE *sndfile;
    std::vector<float> buffer;
    const int channels;
};
