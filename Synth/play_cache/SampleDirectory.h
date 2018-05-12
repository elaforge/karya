// Copyright 2018 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

#include <string>
#include <vector>

#include <sndfile.h>


// Stream from a directory of samples.  Files are in sorted order, and are
// opened and closed on demand.  Each *.wav file is expected to be
// CHECKPOINT_SECONDS long, which is used to find the initial sample given the
// offset.
class SampleDirectory {
public:
    SampleDirectory(std::ostream &log, int channels, int sampleRate,
        const std::string &dir, sf_count_t offset);
    ~SampleDirectory();

    // Read the number of frames into an internal static buffer and put it in
    // out.
    sf_count_t read(sf_count_t frames, float **out);

private:
    std::ostream &log;
    const int channels;
    const int sampleRate;
    const std::string dir;

    std::string fname;
    SNDFILE *sndfile;
    std::vector<float> buffer;
};
