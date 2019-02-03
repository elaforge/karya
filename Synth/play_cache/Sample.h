// Copyright 2018 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

#include <string>
#include <vector>

#include <sndfile.h>

#include "Audio.h"


// Stream from a directory of samples.  Files are in sorted order, and are
// opened and closed on demand.  Each *.wav file is expected to be
// CHECKPOINT_SECONDS long, which is used to find the initial sample given the
// offset.
class SampleDirectory : public Audio {
public:
    SampleDirectory(std::ostream &log, int channels, int sample_rate,
        const std::string &dir, sf_count_t offset);
    ~SampleDirectory();
    bool read(int channels, sf_count_t frames, float **out) override;

private:
    std::ostream &log;
    const int sample_rate;
    const std::string dir;

    std::string fname;
    SNDFILE *sndfile;
    std::vector<float> buffer;
};


// Just stream a single sample file.
class SampleFile : public Audio {
public:
    // If expand_channels is true, then also accept mono input files, and
    // expand them as required.
    SampleFile(std::ostream &log,
        int channels, bool expand_channels, int sample_rate,
        const std::string &fname, sf_count_t offset);
    ~SampleFile();
    bool read(int channels, sf_count_t frames, float **out) override;

private:
    std::ostream &log;
    const bool expand_channels;
    const std::string fname;
    SNDFILE *sndfile;
    int file_channels;
    std::vector<float> buffer;
    std::vector<float> expand_buffer;
};
