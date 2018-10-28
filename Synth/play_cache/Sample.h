// Copyright 2018 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

#include <string>
#include <vector>

#include <sndfile.h>


class Sample {
public:
    virtual ~Sample() {};
    // Read the number of frames into an internal static buffer and put it in
    // out.
    virtual sf_count_t read(sf_count_t frames, float **out) = 0;
};


// Stream from a directory of samples.  Files are in sorted order, and are
// opened and closed on demand.  Each *.wav file is expected to be
// CHECKPOINT_SECONDS long, which is used to find the initial sample given the
// offset.
class SampleDirectory : public Sample {
public:
    SampleDirectory(std::ostream &log, int channels, int sampleRate,
        const std::string &dir, sf_count_t offset);
    ~SampleDirectory();
    sf_count_t read(sf_count_t frames, float **out) override;

private:
    std::ostream &log;
    const int channels;
    const int sampleRate;
    const std::string dir;

    std::string fname;
    SNDFILE *sndfile;
    std::vector<float> buffer;
};


// Just stream a single sample file.
class SampleFile : public Sample {
public:
    SampleFile(std::ostream &log, int channels, int sampleRate,
        const std::string &fname, sf_count_t offset);
    ~SampleFile();
    sf_count_t read(sf_count_t frames, float **out) override;

private:
    std::ostream &log;
    const int channels;
    const std::string fname;

    SNDFILE *sndfile;
    std::vector<float> buffer;
};
