// Copyright 2016 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once
#include <memory>
#include <sstream>
#include <string>

// Hold one sample.
class Sample {
public:
    Sample(const char *fname);
    ~Sample();
    std::string error() const { return error_; }
    int samplerate() const { return info.samplerate; }
    const sf_count_t read(sf_count_t fromFrame, float **frames) const;
private:
    // If set, there was an error reading the sample.
    std::string error_;
    SNDFILE *sndfile;
    SF_INFO info;
    std::unique_ptr<float[]> frames;
};
