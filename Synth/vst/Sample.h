// Copyright 2016 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once
#include <memory>
#include <string>

#include <sndfile.h>


// Hold one sample.
//
// Currently this loads the entire sample into memory, so it should be
// allocation-free after the creation.  In the future I could try streaming,
// but I'd need to put the reading part in a separate thread.
class Sample {
public:
    enum Error {
        NoError, FileNotFound, ErrorMessage
    };

    Sample(int sampleRate, const std::string &fname);
    const sf_count_t read(sf_count_t fromFrame, float **frames) const;

    Error error;
    std::string errorMessage;
private:
    SF_INFO info;
    std::unique_ptr<float[]> frames;
};
