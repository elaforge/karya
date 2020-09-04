// Copyright 2018 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

#include <string>
#include <memory>
#include <vector>

#include "Audio.h"


// Read and mix together samples from subdirectories.
class Tracks : public Audio {
public:
    Tracks(std::ostream &log, int channels, int sample_rate,
        const std::string &dir, sf_count_t start_offset,
        const std::vector<std::string> &mutes);
    bool read(int channels, sf_count_t frames, float **out) override;

private:
    std::ostream &log;
    std::vector<std::unique_ptr<Audio>> audios;
    std::vector<float> buffer;
};
