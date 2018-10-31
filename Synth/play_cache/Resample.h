// Copyright 2018 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

#include <fstream>
#include <memory>
#include <vector>

#include <samplerate.h>

#include "Audio.h"


class Resample : public Audio {
public:
    Resample(std::ostream &log, int channels, double ratio, Audio *audio);
    ~Resample();
    bool read(int channels, sf_count_t frames, float **out) override;
private:
    std::ostream &log;
    std::unique_ptr<Audio> audio;
    SRC_STATE *state;
    SRC_DATA data;
    // float *input;
    std::vector<float> output;
};
