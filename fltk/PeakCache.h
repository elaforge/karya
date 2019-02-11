// Copyright 2019 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

#include <memory>
#include <string>
#include <vector>

#include "types.h"


// either a separate ZoomedPeakCache, which PeakCache can produce.  Or each
// PeakCache has an internal one, and you ask by zoom.
// If it has an internal one, then I have a dangling pointer if I return it.
class PeakCache {
public:
    struct Params {
        Params(const std::string &filename, ScoreTime start,
                const std::vector<double> &ratios)
            : filename(filename), start(start), ratios(ratios)
        {}
        std::string filename;
        ScoreTime start;
        std::vector<double> ratios;
    };

    PeakCache(const Params &params) : params(params) {}
    // Load params.filename into peaks.
    void load();
    // Return peaks downsampled to have 1/pixel at the given zoom_factor.
    std::unique_ptr<std::vector<float>> get(double zoom_factor);
    // The number of pixels covered by each peak, which is >1 if the zoom was
    // high enough that peaks would have had to be upsampled.
    static double pixels_per_peak(double zoom_factor);

    const Params params;

private:
    // Maximum values of each chunk of samples.  This is the absolute value of
    // all channels, so it's mono and only positive.
    std::vector<float> peaks;
};
