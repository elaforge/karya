// Copyright 2019 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

#include <unordered_map>
#include <memory>
#include <string>
#include <vector>

#include "Zoom.h"
#include "types.h"


// This is a global cache for loaded audio files.  The samples are downsampled
// to an intermediate form, and then downsampled again for a specific zoom.
// The cache keeps 'PeakCache::Entry's by weak_ptr, and each Entry keeps a
// single zoom cache by zoom factor.
//
// All of this downsampling happens synchronously, but so far it seems to be
// fast enough to not introduce a noticeable hiccup.
//
// API:
// std::shared_ptr<PeakCache::Entry> entry = PeakCache::get()->load(params);
// std::shared_ptr<std::vector<float>> peaks = entry->at_zoom(zoom.factor);
// ScoreTime chunk_start = entry->start;
class PeakCache {
public:
    // Get the global instance.
    static PeakCache *get();
    // The number of pixels covered by each peak, which is >1 if the zoom was
    // high enough that peaks would have had to be upsampled.
    static double pixels_per_peak(double zoom_factor);

    struct Params {
        Params(const std::string &filename, ScoreTime start,
                const std::vector<double> &ratios)
            : filename(filename), start(start), ratios(ratios)
        {}
        std::string filename;
        ScoreTime start;
        std::vector<double> ratios;

        // All this just to use unordered_map.  C++ sure is a pain.
        bool operator==(const Params &that) const {
            return filename == that.filename && start == that.start
                && ratios == that.ratios;
        }
        size_t hash() const {
            size_t val = ratios.size();
            // Magic courtesy of stack overflow "how to specialize std::hash".
            // Ultimately it comes from boost combine_hash.
            for (double d : ratios) {
                val ^= std::hash<double>()(d)
                    + 0x9e3779b9 + (val << 6) + (val >> 2);
            }
            return std::hash<std::string>()(filename)
                ^ std::hash<double>()(start.to_real())
                ^ val;
        }
    };

    class Entry {
    public:
        Entry(ScoreTime start, std::vector<float> *peaks)
            : start(start), peaks(peaks), cached_zoom(0)
        {}

        const ScoreTime start;
        // Get peaks adapted for this zoom level.
        std::shared_ptr<const std::vector<float>> at_zoom(double zoom_factor);

    private:
        // Maximum values of each chunk of samples.  This is the absolute value
        // of all channels, so it's mono and only positive.
        // std::vector<float> peaks;
        std::unique_ptr<std::vector<float>> peaks;
        double cached_zoom;
        std::shared_ptr<const std::vector<float>> zoom_cache;
    };

    // Load the file and downsample its peaks.  Use a cached Entry if
    // one is still alive.
    std::shared_ptr<Entry> load(const Params &params);

private:
    // C++ isn't done being a pain yet!  I can't specialize std::hash because
    // of order of declaration issues.
    struct HashParams {
        size_t operator()(const Params &p) const { return p.hash(); }
    };
    std::unordered_map<Params, std::weak_ptr<Entry>, HashParams> cache;
};
