// Copyright 2019 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

#include <algorithm>
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
// You get load Entry from a file (which is cached), mix multiple Entries into
// a MixedEntry, then ask MixedEntry for peaks at your zoom.
//
// MixedEntry *mixed = new MixedEntry(start);
// mixed->add(PeakCache::get()->load(params));
// std::shared_ptr<std::vector<float>> peaks = mixed->at_zoom(zoom.factor);
// ScoreTime chunk_start = mixed->start;
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
        const std::string filename;
        const ScoreTime start;
        const std::vector<double> ratios;

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
        Entry(ScoreTime start, std::vector<float> *peaks) :
            start(start), peaks(peaks)
        {}

        const ScoreTime start;
        // Maximum absolute values of each chunk of samples.  This is mono and
        // only positive.
        const std::shared_ptr<const std::vector<float>> peaks;
    };

    // This consists of >=1 Entry, with the peaks added together.
    // Unlike Entry, this isn't cached, so on each refresh they'll have to be
    // recreated and remixed, but loading an 480 element Entry from a 1.3mb
    // file is the expensive step.
    class MixedEntry {
    public:
        MixedEntry(ScoreTime start) : start(start), cached_zoom(0) {}
        void add(std::shared_ptr<const Entry> entry);
        float max_peak() const { return _max_peak; };
        // Get peaks adapted for this zoom level.
        std::shared_ptr<const std::vector<float>> at_zoom(double zoom_factor);

        const ScoreTime start;
    private:
        const std::vector<float> &peaks() const {
            return peaks1.get() ? *peaks1 : peaks_n;
        };
        float _max_peak;
        // If there is just one thing to mix, then reuse its pointer.
        std::shared_ptr<const std::vector<float>> peaks1;
        // Otherwise I have to allocate a buffer.
        std::vector<float> peaks_n;
        // If this hasn't changed, 'at_zoom' can reuse 'zoom_cache'.
        double cached_zoom;
        std::shared_ptr<const std::vector<float>> zoom_cache;
        // Keep the source Entrys alive in the 'cache'.
        std::vector<std::shared_ptr<const Entry>> sources;
    };

    // Load the file and downsample its peaks.  Use a cached Entry if one is
    // still alive.  The returned Entry will stay alive at least until the next
    // 'gc'.
    std::shared_ptr<const Entry> load(const Params &params);

    // Remove cache entries which are only kept alive by gc_roots, and
    // re-initialize gc_roots with the current live set.
    //
    // The reason for manually-triggered GC is that when the synthesizer hits
    // its cache, many waveform chunks will stay the same, so I don't want to
    // reload those.  But the rendering and hence waveform display process is
    // incremental, so karya will clear all waveforms and rebuild the
    // 'MixedEntry's over time.  And since output from multiple instruments
    // can coexist in one MixedEntry, I only know I can GC unused 'Entry's
    // once all renders are complete.
    //
    // I went through several attempts before coming to this design, since I
    // really wanted to just not unload the re-used waveforms in the first
    // place, and avoid all this complexity.  But since I have one way
    // communication from synthesizer -> karya -> fltk, it just doesn't work to
    // try to know fltk's state and only send the right things to it.
    void gc();

private:
    // C++ isn't done being a pain yet!  I can't specialize std::hash because
    // of order of declaration issues.
    struct HashParams {
        size_t operator()(const Params &p) const { return p.hash(); }
    };
    std::unordered_map<Params, std::weak_ptr<Entry>, HashParams> cache;
    // This will keep things in cache alive, until a gc().
    std::vector<std::shared_ptr<Entry>> gc_roots;
};
