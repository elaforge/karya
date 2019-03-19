// Copyright 2019 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <chrono>
#include <math.h>
#include <sndfile.h>

#include "PeakCache.h"
#include "types.h"
#include "util.h"

// for SAMPLING_RATE.
#include "Synth/Shared/config.h"


enum {
    // Store a max value at this sampling rate.
    reduced_sampling_rate = 120,
    // Read this many frames at once when reading the file.
    read_buffer_frames = 256,

    sampling_rate = SAMPLING_RATE,
    // Each Params::ratios breakpoint is this many frames apart.
    frames_per_ratio = sampling_rate / 2,
};

// If true, print some stats about resampling times.
const static bool print_metrics = false;


PeakCache *
PeakCache::get()
{
    static PeakCache peak_cache;
    return &peak_cache;
}


double
PeakCache::pixels_per_peak(double zoom_factor)
{
    double period = reduced_sampling_rate / zoom_factor;
    if (period <= 1)
        return 1 / period;
    else
        return 1;
}


static std::shared_ptr<const std::vector<float>>
reduce_zoom(
    std::shared_ptr<const std::vector<float>> peaks, double zoom_factor)
{
    // zoom_factor is the number of pixels in ScoreTime(1).  So that's the
    // desired sampling rate.  E.g. zoom=2 means ScoreTime(1) is 2 pixels.
    double period = reduced_sampling_rate / zoom_factor;
    if (period <= 1) {
        return peaks;
    }
    std::shared_ptr<std::vector<float>> out(new std::vector<float>());
    out->reserve(ceil(peaks->size() / period));
    double left = period;
    float accum = 0;
    ASSERT(period >= 1);
    for (float n : *peaks) {
        if (left < 1) {
            out->push_back(accum);
            accum = n;
            left += period;
        }
        accum = std::max(accum, n);
        left--;
    }
    if (!peaks->empty())
        out->push_back(accum);
    return out;
}


std::shared_ptr<const std::vector<float>>
PeakCache::Entry::at_zoom(double zoom_factor)
{
    if (zoom_factor != cached_zoom || !zoom_cache.get()) {
        auto start = std::chrono::steady_clock::now();
        zoom_cache = reduce_zoom(peaks, zoom_factor);
        if (print_metrics) {
            // Zooming a track with 43 chunks takes 0.1ms.  So I think I don't
            // have to worry about zoom times.
            auto end = std::chrono::steady_clock::now();
            std::chrono::duration<double> dur = end - start;
            DEBUG("METRIC zoom " << peaks->size() << " to "
                << zoom_cache->size() << " dur: " << dur.count());
        }
        cached_zoom = zoom_factor;
    }
    return zoom_cache;
}


static double
period_at(const std::vector<double> &ratios, sf_count_t frame)
{
    // Use frames_per_ratio to get an index into ratios, then interpolate.
    if (ratios.empty()) {
        return 1;
    }
    size_t i = floor(frame / double(frames_per_ratio));
    double frac = fmod(frame / double(frames_per_ratio), 1);
    if (i < ratios.size()-1) {
        double r1 = ratios[i], r2 = ratios[i+1];
        return (frac * (r2-r1) + r1);
    } else {
        return ratios[ratios.size()-1];
    }
}


// Originally I returned the vector directly and relied on return value
// optimization, but there was still a copy.  unique_ptr didn't believe that
// I wasn't making a copy either, so raw pointer given to Entry is it.
static std::vector<float> *
load_file(const std::string &filename, const std::vector<double> &ratios)
{
    SF_INFO info = {0};
    SNDFILE *sndfile = sf_open(filename.c_str(), SFM_READ, &info);
    std::vector<float> *peaks = new std::vector<float>();
    if (sf_error(sndfile) != SF_ERR_NO_ERROR) {
        // TODO should be LOG
        DEBUG("opening " << filename << ": " << sf_strerror(sndfile));
        return peaks;
    } else if (info.samplerate != sampling_rate) {
        DEBUG(filename << ": expected srate of " << sampling_rate << ", got "
            << info.samplerate);
        return peaks;
    }

    std::vector<float> buffer(read_buffer_frames * info.channels);
    sf_count_t frame = 0;
    sf_count_t frames_left = 0;
    // How many frames to consume in this period
    double srate = sampling_rate / reduced_sampling_rate;
    double period = srate * period_at(ratios, frame);
    // DEBUG("period " << srate << " * "
    //     << period_at(ratios, frame) << " = " << period);
    unsigned int index = 0;
    float accum = 0;
    for (;;) {
        if (frames_left == 0) {
            frames_left += sf_readf_float(
                sndfile, buffer.data(), read_buffer_frames);
            if (!frames_left)
                break;
            index = 0;
        }
        sf_count_t consume = floor(std::min(period, double(frames_left)));
        // TODO can I vectorize?  at least fabsf has it.
        for (; index < consume * info.channels; index++) {
            accum = std::max(accum, fabsf(buffer[index]));
        }
        frames_left -= consume;
        period -= consume;
        frame += consume;
        if (period < 1) {
            peaks->push_back(accum);
            accum = 0;
            period += srate * period_at(ratios, frame);
        }
    }
    // DEBUG("load frames: " << frame << ", peaks: " << peaks->size());
    sf_close(sndfile);
    return peaks;
}


std::shared_ptr<PeakCache::Entry>
PeakCache::load(const Params &params)
{
    auto found = cache.find(params);
    std::shared_ptr<Entry> entry;
    if (found != cache.end()) {
        // DEBUG("entry exists " << params.filename
        //     << " refs: " << found->second.use_count());
        entry = found->second.lock();
    }
    if (!entry) {
        // DEBUG("load " << params.filename);

        auto start = std::chrono::steady_clock::now();
        std::vector<float> *peaks = load_file(params.filename, params.ratios);
        entry.reset(new PeakCache::Entry(params.start, peaks));

        if (print_metrics) {
            // Loading a 3s chunk takes around 3ms.
            static double total_dur;
            static int total_count;
            auto end = std::chrono::steady_clock::now();
            std::chrono::duration<double> dur = end - start;
            total_dur += dur.count();
            total_count++;
            DEBUG("METRIC load " << params.filename << ": " << dur.count()
                << " total_dur: " << total_dur << " of " << total_count);
        }
        gc_roots.push_back(entry);
        cache[params] = entry;
    }
    return entry;
}


void
PeakCache::gc()
{
    // DEBUG("start gc");
    gc_roots.clear();
    auto it = cache.begin();
    // int del = 0, kept = 0;
    while (it != cache.end()) {
        std::shared_ptr<Entry> entry(it->second.lock());
        if (entry.get()) {
            gc_roots.push_back(entry);
            ++it;
            // kept++;
        } else {
            it = cache.erase(it);
            // del++;
        }
    }
    // DEBUG("end gc, del " << del << " kept " << kept);
}
