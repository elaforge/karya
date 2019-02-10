// Copyright 2019 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <math.h>
#include <sndfile.h>

#include "PeakCache.h"
#include "types.h"
#include "util.h"

// for SAMPLING_RATE.
#include "Synth/Shared/config.h"


enum {
    // Store a max value for each group of this many frames.
    // Reduced sampling rate is sampling_rate / sample_period
    // sample_period = 512,

    reduced_sampling_rate = 90,
    // Read this many frames at once when reading the file.
    read_buffer_frames = 256,

    sampling_rate = SAMPLING_RATE,
    // chunk_frames = sampling_rate / 4, // TODO from Shared.Config.chunkSize
    // Each Params::ratios breakpoint is this many frames apart.
    frames_per_ratio = sampling_rate / 2,
};


static double
ratio_at(const std::vector<double> &ratios, sf_count_t frame)
{
    // Use frames_per_ratio to get an index into ratios, then interpolate.
    if (ratios.empty()) {
        return 1;
    }
    int i = floor(frame / double(frames_per_ratio));
    double frac = fmod(frame / double(frames_per_ratio), 1);
    if (i < ratios.size()-1) {
        double r1 = ratios[i], r2 = ratios[i+1];
        return frac * (r2-r1) + r1;
    } else {
        return ratios[ratios.size()-1];
    }
}


// modf() that returns the integral part and stores the fractional port.
static int
proper_frac(double d, double *frac)
{
    double i;
    *frac = modf(d, &i);
    return floor(i);
}


void
PeakCache::load()
{
    DEBUG("PeakCache::load " << params.filename);
    SF_INFO info = {0};
    SNDFILE *sndfile = sf_open(params.filename.c_str(), SFM_READ, &info);
    if (sf_error(sndfile) != SF_ERR_NO_ERROR) {
        // TODO should be LOG
        DEBUG("opening " << params.filename << ": " << sf_strerror(sndfile));
        return;
    }

    // double sample_period = double(sampling_rate) / reduced_sampling_rate;
    // // TODO use params.ratios!
    // this->peaks.resize(ceil(info.frames / double(sample_period)));
    // std::vector<float> buffer(sample_period * info.channels);
    // for (int peak = 0; peak < peaks.size(); peak++) {
    //     sf_count_t frames = sf_readf_float(
    //         sndfile, buffer.data(), sample_period);
    //     peaks[peak] = 0;
    //     for (unsigned int i = 0; i < frames * info.channels; i++) {
    //         peaks[peak] = std::max(peaks[peak], fabsf(buffer[i]));
    //     }
    // }

    std::vector<float> buffer(read_buffer_frames * info.channels);
    sf_count_t frame = 0;
    sf_count_t frames_left = 0;
    // How many frames to consume in this period
    double period = sampling_rate / reduced_sampling_rate
        * ratio_at(params.ratios, frame);
    unsigned int index = 0;
    float accum = 0;
    double frac = 0;
    for (;;) {
        if (frames_left == 0) {
            frames_left += sf_readf_float(
                sndfile, buffer.data(), read_buffer_frames);
            if (!frames_left)
                break;
            index = 0;
        }
        sf_count_t consume = proper_frac(
            std::min(period + frac, double(frames_left)), &frac);
        // TODO can I vectorize?  at least fabsf has it.
        for (; index < consume * info.channels; index++) {
            accum = std::max(accum, fabsf(buffer[index]));
        }
        frames_left -= consume;
        period -= consume;
        frame += consume;
        if (period == 0) {
            peaks.push_back(accum);
            accum = 0;
            period = sampling_rate / reduced_sampling_rate
                * ratio_at(params.ratios, frame);
        }
    }
    DEBUG("PeakCache::load, frames: " << frame << ", peaks: " << peaks.size());
    sf_close(sndfile);

    /*
        // How many frames go in each period?
        // Fit in this much ScoreTime at zoom 1.
        // sample_period
        double duration = (params.end - params.start).to_real() * sampling_rate;

        // the (start, end) is saying how to convert RealTime to ScoreTime.

        // if this is 1, I have to fit chunk_frames into 1.
        // Effectively I'm reducing the sampling rate by some ratio.
        // Then to fit chunk_frames into 1, if reduction is 1, then
        // it's 44.1k/sec, so reduction is 1.
        //
        // Then when I ask for frames, it gets reduced again, or not.

        // If reduce = 1 and duration = chunk_frames, then period is 1.
        // If duration = chunk_frames/2, then period = 2.
        // If duration = 1, then period = chunk_frames
        // duration = 2, -> chunk_frames / 2
        // If duration = n, then period = chunk_frames/n
        double reduction = 512;
        double period = chunk_frames / duration * reduction;

        // Each period samples, collect one.  Carry the remainder.
        for (;;) {
        }
    */
}


static std::vector<float>
reduce(const std::vector<float> &samples, double period)
{
    std::vector<float> out;
    out.reserve(ceil(samples.size() / period));
    double left = period;
    float accum = 0;
    ASSERT(period >= 1);
    for (float n : samples) {
        if (left < 1) {
            out.push_back(accum);
            accum = n;
            left += period;
        }
        accum = std::max(accum, n);
        left--;
    }
    if (!samples.empty())
        out.push_back(accum);
    return out;
}


std::unique_ptr<std::vector<float>>
PeakCache::get(double zoom_factor)
{
    // If zoom is far, shrink peaks so I have one value per pixel.
    // I have to handle the roundoff if it's not an exact multiple of the
    // period.
    // If zoom is close, return them as-is, but the draw has to know that they
    // are >1 pixel apart.  Also the caller has to handle fractions... though I
    // think I can just use fractional coordinates?

    // I need to get to real pixels.  The ratios take it to TrackTime.
    // Zoom takes TrackTime to pixels.
    // If ratio == 1, then 1 TrackTime = 1 second, so there are sampling_rate
    // frames in 1 TrackTime.  Since there are sample_period frames in each
    // peak, then

    // amount of ScoreTime covered by pixel, this gets smaller as I zoom in
    // double per_pixel = 1 / zoom_factor;
    // how many pixels in one score time?  This is the desired sampling rate.

    // zoom_factor is the number of pixels in ScoreTime(1).  So that's the
    // desired sampling rate.
    // E.g. zoom=2 means ScoreTime(1) is 2 pixels.
    double period = reduced_sampling_rate / zoom_factor;

    if (period <= 1) {
        DEBUG("PeakCache::get, zoom: " << zoom_factor
            << ", period: " << period);
        // Surely there's a less noisy way.
        return std::unique_ptr<std::vector<float>>(
            new std::vector<float>(peaks));
    } else {
        auto p = std::unique_ptr<std::vector<float>>(
            new std::vector<float>(reduce(peaks, period)));
        DEBUG("PeakCache::get, zoom: " << zoom_factor
            << ", period: " << period << " size: " << p->size());
        return p;
    }
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
