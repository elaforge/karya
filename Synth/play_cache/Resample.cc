// Copyright 2018 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <math.h>
#include <samplerate.h>

#include "Resample.h"
#include "log.h"


Resample::Resample(std::ostream &log, int channels, double ratio, Audio *audio)
    : log(log), audio(audio) // , leftover(0)
{
    int error;
    this->state = src_new(SRC_SINC_FASTEST, channels, &error);
    // this->state = src_new(SRC_LINEAR, channels, &error);
    if (error) {
        LOG("src error: " << src_strerror(error));
    }
    this->data = {0};
    data.src_ratio = ratio;
}


Resample::~Resample()
{
    if (state)
        src_delete(state);
}


bool
Resample::read(int channels, sf_count_t frames, float **out)
{
    output.resize(frames * channels);
    data.output_frames = frames;
    data.data_out = output.data();

    while (data.output_frames > 0) {
        if (data.input_frames == 0) {
            // Guess how many frames it will need.  It's not always right
            // though.
            // sf_count_t inputFrames = ceil(frames * 1/ratio);
            sf_count_t inputFrames = frames;
            float *input;
            data.end_of_input =
                audio->read(channels, inputFrames, &input);
            // LOG("read " << inputFrames << " done:" << data.end_of_input);
            if (data.end_of_input) {
                data.data_in = nullptr;
                data.input_frames = 0;
            } else {
                data.data_in = input;
                data.input_frames = inputFrames;
            }
        }
        // LOG("in:" << data.input_frames << " out:" << data.output_frames);
        int error = src_process(state, &data);
        // LOG("used:" << data.input_frames_used
        //     << " gen:" << data.output_frames_gen);
        if (error) {
            LOG("resample error: " << src_strerror(error));
            return true;
        }
        if (data.end_of_input && data.output_frames_gen == 0)
            break;

        if (data.data_in)
            data.data_in += data.input_frames_used * channels;
        // outPosition += data.output_frames_gen;
        data.data_out += data.output_frames_gen * channels;
        data.input_frames -= data.input_frames_used;
        data.output_frames -= data.output_frames_gen;
    }

    // Zero the buffer in the unwritten range.
    if (data.output_frames > 0)
        std::fill(output.end() - data.output_frames*channels, output.end(), 0);

    *out = output.data();
    // If input is out and src won't produce any more samples, then I'm done.
    return data.end_of_input && data.output_frames > 0;
}
