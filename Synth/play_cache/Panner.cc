// Copyright 2020 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <fstream>
#include <iostream>
#include <math.h>
#include <string.h>

#include "Synth/Shared/config.h"
#include "Panner.h"
#include "log.h"


// Miscellaneous constants.
enum {
    channels = 2,
    num_inputs = 2,
    num_programs = 0,
    // How much inherent delay the plugin has.
    initial_delay = 0
};

// VST parameters.
enum {
    p_volume = 0,
    p_volume_cc,
    p_pan_cc,
    num_parameters
};

static const int32_t unique_id = 1527646442;
static const int32_t version = 1;

enum {
    ControlChange = 0xb0,

    VolumeCC = 7,
    PanCC = 10
};

static const char *log_filename = VST_BASE_DIR "/Panner.log";


// Magic function name, called by VSTMain, which is called by the host.
VstEffectInterface *
create_effect_instance(VstHostCallback host_callback)
{
    return (new Panner(host_callback))->get_vst();
}

// Plugin::Plugin(VstHostCallback host_callback,
//     int32_t num_programs, int32_t num_parameters, int32_t num_in_channels,
//     int32_t num_out_channels, int32_t unique_id, int32_t version,
//     int32_t initial_delay, bool is_synth)
Panner::Panner(VstHostCallback host_callback) :
    Plugin(host_callback, num_programs, num_parameters, num_inputs, channels,
        // It's not a synth, but I want it to receive MIDI, so I think it
        // should be is_synth=true.
        unique_id, version, initial_delay, false),
    volume(1), pan(0),
    volume_param(1), volume_cc(7), pan_cc(10),
    log(log_filename, std::ios::app)
{
    LOG("started");
}

Panner::~Panner()
{
    LOG("stopped");
}


void
Panner::resume()
{
    buffer.resize(max_block_frames * channels);
}


// configure

void
Panner::get_plugin_name(char *name)
{
    strncpy(name, "Panner", Max::PlugInNameStringLength);
}

void
Panner::get_manufacturer_name(char *text)
{
    strncpy(text, "Karya", Max::ManufacturerStringLength);
}

bool
Panner::get_output_properties(int32_t index, VstPinProperties *properties)
{
    if (index >= channels)
        return false;
    properties->flags = VstPinProperties::IsActive;
    switch (index) {
    case 0:
        strncpy(properties->text, "out1", 63);
        properties->flags |= VstPinProperties::IsStereo;
        break;
    case 1:
        strncpy(properties->text, "out2", 63);
        break;
    }
    return true;
}

bool
Panner::get_input_properties(int32_t index, VstPinProperties *properties)
{
    if (index >= channels)
        return false;
    properties->flags = VstPinProperties::IsActive;
    switch (index) {
    case 0:
        strncpy(properties->text, "in1", 63);
        properties->flags |= VstPinProperties::IsStereo;
        break;
    case 1:
        strncpy(properties->text, "in2", 63);
        break;
    }
    return true;
}


// parameters

static float
db_to_linear(float db)
{
    return exp2(db * 0.16609640474);
}

static float
linear_to_db(float f)
{
    return log10(f) * 20;
}

void
Panner::set_parameter(int32_t index, float value)
{
    switch (index) {
    case p_volume:
        this->volume_param = value;
        break;
    case p_volume_cc:
        this->volume_cc = floorf(value * 127);
        break;
    case p_pan_cc:
        this->pan_cc = floorf(value * 127);
    }
}

float
Panner::get_parameter(int32_t index)
{
    switch (index) {
    case p_volume:
        return volume_param;
    case p_volume_cc:
        return volume_cc;
    case p_pan_cc:
        return pan_cc;
    default:
        return 0;
    }
}

void
Panner::get_parameter_label(int32_t index, char *label)
{
    switch (index) {
    case p_volume:
        strncpy(label, "dB", Max::ParameterOrPinLabelLength);
        break;
    case p_volume_cc:
    case p_pan_cc:
        *label = '\0';
        break;
    }
}

void
Panner::get_parameter_text(int32_t index, char *text)
{
    switch (index) {
    case p_volume:
        snprintf(text, Max::ParameterOrPinLabelLength, "%.2fdB",
            linear_to_db(volume_param));
        break;
    case p_volume_cc:
        snprintf(text, Max::ParameterOrPinLabelLength, "%d", volume_cc);
        break;
    case p_pan_cc:
        snprintf(text, Max::ParameterOrPinLabelLength, "%d", pan_cc);
        break;
    }
}

void
Panner::get_parameter_name(int32_t index, char *text)
{
    switch (index) {
    case p_volume:
        strncpy(text, "volume", Max::ParameterOrPinLabelLength);
        break;
    case p_volume_cc:
        strncpy(text, "vol cc", Max::ParameterOrPinLabelLength);
        break;
    case p_pan_cc:
        strncpy(text, "pan cc", Max::ParameterOrPinLabelLength);
        break;
    }
}

// process


int32_t
Panner::process_events(const VstEventBlock *events)
{
    for (int32_t i = 0; i < events->number_of_events; i++) {
        if (events->events[i]->type != VstEventBlock::Midi)
            continue;

        VstMidiEvent *event = (VstMidiEvent *) events->events[i];
        const char *data = event->midi_data;

        int status = data[0] & 0xf0;
        if (status == ControlChange) {
            float val = float(data[2]) / 127;
            const int cc = data[1];
            if (cc == 0) {
                // 0 disables this control.
            } else if (cc == pan_cc) {
                pan_to = fmaxf(-1, fminf(1, val*2 - 1));
                LOG("pan_to: " << pan_to);
            } else if (cc == volume_cc) {
                volume_to = db_to_linear(val * -96);
                LOG("volume_to: " << pan_to);
            }
        }
    }
    return 1;
}


static float
update_control(float val, float valTo)
{
    // volume and pan move maximum of this much per sample, to avoid
    // discontinuity.
    const float max_slope = 0.001;
    float diff = fmaxf(max_slope, fabsf(valTo - val));
    return val > valTo ? val - diff : val + diff;
}


// This is a bit of weird magic from reaktor to curve the function slightly,
// presumably for somewhat equal-power-ish panning.
static float
shape(float x)
{
    return x * (4 - x) * 1/3;
}

void
Panner::process(float **inputs, float **outputs, int32_t process_frames)
{
    float *out1 = outputs[0];
    float *out2 = outputs[1];
    float *in1 = inputs[0];
    float *in2 = inputs[1];

    memset(out1, 0, process_frames * sizeof(float));
    memset(out2, 0, process_frames * sizeof(float));

    // float maxval = 0;
    // for (int i = 0; i < process_frames; i++)
    //     maxval = fmaxf(maxval, in1[i]);
    // if (maxval > 0)
    //     LOG("block " << maxval);

    // float delta = 0.001;
    // float val = 0;
    // for (int frame = 0; frame < process_frames; frame++) {
    //     out1[frame] = val;
    //     if (val >= 1)
    //         val += delta;
    //     else
    //         val -= delta;
    // }

    // memset(out1, 0, process_frames * sizeof(float));
    // memset(out2, 0, process_frames * sizeof(float));
    // return;

    for (int frame = 0; frame < process_frames; frame++) {
        volume = update_control(volume, volume_to);
        pan = update_control(pan, pan_to);

        // Stereo pan:
        // let pan12 pan = (max (-1) (pan*2 - 1), min 1 ((pan+1) * 2 - 1))
        // > map pan12 [-1, -0.5, 0, 0.5, 1]
        // [(-1.0, -1.0), (-1.0, 0.0), (-1.0, 1.0), (0.0, 1.0), (1.0, 1.0)]

        float pan1 = fmaxf(-1, pan*2 - 1);
        float pan2 = fminf(1, (pan+1) * 2 - 1);

        float left = in1[frame];
        float right = in2[frame];

        // setPan (left, right) =
        //     ( (1-pan1) * left + (1-pan2) * left
        //     , (1+pan1) * right + (1+pan2) * right
        //     )

        left = shape(1-pan1) * left + shape(1-pan2) * left;
        right = shape(1+pan1) * right + shape(1+pan2) * right;

        out1[frame] = left / 4 * volume * volume_param;
        out2[frame] = right / 4 * volume * volume_param;
    }
}
