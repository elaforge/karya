// Copyright 2020 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <math.h>

#include "Synth/Shared/config.h"
#include "Panner.h"


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
    num_parameters
};

static const int32_t unique_id = 1527646442;
static const int32_t version = 1;

enum {
    ControlChange = 0xb0,

    VolumeCC = 7,
    PanCC = 10
};


// Magic function name, called by VSTMain, which is called by the host.
VstEffectInterface *
create_effect_instance(VstHostCallback host_callback)
{
    return (new Panner(host_callback))->get_vst();
}

// Plugin::Plugin(VstHostCallback host_callback,
//     int32_t num_programs, int32_t num_parameters, int32_t num_in_channels,
//     int32_t num_out_channels, int32_t unique_id, int32_t version,
//     int32_t initial_delay, bool is_synth) :
Panner::Panner(VstHostCallback host_callback) :
    Plugin(host_callback, num_programs, num_parameters, num_inputs, channels,
        unique_id, version, initial_delay, true),
    volume(1), pan(0)
{
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
    switch (index) {
    case 0:
        strncpy(properties->text, "out1", 63);
        break;
    case 1:
        strncpy(properties->text, "out2", 63);
        break;
    }
    properties->flags = VstPinProperties::IsActive | VstPinProperties::IsStereo;
    return true;
}


// process

static float
db_to_linear(float db)
{
    return exp2(db * 0.16609640474);
}


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
            switch (data[1]) {
            case PanCC:
                panTo = fmaxf(-1, fminf(1, val*2 - 1));
                break;
            case VolumeCC:
                volumeTo = db_to_linear(val * -96);
                break;
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

    for (int frame = 0; frame < process_frames; frame++) {
        volume = update_control(volume, volumeTo);
        pan = update_control(pan, panTo);

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

        out1[frame] = left / 4 * volume;
        out2[frame] = right / 4 * volume;
    }
}
