// Copyright 2020 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once
#include <fstream>
#include <string>
#include <vector>

#include "Synth/vst2/interface.h"


class Panner : public Plugin {
public:
    Panner(VstHostCallback host_callback);
    virtual ~Panner();
    virtual void resume() override;

    // configure

    virtual void get_plugin_name(char *name) override;
    virtual void get_manufacturer_name(char *text) override;

    virtual int32_t get_num_midi_input_channels() override { return 1; }
    virtual int32_t get_num_midi_output_channels() override { return 0; }

    virtual bool get_output_properties(
        int32_t index, VstPinProperties *properties) override;
    virtual bool get_input_properties(
        int32_t index, VstPinProperties *properties) override;

    virtual void set_sample_rate(float sample_rate) override {
        this->sample_rate = sample_rate;
    }
    virtual void set_block_size(int32_t block_size) override {
        this->max_block_frames = block_size;
    }

    virtual void set_parameter(int32_t index, float value) override;
    virtual float get_parameter(int32_t index) override;
    virtual void get_parameter_label(int32_t index, char *label) override;
    virtual void get_parameter_text(int32_t index, char *text) override;
    virtual void get_parameter_name(int32_t index, char *text) override;

    // process

    virtual void process(float **inputs, float **outputs, int32_t frames)
        override;
    virtual int32_t process_events(const VstEventBlock *events) override;

private:
    float volume, volume_to;
    float pan, pan_to;

    int volume_cc, pan_cc;
    float volume_param;
    std::vector<float> buffer;

    int sample_rate;
    // process_replacing's frames arguent will never exceed this.
    int32_t max_block_frames;
    std::ofstream log;
};
