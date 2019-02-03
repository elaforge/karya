// Copyright 2016 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once
#include <fstream>
#include <vector>
#include <string>

#include "Synth/vst2/interface.h"

#include "Osc.h"
#include "Streamer.h"


// Per-play config.  This decodes the config sent by
// 'Perform.Im.Play.encode_play_config'.
//
// This is nothing like a robust protocol, because I just assume the PlayConfig
// MIDI msgs will be complete before the start play one comes in, and there's
// no protection against the host deciding to toss in some MIDI just for fun.
// It seems to work for now, but if it gets to be too much bother I can switch
// to sysex, or perhaps a separate socket and a real protocol like OSC.
class PlayConfig {
public:
    PlayConfig() {
        // Try to avoid some allocation, not that I'm consistent about that.
        score_path.reserve(64);
        muted_instruments.reserve(8);
        clear();
    }
    void collect(std::ofstream &log, unsigned char d1, unsigned char d2);
    void clear() {
        score_path.clear();
        muted_instruments.clear();
        instrument_index = -1;
    }

    // Current playing block.
    std::string score_path;
    std::vector<std::string> muted_instruments;
private:
    void collect1(unsigned char d);
    int instrument_index;
};

// This is a simple VST that understands MIDI messages to play from a certain
// time, and plays back samples from the cache directory.  It's expected that
// offline synthesizers will be maintaining the cache.
class PlayCache : public Plugin {
public:
    PlayCache(VstHostCallback host_callback);
    virtual ~PlayCache();
    virtual void resume() override;

    // configure

    virtual void get_plugin_name(char *name) override;
    virtual void get_manufacturer_name(char *text) override;

    virtual int32_t get_num_midi_input_channels() override { return 1; }
    virtual int32_t get_num_midi_output_channels() override { return 0; }

    virtual bool get_output_properties(
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

    virtual void process(float **_inputs, float **outputs, int32_t frames)
        override;
    virtual int32_t process_events(const VstEventBlock *events) override;

private:
    void start(int32_t start_offset);

    // I don't know why set_sample_rate is a float, but I don't support that.
    int sample_rate;
    // process_replacing's frames arguent will never exceed this.
    int32_t max_block_frames;
    // Playing from this sample, in frames since the beginning of the score.
    unsigned int start_frame;
    // True if I am playing, or should start playing once start_offset is 0.
    bool playing;
    // When playing is set, this has the number of frames to wait before
    // starting.
    int32_t start_offset;

    // parameters
    float volume;

    std::ofstream log;
    std::unique_ptr<TracksStreamer> streamer;
    std::unique_ptr<Osc> osc;
    PlayConfig play_config;
};
