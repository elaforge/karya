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
        scorePath.reserve(64);
        mutedInstruments.reserve(8);
        clear();
    }
    void collect(std::ofstream &log, unsigned char d1, unsigned char d2);
    void clear() {
        scorePath.clear();
        mutedInstruments.clear();
        instrumentIndex = -1;
    }

    // Current playing block.
    std::string scorePath;
    std::vector<std::string> mutedInstruments;
private:
    void collect1(unsigned char d);
    int instrumentIndex;
};

// This is a simple VST that understands MIDI messages to play from a certain
// time, and plays back samples from the cache directory.  It's expected that
// offline synthesizers will be maintaining the cache.
class PlayCache : public Plugin {
public:
    PlayCache(VstHostCallback hostCallback);
    virtual ~PlayCache();
    virtual void resume() override;

    // configure

    virtual void getPluginName(char *name) override;
    virtual void getManufacturerName(char *text) override;

    virtual int32_t getNumMidiInputChannels() override { return 1; }
    virtual int32_t getNumMidiOutputChannels() override { return 0; }

    virtual bool getOutputProperties(
        int32_t index, VstPinProperties *properties) override;

    virtual void setSampleRate(float sampleRate) override {
        this->sampleRate = sampleRate;
    }
    virtual void setBlockSize(int32_t blockSize) override {
        this->maxBlockFrames = blockSize;
    }

    virtual void setParameter(int32_t index, float value) override;
    virtual float getParameter(int32_t index) override;
    virtual void getParameterLabel(int32_t index, char *label) override;
    virtual void getParameterText(int32_t index, char *text) override;
    virtual void getParameterName(int32_t index, char *text) override;

    // process

    virtual void process(float **_inputs, float **outputs, int32_t frames)
        override;
    virtual int32_t processEvents(const VstEventBlock *events) override;

private:
    void start(int32_t startOffset);

    // I don't know why setSampleRate is a float, but I don't support that.
    int sampleRate;
    // processReplacing's frames arguent will never exceed this.
    int32_t maxBlockFrames;
    // Playing from this sample, in frames since the beginning of the score.
    unsigned int startFrame;
    // True if I am playing, or should start playing once startOffset is 0.
    bool playing;
    // When playing is set, this has the number of frames to wait before
    // starting.
    int32_t startOffset;

    // parameters
    float volume;

    std::ofstream log;
    std::unique_ptr<Streamer> streamer;
    std::unique_ptr<Osc> osc;
    PlayConfig playConfig;
};
