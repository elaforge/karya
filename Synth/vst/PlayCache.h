// Copyright 2016 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once
#include <fstream>
#include <vector>
#include <string>

#include "public.sdk/source/vst2.x/audioeffectx.h"

#include "Samples.h"


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
        blockId.reserve(64);
        mutedInstruments.reserve(8);
        clear();
    }
    void collect(std::ofstream &log, unsigned char d1, unsigned char d2);
    void clear() {
        blockId.clear();
        mutedInstruments.clear();
        instrumentIndex = -1;
    }

    // Current playing block.
    std::string blockId;
    std::vector<std::string> mutedInstruments;
private:
    void collect1(unsigned char d);
    int instrumentIndex;
};

// This is a simple VST that understands MIDI messages to play from a certain
// time, and plays back samples from the cache directory.  It's expected that
// offline synthesizers will be maintaining the cache.
class PlayCache : public AudioEffectX {
public:
    PlayCache(audioMasterCallback audioMaster);
    virtual ~PlayCache();

    // configure

    virtual bool getEffectName(char *name);
    virtual bool getVendorString(char *text);
    virtual bool getProductString(char *text);
    virtual VstInt32 getVendorVersion();
    virtual VstInt32 canDo(char *text);

    virtual VstInt32 getNumMidiInputChannels();
    virtual VstInt32 getNumMidiOutputChannels();

    virtual bool getOutputProperties(
        VstInt32 index, VstPinProperties *properties);

    // virtual void setSampleRate(float sampleRate);
    // virtual void setBlockSize(VstInt32 blockSize);

    // virtual void setProgram(VstInt32 program);
    // virtual void setProgramName(char *name);
    // virtual void getProgramName(char *name);
    // virtual bool getProgramNameIndexed(
    //     VstInt32 category, VstInt32 index, char *text);

    virtual void setParameter(VstInt32 index, float value);
    virtual float getParameter(VstInt32 index);
    virtual void getParameterLabel(VstInt32 index, char *label);
    virtual void getParameterDisplay(VstInt32 index, char *text);
    virtual void getParameterName(VstInt32 index, char *text);

    // only used if canDo "midiProgramNames"
    // virtual VstInt32 getMidiProgramName(
    //     VstInt32 channel, MidiProgramName *midiProgramName);
    // virtual VstInt32 getCurrentMidiProgram(
    //     VstInt32 channel, MidiProgramName *currentProgram);
    // virtual VstInt32 getMidiProgramCategory(
    //     VstInt32 channel, MidiProgramCategory *category);
    // virtual bool hasMidiProgramsChanged(VstInt32 channel);
    // virtual bool getMidiKeyName(VstInt32 channel, MidiKeyName *keyName);

    // process

    virtual void processReplacing(
        float **_inputs, float **outputs, VstInt32 frames);
    virtual VstInt32 processEvents(VstEvents *events);

private:
    void start(VstInt32 delta);
    // Playing from this sample, in frames since the beginning of the score.
    unsigned int offsetFrames;
    // True if I am playing, or should start playing once delta is 0.
    bool playing;
    // When playing is set, this has the number of frames to wait before
    // starting.
    VstInt32 delta;

    // parameters
    float volume;

    std::ofstream log;
    Samples *samples;
    PlayConfig playConfig;
};
