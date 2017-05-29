// Copyright 2016 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <iostream>
#include <fstream>
#include <unistd.h>

#include <sndfile.h>

#include "PlayCache.h"

// TODO this should probably append to a ring-buffer and be flushed by
// a separate thread, so I can call it from processReplacing.
#define LOG(X) do { this->log << __FILE__ << ':' << __LINE__ << ' ' \
    << X << std::endl; } while (0)


// Miscellaneous constants.
enum {
    numOutputs = 2
};

// VST parameters.
enum {
    pVolume = 0
};

// VST_BASE_DIR must be defined when compiling.
static const char *log_filename = VST_BASE_DIR "/PlayCache.log";
static const char *sample_filename = VST_BASE_DIR "/cache/out.wav";

AudioEffect *createEffectInstance(audioMasterCallback audioMaster)
{
    return new PlayCache(audioMaster);
}

PlayCache::PlayCache(audioMasterCallback audioMaster) :
    AudioEffectX(audioMaster, 1, 1),
    offsetFrames(0), playing(false), delta(0), volume(1),
    log(log_filename, std::ios::app),
    sample(nullptr)
{
    if (!log.good()) {
        // Wait, how am I supposed to report this?  Can I put it in the GUI?
        // LOG("couldn't open " << log_filename);
    }
    if (audioMaster) {
        setNumInputs(0);
        setNumOutputs(numOutputs);
        canProcessReplacing();
        isSynth();
        setUniqueID('bdpm');
    }
    LOG("started");
}

PlayCache::~PlayCache()
{
    LOG("quitting");
    if (sample)
        delete sample;
}

// configure

bool PlayCache::getEffectName(char *name)
{
    vst_strncpy(name, "PlayCache", kVstMaxEffectNameLen);
    return true;
}

bool PlayCache::getVendorString(char *text)
{
    vst_strncpy(text, "Karya", kVstMaxVendorStrLen);
    return true;
}

bool PlayCache::getProductString(char *text)
{
    vst_strncpy(text, "PlayCache", kVstMaxProductStrLen);
    return true;
}

VstInt32 PlayCache::getVendorVersion() { return 1; }

VstInt32 PlayCache::canDo(char *text)
{
    // if (!strcmp(text, "receiveVstEvents"))
    //     return 1;
    if (!strcmp(text, "receiveVstMidiEvent"))
        return 1;
    return -1;
}

VstInt32 PlayCache::getNumMidiInputChannels() { return 1; }
VstInt32 PlayCache::getNumMidiOutputChannels () { return 0; }

bool PlayCache::getOutputProperties(
    VstInt32 index, VstPinProperties *properties)
{
    if (index >= numOutputs)
        return false;
    switch (index) {
    case 0:
        vst_strncpy(properties->label, "out1", 63);
        break;
    case 1:
        vst_strncpy(properties->label, "out2", 63);
        break;
    }
    properties->flags = kVstPinIsActive | kVstPinIsStereo;
    return true;
}

// parameters

void PlayCache::setParameter(VstInt32 index, float value)
{
    switch (index) {
    case pVolume:
        this->volume = value;
        break;
    }
}

float PlayCache::getParameter(VstInt32 index)
{
    switch (index) {
    case pVolume:
        return this->volume;
    default:
        return 0;
    }
}

void PlayCache::getParameterLabel(VstInt32 index, char *label)
{
    switch (index) {
    case pVolume:
        vst_strncpy(label, "dB", kVstMaxParamStrLen);
        break;
    }
}

void PlayCache::getParameterDisplay(VstInt32 index, char *text)
{
    switch (index) {
    case pVolume:
        dB2string(this->volume, text, kVstMaxParamStrLen);
        break;
    }
}

void PlayCache::getParameterName(VstInt32 index, char *text)
{
    switch (index) {
    case pVolume:
        vst_strncpy(text, "volume", kVstMaxParamStrLen);
        break;
    }
}


// process

void
PlayCache::start(VstInt32 delta)
{
    if (sample)
        delete sample;
    sample = new Sample(sample_filename);
    if (!sample->error().empty()) {
        LOG("error opening sample: " << sample->error());
    }
    LOG("start playing at delta " << delta << " from frame " << offsetFrames);
    this->delta = delta;
    this->playing = true;
}

enum {
    NoteOff = 0x80,
    ControlChange = 0xb0,
    AllSoundOff = 0x78,
    ResetAllControllers = 0x79,
    AllNotesOff = 0x7b
};

VstInt32 PlayCache::processEvents(VstEvents *events)
{
    for (VstInt32 i = 0; i < events->numEvents; i++) {
        if (events->events[i]->type != kVstMidiType)
            continue;

        VstMidiEvent *event = (VstMidiEvent *) events->events[i];
        const char *data = event->midiData;

        // Parse the protocol emitted by Perform.Im.Play.
        int status = data[0] & 0xf0;
        if (status == ControlChange
                && (data[1] == AllSoundOff || data[1] == AllNotesOff
                    || data[i] == ResetAllControllers)) {
            // See NOTE [play-im] for why I stop on these msgs, but not
            // NoteOff.
            this->offsetFrames = 0;
            this->playing = false;
        } else if (status == 0x90) {
            start(event->deltaFrames);
        } else if (status == 0xa0 && data[1] < 5) {
            // Use aftertouch on keys 0--4 to set offsetFrames bits 0--35.
            unsigned int index = int(data[1]) * 7;
            unsigned int val = data[2];
            // Turn off bits in the range, then replace them.
            this->offsetFrames &= ~(0x7f << index);
            this->offsetFrames |= val << index;
        }
    }
    return 1;
}

void PlayCache::processReplacing(
    float **_inputs, float **outputs, VstInt32 processFrames)
{
    float *out1 = outputs[0];
    float *out2 = outputs[1];

    memset(out1, 0, processFrames * sizeof(float));
    memset(out2, 0, processFrames * sizeof(float));

    // TODO fade out if this makes a nasty click.
    if (!this->playing || !sample || !sample->error().empty())
        return;

    float *samples;
    sf_count_t framesLeft = sample->read(offsetFrames, &samples);
    if (framesLeft == 0) {
        LOG("out of samples");
        this->playing = false;
        return;
    }

    if (this->sample->samplerate() != this->sampleRate) {
        LOG("vst has sample rate " << sampleRate << " but sample has "
            << sample->samplerate());
        return;
    }

    // LOG("process frames " << processFrames << " delta: " << delta
    //     << " offset: " << offsetFrames << " left: " << framesLeft);

    if (this->delta > 0) {
        if (delta >= processFrames) {
            delta -= processFrames;
            return;
        }
        memset(out1, 0, delta * sizeof(float));
        memset(out2, 0, delta * sizeof(float));
        out1 += delta;
        out2 += delta;
        processFrames -= delta;
        this->delta = 0;
    }
    int frames = std::min(VstInt32(framesLeft), processFrames);
    for (int frame = 0; frame < frames; frame++) {
        (*out1++) = samples[frame*2] * volume;
        (*out2++) = samples[frame*2 + 1] * volume;
    }
    this->offsetFrames += frames;
}
