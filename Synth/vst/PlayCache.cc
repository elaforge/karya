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
#define LOG(MSG) LOG_TO(this->log, MSG)
#define LOG_TO(OUT, MSG) do { OUT << __FILE__ << ':' << __LINE__ << ' ' \
    << MSG << std::endl; } while (0)


// Miscellaneous constants.
enum {
    numOutputs = 2
};

// VST parameters.
enum {
    pVolume = 0
};

// VST_BASE_DIR must be defined when compiling.
static const char *logFilename = VST_BASE_DIR "/PlayCache.log";
static const char *cacheDir = VST_BASE_DIR "/cache/";

AudioEffect *createEffectInstance(audioMasterCallback audioMaster)
{
    return new PlayCache(audioMaster);
}

PlayCache::PlayCache(audioMasterCallback audioMaster) :
    AudioEffectX(audioMaster, 1, 1),
    offsetFrames(0), playing(false), delta(0), volume(1),
    log(logFilename, std::ios::app),
    samples(nullptr)
{
    if (!log.good()) {
        // Wait, how am I supposed to report this?  Can I put it in the GUI?
        // LOG("couldn't open " << logFilename);
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
    if (samples)
        delete samples;
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
    if (samples)
        delete samples;
    // This can happen if the DAW gets a NoteOn before the config msgs.
    if (playConfig.blockId.empty()) {
        LOG("play received, but blockId is empty");
        return;
    }
    LOG("start playing at delta " << delta << " block '" << playConfig.blockId
        << "' from frame " << offsetFrames);
    // TODO Samples allocation should be on a non-realtime thread, and this
    // should just be an async request to it.  However, I still need to start
    // reading samples immediately.

    samples = new Samples(log, this->sampleRate, cacheDir, playConfig.blockId,
        playConfig.mutedInstruments);

    // If there are errors then processReplacing will quit right away.
    for (const std::string &error : samples->errors()) {
        LOG("error opening samples: " << error);
    }
    for (const std::string &filename : samples->filenames()) {
        LOG("loaded sample: " << filename);
    }
    this->playConfig.clear();
    this->delta = delta;
    this->playing = true;
}

enum {
    NoteOff = 0x80,
    NoteOn = 0x90,
    Aftertouch = 0xa0,
    ControlChange = 0xb0,
    PitchBend = 0xe0,

    // ControlChange subtypes.
    AllSoundOff = 0x78,
    ResetAllControllers = 0x79,
    AllNotesOff = 0x7b
};

void
PlayConfig::collect(std::ofstream &log, unsigned char d1, unsigned char d2)
{
    // I see (0, 64) when karya quits.  It's probably the usual pitch bend
    // reset.  Rather than hack that out I'll just ignore it.  This will break
    // if a muted instrument starts with '@', but that character isn't allowed
    // in instrument names anyway.
    if (d1 == 0 && d2 == 64) {
        // LOG_TO(log, "ignoring (0, 64) pitch bend");
        return;
    }
    // LOG_TO(log, "collect: " << int(d1) << ", " << int(d2));
    collect1(d1);
    collect1(d2);
    if (instrumentIndex == -1) {
        LOG_TO(log, "blockId: '" << blockId << "'");
    } else {
        LOG_TO(log, "muted: '" << mutedInstruments[instrumentIndex] << "'");
    }
}

void
PlayConfig::collect1(unsigned char d)
{
    switch (d) {
    case 0:
        instrumentIndex++;
        break;
    case 0x7f:
        // This is sent at the beginning as an explicit clear, in case stray
        // pitch bend has accumulated junk.
        clear();
        break;
    case ' ':
        // Encode pads with space if there is an odd number of characters.
        break;
    default:
        if (instrumentIndex == -1) {
            blockId.push_back(d);
        } else {
            // TODO there's allocation here, but no point worrying about it
            // while Samples are still loaded in the audio thread.
            while (instrumentIndex >= mutedInstruments.size())
                mutedInstruments.push_back(std::string());
            mutedInstruments[instrumentIndex].push_back(d);
        }
        break;
    }
}


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
        } else if (status == NoteOn) {
            start(event->deltaFrames);
        } else if (status == Aftertouch && data[1] < 5) {
            // Use aftertouch on keys 0--4 to set offsetFrames bits 0--35.
            unsigned int index = int(data[1]) * 7;
            unsigned int val = data[2];
            // Turn off bits in the range, then replace them.
            this->offsetFrames &= ~(0x7f << index);
            this->offsetFrames |= val << index;
        } else if (status == PitchBend) {
            playConfig.collect(log, data[1], data[2]);
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
    if (!this->playing || !samples || !samples->errors().empty())
        return;

    float *sampleVals;
    sf_count_t framesLeft = samples->read(
        offsetFrames, processFrames, &sampleVals);
    if (framesLeft == 0) {
        LOG("out of samples");
        this->playing = false;
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
    int frameCount = std::min(VstInt32(framesLeft), processFrames);
    for (int frame = 0; frame < frameCount; frame++) {
        (*out1++) = sampleVals[frame*2] * volume;
        (*out2++) = sampleVals[frame*2 + 1] * volume;
    }
    this->offsetFrames += frameCount;
}
