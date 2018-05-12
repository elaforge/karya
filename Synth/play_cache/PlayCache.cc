// Copyright 2016 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <fstream>
#include <iostream>
#include <math.h>
#include <unistd.h>

#include <sndfile.h>

#include "PlayCache.h"
#include "log.h"


// TODO LOG() called from the audio thread should put them on a ringbuffer

// Miscellaneous constants.
enum {
    numOutputs = 2,
    numInputs = 0,
    numPrograms = 0,
    // How much inherent delay the plugin has.  I'm just streaming samples, so
    // it's 0.
    initialDelay = 0
};

// VST parameters.
enum {
    pVolume = 0,
    numParameters
};

// VST_BASE_DIR must be defined when compiling.
static const char *logFilename = VST_BASE_DIR "/PlayCache.log";
static const char *cacheDir = VST_BASE_DIR "/cache/";


// Magic function name, called by VSTMain, which is called by the host.
VstEffectInterface *
createEffectInstance(VstHostCallback hostCallback)
{
    return (new PlayCache(hostCallback))->getVst();
}

// Plugin::Plugin(VstHostCallback hostCallback,
//         int32_t numPrograms, int32_t numParameters, int32_t numInChannels,
//         int32_t numOutChannels, int32_t uniqueID, int32_t version,
//         int32_t initialDelay, bool isSynth) :
PlayCache::PlayCache(VstHostCallback hostCallback) :
    Plugin(hostCallback, numPrograms, numParameters, numInputs, numOutputs,
        'bdpm', 1, initialDelay, true),
    offsetFrames(0), playing(false), delta(0), volume(1),
    log(logFilename, std::ios::app)
{
    if (!log.good()) {
        // Wait, how am I supposed to report this?  Can I put it in the GUI?
        // LOG("couldn't open " << logFilename);
    }
    LOG("started");
}

PlayCache::~PlayCache()
{
    LOG("quitting");
}

void
PlayCache::resume()
{
    if (!streamer.get() || streamer->sampleRate != sampleRate
        || streamer->maxFrames != maxBlockFrames)
    {
        streamer.reset(
            new Streamer(log, numOutputs, sampleRate, maxBlockFrames));
    }
    Plugin::resume();
}

// configure

void
PlayCache::getPluginName(char *name)
{
    strncpy(name, "PlayCache", Max::PlugInNameStringLength);
}

void
PlayCache::getManufacturerName(char *text)
{
    strncpy(text, "Karya", Max::ManufacturerStringLength);
}

bool
PlayCache::getOutputProperties(int32_t index, VstPinProperties *properties)
{
    if (index >= numOutputs)
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


// parameters

void
PlayCache::setParameter(int32_t index, float value)
{
    switch (index) {
    case pVolume:
        this->volume = value;
        break;
    }
}

float
PlayCache::getParameter(int32_t index)
{
    switch (index) {
    case pVolume:
        return this->volume;
    default:
        return 0;
    }
}

void
PlayCache::getParameterLabel(int32_t index, char *label)
{
    switch (index) {
    case pVolume:
        strncpy(label, "dB", Max::ParameterOrPinLabelLength);
        break;
    }
}

static float
linearToDb(float f)
{
    return log10(f) * 20;
}

void
PlayCache::getParameterText(int32_t index, char *text)
{
    switch (index) {
    case pVolume:
        snprintf(text, Max::ParameterOrPinLabelLength, "%.2fdB",
            linearToDb(this->volume));
        break;
    }
}

void
PlayCache::getParameterName(int32_t index, char *text)
{
    switch (index) {
    case pVolume:
        strncpy(text, "volume", Max::ParameterOrPinLabelLength);
        break;
    }
}


// process

// Start streaming samples from offsetFrames, starting at the given delta.
void
PlayCache::start(int32_t delta)
{
    // Hopefully this should wind up statically allocated.
    static std::string samplesDir(4096, ' ');

    // This can happen if the DAW gets a NoteOn before the config msgs.
    if (playConfig.scorePath.empty()) {
        LOG("play received, but scorePath is empty");
        return;
    }
    LOG("start playing at delta " << delta << " block '" << playConfig.scorePath
        << "' from frame " << offsetFrames);

    samplesDir.clear();
    samplesDir += cacheDir;
    samplesDir += playConfig.scorePath;
    streamer->start(samplesDir, offsetFrames, playConfig.mutedInstruments);
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
        // LOG("ignoring (0, 64) pitch bend");
        return;
    }
    // LOG("collect: " << int(d1) << ", " << int(d2));
    collect1(d1);
    collect1(d2);
    if (instrumentIndex == -1) {
        // LOG("scorePath: '" << scorePath << "'");
    } else {
        // LOG("muted: '" << mutedInstruments[instrumentIndex] << "'");
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
            scorePath.push_back(d);
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


int32_t
PlayCache::processEvents(const VstEventBlock *events)
{
    for (int32_t i = 0; i < events->numberOfEvents; i++) {
        if (events->events[i]->type != VstEventBlock::Midi)
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
            LOG("note off");
        } else if (status == NoteOn) {
            start(event->sampleOffset);
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

void
PlayCache::process(float **_inputs, float **outputs, int32_t processFrames)
{
    float *out1 = outputs[0];
    float *out2 = outputs[1];

    memset(out1, 0, processFrames * sizeof(float));
    memset(out2, 0, processFrames * sizeof(float));

    // TODO fade out if this makes a nasty click.
    if (!this->playing)
        return;

    float *sampleVals;
    if (!streamer->read(processFrames, &sampleVals)) {
        LOG("out of samples");
        this->playing = false;
        return;
    }

    // LOG("process frames " << processFrames << " delta: " << delta
    //     << " offset: " << offsetFrames;

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
    for (int frame = 0; frame < processFrames; frame++) {
        (*out1++) = sampleVals[frame*2] * volume;
        (*out2++) = sampleVals[frame*2 + 1] * volume;
    }
    // I don't actually use offsetFrames any more, so I don't technically need
    // to update it.
    this->offsetFrames += processFrames;
}
