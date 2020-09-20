// Copyright 2016 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <fstream>
#include <iostream>
#include <math.h>
#include <string.h>
#include <unistd.h>

#include <sndfile.h>

#include "Synth/Shared/config.h"
#include "PlayCache.h"
#include "log.h"


// TODO LOG() called from the audio thread should put them on a ringbuffer

// Miscellaneous constants.
enum {
    channels = 2,
    num_inputs = 0,
    num_programs = 0,
    // How much inherent delay the plugin has.  I'm just streaming samples, so
    // it's 0.
    initial_delay = 0
};

// VST parameters.
enum {
    p_volume = 0,
    num_parameters
};

// VST_BASE_DIR must be defined when compiling.
static const char *log_filename = VST_BASE_DIR "/PlayCache.log";
static const char *cache_dir = VST_BASE_DIR "/cache/";

static const int32_t unique_id = 'bdpm';
static const int32_t version = 1;

// Magic function name, called by VSTMain, which is called by the host.
VstEffectInterface *
create_effect_instance(VstHostCallback host_callback)
{
    return (new PlayCache(host_callback))->get_vst();
}

// Plugin::Plugin(VstHostCallback host_callback,
//     int32_t num_programs, int32_t num_parameters, int32_t num_in_channels,
//     int32_t num_out_channels, int32_t unique_id, int32_t version,
//     int32_t initial_delay, bool is_synth) :
PlayCache::PlayCache(VstHostCallback host_callback) :
    Plugin(host_callback, num_programs, num_parameters, num_inputs, channels,
        unique_id, version, initial_delay, true),
    start_frame(0), playing(false), start_offset(0), volume(1),
    log(log_filename, std::ios::app)
{
    if (!log.good()) {
        // Wait, how am I supposed to report this?  Can I put it in the GUI?
        // LOG("couldn't open " << log_filename);
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
    bool changed = false;
    if (!streamer.get() || streamer->sample_rate != sample_rate
            || streamer->max_frames != max_block_frames)
    {
        streamer.reset(
            new TracksStreamer(log, channels, sample_rate, max_block_frames));
        changed = true;
    }
    if (!osc.get() || changed) {
        osc.reset(new Osc(log, channels, sample_rate, max_block_frames));
    }
    Plugin::resume();
}

// configure

void
PlayCache::get_plugin_name(char *name)
{
    strncpy(name, "PlayCache", Max::PlugInNameStringLength);
}

void
PlayCache::get_manufacturer_name(char *text)
{
    strncpy(text, "Karya", Max::ManufacturerStringLength);
}

bool
PlayCache::get_output_properties(int32_t index, VstPinProperties *properties)
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


// parameters

void
PlayCache::set_parameter(int32_t index, float value)
{
    switch (index) {
    case p_volume:
        this->volume = value;
        break;
    }
}

float
PlayCache::get_parameter(int32_t index)
{
    switch (index) {
    case p_volume:
        return this->volume;
    default:
        return 0;
    }
}

void
PlayCache::get_parameter_label(int32_t index, char *label)
{
    switch (index) {
    case p_volume:
        strncpy(label, "dB", Max::ParameterOrPinLabelLength);
        break;
    }
}

static float
linear_to_db(float f)
{
    return log10(f) * 20;
}

void
PlayCache::get_parameter_text(int32_t index, char *text)
{
    switch (index) {
    case p_volume:
        snprintf(text, Max::ParameterOrPinLabelLength, "%.2fdB",
            linear_to_db(this->volume));
        break;
    }
}

void
PlayCache::get_parameter_name(int32_t index, char *text)
{
    switch (index) {
    case p_volume:
        strncpy(text, "volume", Max::ParameterOrPinLabelLength);
        break;
    }
}


// process

// Start streaming samples from start_frame, starting start_offset from now.
void
PlayCache::start(int32_t start_offset)
{
    // Hopefully this should wind up statically allocated.
    static std::string samples_dir(4096, ' ');

    // This can happen if the DAW gets a NoteOn before the config msgs.
    if (play_config.score_path.empty()) {
        LOG("play received, but score_path is empty");
        return;
    }
    LOG("start playing at start_offset " << start_offset
        << " block '" << play_config.score_path
        << "' from frame " << start_frame);

    samples_dir.clear();
    samples_dir += cache_dir;
    samples_dir += play_config.score_path;
    streamer->start(samples_dir, start_frame, play_config.muted_instruments);
    this->play_config.clear();
    this->start_offset = start_offset + START_LATENCY_FRAMES;
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
    if (instrument_index == -1) {
        // LOG("score_path: '" << score_path << "'");
    } else {
        // LOG("muted: '" << muted_instruments[instrument_index] << "'");
    }
}

void
PlayConfig::collect1(unsigned char d)
{
    switch (d) {
    case 0:
        instrument_index++;
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
        if (instrument_index == -1) {
            score_path.push_back(d);
        } else {
            // TODO there's allocation here, but no point worrying about it
            // while Samples are still loaded in the audio thread.
            while (instrument_index >= muted_instruments.size())
                muted_instruments.push_back(std::string());
            muted_instruments[instrument_index].push_back(d);
        }
        break;
    }
}


int32_t
PlayCache::process_events(const VstEventBlock *events)
{
    for (int32_t i = 0; i < events->number_of_events; i++) {
        if (events->events[i]->type != VstEventBlock::Midi)
            continue;

        VstMidiEvent *event = (VstMidiEvent *) events->events[i];
        const char *data = event->midi_data;

        // Parse the protocol emitted by Perform.Im.Play.
        int status = data[0] & 0xf0;
        if (status == ControlChange
                && (data[1] == AllSoundOff || data[1] == AllNotesOff
                    || data[i] == ResetAllControllers)) {
            // See NOTE [play-im] for why I stop on these msgs, but not
            // NoteOff.
            this->start_frame = 0;
            this->playing = false;
            LOG("note off");
        } else if (status == NoteOn) {
            start(event->sample_offset);
        } else if (status == Aftertouch && data[1] < 5) {
            // Use aftertouch on keys 0--4 to set start_frame bits 0--35.
            unsigned int index = int(data[1]) * 7;
            unsigned int val = data[2];
            // Turn off bits in the range, then replace them.
            this->start_frame &= ~(0x7f << index);
            this->start_frame |= val << index;
        } else if (status == PitchBend) {
            play_config.collect(log, data[1], data[2]);
        }
    }
    return 1;
}

void
PlayCache::process(float **_inputs, float **outputs, int32_t process_frames)
{
    float *out1 = outputs[0];
    float *out2 = outputs[1];

    memset(out1, 0, process_frames * sizeof(float));
    memset(out2, 0, process_frames * sizeof(float));

    float *osc_samples;
    bool osc_done = !osc.get()
        || this->osc->read(channels, process_frames, &osc_samples);
    if (!osc_done) {
        for (int frame = 0; frame < process_frames; frame++) {
            out1[frame] += osc_samples[frame*2] * volume;
            out2[frame] += osc_samples[frame*2 + 1] * volume;
        }
    }

    if (playing) {
        // Leave some silence at the beginning if there is a start_offset.
        if (start_offset > 0) {
            int32_t offset = std::min(process_frames, start_offset);
            out1 += offset;
            out2 += offset;
            process_frames -= offset;
            start_offset -= offset;
        }

        float *stream_samples;
        if (this->streamer->read(channels, process_frames, &stream_samples)) {
            LOG("out of samples");
            this->playing = false;
        } else {
            for (int frame = 0; frame < process_frames; frame++) {
                out1[frame] += stream_samples[frame*2] * volume;
                out2[frame] += stream_samples[frame*2 + 1] * volume;
            }
        }
    }
}
