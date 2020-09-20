#include <stdlib.h>
#include <string.h>

#include "interface.h"


// initialize

// Implemented by the plugin.  Presumably looks like
// return new PluginSubclass(host_callback)->get_vst();
extern VstEffectInterface *create_effect_instance(
    VstHostCallback host_callback);

static VstEffectInterface *
plugin_entry_point(VstHostCallback host_callback)
{
    if (host_callback(0, HostOp::VstVersion, 0, 0, 0, 0) == 0)
        return nullptr;
    else
        return create_effect_instance(host_callback);
}

#define EXPORTED_FUNCTION extern "C" __attribute__ ((visibility("default")))

EXPORTED_FUNCTION VstEffectInterface *
VSTPluginMain(VstHostCallback host_callback);

EXPORTED_FUNCTION VstEffectInterface *
VSTPluginMain(VstHostCallback host_callback)
{
    return plugin_entry_point(host_callback);
}

#if __APPLE__

// Evidently this is for old hosts.  Maybe unnecessary.
EXPORTED_FUNCTION VstEffectInterface *main_macho(VstHostCallback host_callback);

EXPORTED_FUNCTION VstEffectInterface *
main_macho(VstHostCallback host_callback)
{
    return plugin_entry_point(host_callback);
}

#elif __linux__

EXPORTED_FUNCTION VstEffectInterface *
main_plugin(VstHostCallback host_callback) asm ("main");

EXPORTED_FUNCTION VstEffectInterface *
main_plugin(VstHostCallback host_callback)
{
    return VSTPluginMain(host_callback);
}

#else
#error "not mac or linux"
#endif



static pointer_sized_int
dispatcher_cb(
    VstEffectInterface *vst, int32_t op, int32_t index, pointer_sized_int value,
    void *ptr, float opt)
{
    Plugin *plugin = static_cast<Plugin *>(vst->effect_pointer);

    switch (op) {
    case Op::Open:
        plugin->open();
        return 0;
        break;
    case Op::Close:
        plugin->close();
        delete plugin;
        return 1;
    case Op::GetCurrentProgram:
        return 0;
    case Op::SetCurrentProgram:
        return 0;
    case Op::SetCurrentProgramName:
        return 0;
    case Op::GetCurrentProgramName:
        * (char *) ptr = '\0';
        return 0;
    case Op::GetParameterLabel:
        plugin->get_parameter_label(index, (char *) ptr);
        return 0;
    case Op::GetParameterText:
        plugin->get_parameter_text(index, (char *) ptr);
        return 0;
    case Op::GetParameterName:
        plugin->get_parameter_name(index, (char *) ptr);
        return 0;
    // deprecated
    // case Op::Identify:
    //     return BigEndian('NvEf');
    case Op::SetSampleRate:
        plugin->set_sample_rate(opt);
        return 0;
    case Op::SetBlockSize:
        plugin->set_block_size((int32_t) value);
        return 0;
    case Op::ResumeSuspend:
        if (value)
            plugin->resume();
        else
            plugin->suspend();
        return 0;
    case Op::GetOutputPinProperties:
        return plugin->get_output_properties(
            index, (VstPinProperties *) ptr) ? 1 : 0;
    case Op::GetInputPinProperties:
        return plugin->get_input_properties(
            index, (VstPinProperties *) ptr) ? 1 : 0;

    case Op::SetBypass:
        return plugin->set_bypass(value ? true : false) ? 1 : 0;

    case Op::GetPlugInName:
    case Op::GetManufacturerProductName:
        plugin->get_plugin_name((char *) ptr);
        return 1;

    case Op::GetManufacturerName:
        plugin->get_manufacturer_name((char *) ptr);
        return 1;

    case Op::GetManufacturerVersion:
        return 1;

    case Op::CanPlugInDo:
        plugin->can_do((const char *) ptr);
        return 1;

    case Op::PreAudioProcessingEvents:
        return plugin->process_events((VstEventBlock *) ptr);
    case Op::GetNumMidiInputChannels:
        return plugin->get_num_midi_input_channels();
    case Op::GetNumMidiOutputChannels:
        return plugin->get_num_midi_output_channels();
    }
    return 0;
}


int
Plugin::can_do(const char *text)
{
    auto matches = [=](const char *s) { return strcmp(text, s) == 0; };

    if (matches("receiveVstEvents")
         || matches("receiveVstMidiEvent")
         || matches("receiveVstMidiEvents"))
    {
        return is_synth ? 1 : -1;
    }
    return 0;
}


static void
set_parameter_cb(VstEffectInterface *vst, int32_t index, float value)
{
    static_cast<Plugin *>(vst->effect_pointer)->set_parameter(index, value);
}

static float
get_parameter_cb(VstEffectInterface *vst, int32_t index)
{
    return static_cast<Plugin *>(vst->effect_pointer)->get_parameter(index);
}

static void
process_replacing_cb(
    VstEffectInterface *vst, float **inputs, float **outputs, int32_t frames)
{
    static_cast<Plugin *>(vst->effect_pointer)->process(
        inputs, outputs, frames);
}

static void
process_cb(
    VstEffectInterface *vst, float **inputs, float **outputs, int32_t frames)
{
    // Do nothing so it's obvious that it's calling the wrong thing.
    // static_cast<Plugin *>(vst->effect_pointer)->process(
    //     inputs, outputs, frames);
}

Plugin::Plugin(VstHostCallback host_callback,
        int32_t num_programs, int32_t num_parameters, int32_t num_in_channels,
        int32_t num_out_channels, int32_t unique_id, int32_t version,
        int32_t initial_delay, bool is_synth) :
    host_callback(host_callback), is_synth(is_synth)
{
    memset(&vst, 0, sizeof vst);
    vst.interface_identifier = 'VstP';
    vst.dispatch_function = dispatcher_cb;
    vst.process_audio_function = process_cb; // obsolete
    vst.set_parameter_value_function = set_parameter_cb;
    vst.get_parameter_value_function = get_parameter_cb;
    vst.num_programs = num_programs;
    vst.num_parameters = num_parameters;
    vst.num_input_channels = num_in_channels;
    vst.num_output_channels = num_out_channels;

    vst.flags |= Flag::InplaceAudio;
    if (is_synth)
        vst.flags |= Flag::IsSynth;

    // Also called set_initial_delay.
    vst.latency = initial_delay;

    vst.effect_pointer = this;
    vst.user_pointer = nullptr;

    vst.plug_in_identifier = unique_id;
    vst.plug_in_version = version;

    vst.process_audio_inplace_function = process_replacing_cb;
    vst.process_double_audio_inplace_function = nullptr;
}

void
Plugin::resume()
{
    // If this plug-in is a synth or it can receive midi events we need to
    // tell the host that we want midi. In the SDK this method is marked as
    // deprecated, but some hosts rely on this behaviour.
    if (is_synth && host_callback != nullptr) {
        host_callback(&vst, HostOp::PlugInWantsMidi, 0, 1, 0, 0);
    }
}
