#include <stdlib.h>
#include <string.h>

#include "interface.h"


// initialize

// Implemented by the plugin.  Presumably looks like
// return new PluginSubclass(hostCallback)->getVst();
extern VstEffectInterface *createEffectInstance(VstHostCallback hostCallback);

static VstEffectInterface *
pluginEntryPoint(VstHostCallback hostCallback)
{
    if (hostCallback(0, HostOp::VstVersion, 0, 0, 0, 0) == 0)
        return nullptr;
    else
        return createEffectInstance(hostCallback);
}

#define EXPORTED_FUNCTION extern "C" __attribute__ ((visibility("default")))

EXPORTED_FUNCTION VstEffectInterface *
VSTPluginMain(VstHostCallback hostCallback);

EXPORTED_FUNCTION VstEffectInterface *
VSTPluginMain(VstHostCallback hostCallback)
{
    return pluginEntryPoint(hostCallback);
}

#if __APPLE__

// Evidently this is for old hosts.  Maybe unnecessary.
EXPORTED_FUNCTION VstEffectInterface *main_macho(VstHostCallback hostCallback);

EXPORTED_FUNCTION VstEffectInterface *
main_macho(VstHostCallback hostCallback)
{
    return pluginEntryPoint(hostCallback);
}

#elif __linux__

EXPORTED_FUNCTION VstEffectInterface *
main_plugin(VstHostCallback hostCallback) asm ("main");

EXPORTED_FUNCTION VstEffectInterface *
main_plugin(VstHostCallback hostCallback)
{
    return VSTPluginMain(hostCallback);
}

#else
#error "not mac or linux"
#endif



static pointer_sized_int
dispatcherCB(
    VstEffectInterface *vst, int32_t op, int32_t index, pointer_sized_int value,
    void *ptr, float opt)
{
    Plugin *plugin = static_cast<Plugin *>(vst->effectPointer);

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
        plugin->getParameterLabel(index, (char *) ptr);
        return 0;
    case Op::GetParameterText:
        plugin->getParameterText(index, (char *) ptr);
        return 0;
    case Op::GetParameterName:
        plugin->getParameterName(index, (char *) ptr);
        return 0;
    // deprecated
    // case Op::Identify:
    //     return BigEndian('NvEf');
    case Op::SetSampleRate:
        plugin->setSampleRate(opt);
        return 0;
    case Op::SetBlockSize:
        plugin->setBlockSize((int32_t) value);
        return 0;
    case Op::ResumeSuspend:
        if (value)
            plugin->resume();
        else
            plugin->suspend();
        return 0;
    case Op::GetOutputPinProperties:
        return plugin->getOutputProperties(
            index, (VstPinProperties *) ptr) ? 1 : 0;
    case Op::GetInputPinProperties:
        return plugin->getInputProperties(
            index, (VstPinProperties *) ptr) ? 1 : 0;

    case Op::SetBypass:
        return plugin->setBypass(value ? true : false) ? 1 : 0;

    case Op::GetPlugInName:
    case Op::GetManufacturerProductName:
        plugin->getPluginName((char *) ptr);
        return 1;

    case Op::GetManufacturerName:
        plugin->getManufacturerName((char *) ptr);
        return 1;

    case Op::GetManufacturerVersion:
        return 1;

    case Op::CanPlugInDo:
        plugin->canDo((const char *) ptr);
        return 1;

    case Op::PreAudioProcessingEvents:
        return plugin->processEvents((VstEventBlock *) ptr);
    case Op::GetNumMidiInputChannels:
        return plugin->getNumMidiInputChannels();
    case Op::GetNumMidiOutputChannels:
        return plugin->getNumMidiOutputChannels();
    }
    return 0;
}


int
Plugin::canDo(const char *text)
{
    auto matches = [=](const char *s) { return strcmp(text, s) == 0; };

    if (matches("receiveVstEvents")
         || matches("receiveVstMidiEvent")
         || matches("receiveVstMidiEvents"))
    {
        return isSynth ? 1 : -1;
    }
    return 0;
}


static void
setParameterCB(VstEffectInterface *vst, int32_t index, float value)
{
    static_cast<Plugin *>(vst->effectPointer)->setParameter(index, value);
}

static float
getParameterCB(VstEffectInterface *vst, int32_t index)
{
    // return 0;
    return static_cast<Plugin *>(vst->effectPointer)->getParameter(index);
}

static void
processReplacingCB(
    VstEffectInterface *vst, float **inputs, float **outputs, int32_t frames)
{
    static_cast<Plugin *>(vst->effectPointer)->process(inputs, outputs, frames);
}

Plugin::Plugin(VstHostCallback hostCallback,
        int32_t numPrograms, int32_t numParameters, int32_t numInChannels,
        int32_t numOutChannels, int32_t uniqueID, int32_t version,
        int32_t initialDelay, bool isSynth) :
    hostCallback(hostCallback), isSynth(isSynth)
{
    memset(&vst, 0, sizeof vst);
    vst.interfaceIdentifier = 'VstP';
    vst.dispatchFunction = dispatcherCB;
    vst.processAudioFunction = nullptr; // obsolete
    vst.setParameterValueFunction = setParameterCB;
    vst.getParameterValueFunction = getParameterCB;
    vst.numPrograms = numPrograms;
    vst.numParameters = numParameters;
    vst.numInputChannels = numInChannels;
    vst.numOutputChannels = numOutChannels;

    vst.flags |= Flag::InplaceAudio;
    if (isSynth)
        vst.flags |= Flag::IsSynth;

    // Also called setInitialDelay.
    vst.latency = initialDelay;

    vst.effectPointer = this;
    vst.userPointer = nullptr;

    vst.plugInIdentifier = uniqueID;
    vst.plugInVersion = version;

    vst.processAudioInplaceFunction = processReplacingCB;
    vst.processDoubleAudioInplaceFunction = nullptr;
}

void
Plugin::resume()
{
    // If this plug-in is a synth or it can receive midi events we need to
    // tell the host that we want midi. In the SDK this method is marked as
    // deprecated, but some hosts rely on this behaviour.
    if (isSynth && hostCallback != nullptr) {
        hostCallback(&vst, HostOp::PlugInWantsMidi, 0, 1, 0, 0);
    }
}
