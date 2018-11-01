// This is a wrapper around VST2.
//
// The reason I have to do this is that the original one is by Steinberg and
// under a restrictive license.  They are trying to get rid of it, to encourage
// VST3, which is GPL.  But VST3 seems like an overengineered mess and after a
// few hours of looking at docs and examples I couldn't even figure out how to
// make a hello world.  Since I don't want any of the VST3 features, and host
// support is probably still worse than VST2, I'll just keep using VST2.
//
// This is adapted from the JUCE VST2 adaptor, which is also GPLv3.  JUCE
// itself is another all-singing all-dancing monolith, so I just copied out the
// struct definitions.

#pragma once


// VST 2 adaptor
//
// This is mostly copied from the JUCE VST adaptor:
// juce_VSTInterface.h
// juce_VST_Wrapper.cpp

// Should be __cdecl on windows.
#define VSTINTERFACECALL

// A signed integer type that's guaranteed to be large enough to hold a pointer
// without truncating it.
#if __LP64__ || _WIN64
typedef int64_t pointer_sized_int;
#else
typedef int32_t pointer_sized_int;
#endif

#if __APPLE__
    #if __LP64__
        #pragma options align=power
    #else
        #pragma options align=mac68k
    #endif
#elif __linux__
    #pragma pack(push, 8)
#else
    #error "not mac or linux"
#endif

// This has to exactly match what is expected by VST hosts.
struct VstEffectInterface {
    int32_t interfaceIdentifier;
    pointer_sized_int (VSTINTERFACECALL* dispatchFunction)
        (VstEffectInterface*, int32_t op, int32_t index,
         pointer_sized_int value, void* ptr, float opt);
    void (VSTINTERFACECALL* processAudioFunction)
        (VstEffectInterface*, float** inputs, float** outputs,
         int32_t numSamples);
    void (VSTINTERFACECALL* setParameterValueFunction)
        (VstEffectInterface*, int32_t parameterIndex, float value);
    float (VSTINTERFACECALL* getParameterValueFunction)
        (VstEffectInterface*, int32_t parameterIndex);
    int32_t numPrograms;
    int32_t numParameters;
    int32_t numInputChannels;
    int32_t numOutputChannels;
    int32_t flags;
    pointer_sized_int hostSpace1;
    pointer_sized_int hostSpace2;
    int32_t latency;
    int32_t deprecated1;
    int32_t deprecated2;
    float deprecated3;
    void* effectPointer;
    void* userPointer;
    int32_t plugInIdentifier;
    int32_t plugInVersion;
    void (VSTINTERFACECALL* processAudioInplaceFunction)
        (VstEffectInterface*, float** inputs, float** outputs,
         int32_t numSamples);
    void (VSTINTERFACECALL* processDoubleAudioInplaceFunction)
        (VstEffectInterface*, double** inputs, double** outputs,
         int32_t numSamples);
    char emptySpace[56];
};

typedef pointer_sized_int (VSTINTERFACECALL* VstHostCallback)(
    VstEffectInterface*, int32_t op, int32_t index, pointer_sized_int value,
    void* ptr, float opt);

namespace Flag {
    enum VstEffectInterfaceFlags {
        HasEditor          = 1,
        InplaceAudio       = 16,
        DataInChunks       = 32,
        IsSynth            = 256,
        InplaceDoubleAudio = 4096
    };
}

namespace Op {
    enum VstHostToPlugInOpcodes {
        Open,
        Close,
        SetCurrentProgram,
        GetCurrentProgram,
        SetCurrentProgramName,
        GetCurrentProgramName,
        GetParameterLabel,
        GetParameterText,
        GetParameterName,
        SetSampleRate = GetParameterName + 2,
        SetBlockSize,
        ResumeSuspend,
        GetEditorBounds,
        OpenEditor,
        CloseEditor,
        DrawEditor,
        GetMouse,
        EditorIdle = GetMouse + 2,
        effEditorTop,
        SleepEditor,
        Identify,
        GetData,
        SetData,
        PreAudioProcessingEvents,
        IsParameterAutomatable,
        ParameterValueForText,
        GetProgramName = ParameterValueForText + 2,
        ConnectInput = GetProgramName + 2,
        ConnectOutput,
        GetInputPinProperties,
        GetOutputPinProperties,
        GetPlugInCategory,
        SetSpeakerConfiguration = GetPlugInCategory + 7,
        SetBypass = SetSpeakerConfiguration + 2,
        GetPlugInName,
        GetManufacturerName = GetPlugInName + 2,
        GetManufacturerProductName,
        GetManufacturerVersion,
        ManufacturerSpecific,
        CanPlugInDo,
        GetTailSize,
        Idle,
        KeyboardFocusRequired = Idle + 4,
        GetVstInterfaceVersion,
        GetCurrentMidiProgram = GetVstInterfaceVersion + 5,
        GetSpeakerArrangement = GetCurrentMidiProgram + 6,
        NextPlugInUniqueID,
        StartProcess,
        StopProcess,
        SetNumberOfSamplesToProcess,
        SetSampleFloatType = SetNumberOfSamplesToProcess + 4,
        GetNumMidiInputChannels,
        GetNumMidiOutputChannels,
        Maximum = GetNumMidiOutputChannels
    };
}

namespace HostOp {
    enum VstPlugInToHostOpcodes {
        ParameterChanged,
        VstVersion,
        CurrentId,
        Idle,
        PinConnected,
        PlugInWantsMidi = PinConnected + 2,
        GetTimingInfo,
        PreAudioProcessingEvents,
        SetTime,
        TempoAt,
        GetNumberOfAutomatableParameters,
        GetParameterInterval,
        IOModified,
        NeedsIdle,
        WindowSize,
        GetSampleRate,
        GetBlockSize,
        GetInputLatency,
        GetOutputLatency,
        GetPreviousPlugIn,
        GetNextPlugIn,
        WillReplace,
        GetCurrentAudioProcessingLevel,
        GetAutomationState,
        OfflineStart,
        OfflineReadSource,
        OfflineWrite,
        OfflineGetCurrentPass,
        OfflineGetCurrentMetaPass,
        SetOutputSampleRate,
        GetOutputSpeakerConfiguration,
        GetManufacturerName,
        GetProductName,
        GetManufacturerVersion,
        ManufacturerSpecific,
        SetIcon,
        CanHostDo,
        GetLanguage,
        OpenEditorWindow,
        CloseEditorWindow,
        GetDirectory,
        UpdateView,
        ParameterChangeGestureBegin,
        ParameterChangeGestureEnd,
    };
}

namespace Max {
    enum VstMaxStringLengths {
        NameLength                     = 64,
        ParameterOrPinLabelLength      = 64,
        ParameterOrPinShortLabelLength = 8,
        CategoryLength                 = 24,
        ManufacturerStringLength       = 64,
        PlugInNameStringLength         = 64
    };
}

struct VstPinProperties {
    char text[Max::ParameterOrPinLabelLength];
    int32_t flags;
    int32_t configurationType;
    char shortText[Max::ParameterOrPinShortLabelLength];
    char unused[48];

    enum VstPinInfoFlags {
        IsActive = 1,
        IsStereo = 2,
        Valid    = 4
    };
};

struct VstEvent {
    int32_t type;
    int32_t size;
    int32_t sampleOffset;
    int32_t flags;
    char content[16];
};

struct VstEventBlock {
    enum VstEventTypes {
        Midi  = 1,
        SysEx = 6
    };
    int32_t numberOfEvents;
    pointer_sized_int future;
    // Variable length.
    VstEvent *events[2];
};

struct VstMidiEvent {
    enum VstMidiEventFlags {
        IsRealtime = 1
    };
    int32_t type;
    int32_t size;
    int32_t sampleOffset;
    int32_t flags;
    int32_t noteSampleLength;
    int32_t noteSampleOffset;
    char midiData[4];
    char tuning;
    char noteVelocityOff;
    char future1;
    char future2;
};

struct VstSysExEvent {
    int32_t type;
    int32_t size;
    int32_t offsetSamples;
    int32_t flags;
    int32_t sysExDumpSize;
    pointer_sized_int future1;
    char *sysExDump;
    pointer_sized_int future2;
};



// Plugin

class Plugin {
public:
    Plugin(VstHostCallback hostCallback,
        int32_t numPrograms, int32_t numParameters, int32_t numInChannels,
        int32_t numOutChannels, int32_t uniqueID, int32_t version,
        int32_t initialDelay, bool isSynth);
    virtual ~Plugin() {}
    VstEffectInterface *getVst() { return &vst; }

    virtual void process(float **inputs, float **outputs, int32_t frames) = 0;
    virtual int32_t processEvents(const VstEventBlock *events) { return 0; }

    virtual void open() {}
    virtual void close() {}
    virtual void suspend() {}
    virtual void resume();

    virtual void setParameter(int32_t index, float value) {}
    virtual float getParameter(int32_t index) { return 0; }
    virtual void getParameterLabel(int32_t index, char *label) {}
    virtual void getParameterText(int32_t index, char *text) {}
    virtual void getParameterName(int32_t index, char *text) {}

    virtual void setSampleRate(float sampleRate) = 0;
    virtual void setBlockSize(int32_t maxBlockSize) = 0;

    virtual int32_t getNumMidiInputChannels() { return 0; }
    virtual int32_t getNumMidiOutputChannels() { return 0; }
    virtual bool getOutputProperties(
            int32_t index, VstPinProperties *properties) {
        return false;
    }
    virtual bool getInputProperties(
            int32_t index, VstPinProperties *properties) {
        return false;
    }

    virtual bool setBypass(bool bypass) { return false; }
    virtual void getPluginName(char *name) = 0;
    virtual void getManufacturerName(char *name) = 0;

    int canDo(const char *text);
private:
    // Needed to talk to the host, e.g. send MIDI.
    const VstHostCallback hostCallback;
    VstEffectInterface vst;
    const bool isSynth;
};

#pragma pack (pop)
