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
    int32_t interface_identifier;
    pointer_sized_int (VSTINTERFACECALL* dispatch_function)
        (VstEffectInterface*, int32_t op, int32_t index,
         pointer_sized_int value, void* ptr, float opt);
    void (VSTINTERFACECALL* process_audio_function)
        (VstEffectInterface*, float** inputs, float** outputs,
         int32_t num_samples);
    void (VSTINTERFACECALL* set_parameter_value_function)
        (VstEffectInterface*, int32_t parameter_index, float value);
    float (VSTINTERFACECALL* get_parameter_value_function)
        (VstEffectInterface*, int32_t parameter_index);
    int32_t num_programs;
    int32_t num_parameters;
    int32_t num_input_channels;
    int32_t num_output_channels;
    int32_t flags;
    pointer_sized_int host_space1;
    pointer_sized_int host_space2;
    int32_t latency;
    int32_t deprecated1;
    int32_t deprecated2;
    float deprecated3;
    void* effect_pointer;
    void* user_pointer;
    int32_t plug_in_identifier;
    int32_t plug_in_version;
    void (VSTINTERFACECALL* process_audio_inplace_function)
        (VstEffectInterface*, float** inputs, float** outputs,
         int32_t num_samples);
    void (VSTINTERFACECALL* process_double_audio_inplace_function)
        (VstEffectInterface*, double** inputs, double** outputs,
         int32_t num_samples);
    char empty_space[56];
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
        EffEditorTop,
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
    int32_t configuration_type;
    char short_text[Max::ParameterOrPinShortLabelLength];
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
    int32_t sample_offset;
    int32_t flags;
    char content[16];
};

struct VstEventBlock {
    enum VstEventTypes {
        Midi  = 1,
        SysEx = 6
    };
    int32_t number_of_events;
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
    int32_t sample_offset;
    int32_t flags;
    int32_t note_sample_length;
    int32_t note_sample_offset;
    char midi_data[4];
    char tuning;
    char note_velocity_off;
    char future1;
    char future2;
};

struct VstSysExEvent {
    int32_t type;
    int32_t size;
    int32_t offset_samples;
    int32_t flags;
    int32_t sys_ex_dump_size;
    pointer_sized_int future1;
    char *sys_ex_dump;
    pointer_sized_int future2;
};



// Plugin

class Plugin {
public:
    Plugin(VstHostCallback host_callback,
        int32_t num_programs, int32_t num_parameters, int32_t num_in_channels,
        int32_t num_out_channels, int32_t unique_id, int32_t version,
        int32_t initial_delay, bool is_synth);
    virtual ~Plugin() {}
    VstEffectInterface *get_vst() { return &vst; }

    virtual void process(float **inputs, float **outputs, int32_t frames) = 0;
    virtual int32_t process_events(const VstEventBlock *events) { return 0; }

    virtual void open() {}
    virtual void close() {}
    virtual void suspend() {}
    virtual void resume();

    virtual void set_parameter(int32_t index, float value) {}
    virtual float get_parameter(int32_t index) { return 0; }
    virtual void get_parameter_label(int32_t index, char *label) {}
    virtual void get_parameter_text(int32_t index, char *text) {}
    virtual void get_parameter_name(int32_t index, char *text) {}

    virtual void set_sample_rate(float sample_rate) = 0;
    virtual void set_block_size(int32_t max_block_size) = 0;

    virtual int32_t get_num_midi_input_channels() { return 0; }
    virtual int32_t get_num_midi_output_channels() { return 0; }
    virtual bool get_output_properties(
            int32_t index, VstPinProperties *properties) {
        return false;
    }
    virtual bool get_input_properties(
            int32_t index, VstPinProperties *properties) {
        return false;
    }

    virtual bool set_bypass(bool bypass) { return false; }
    virtual void get_plugin_name(char *name) = 0;
    virtual void get_manufacturer_name(char *name) = 0;

    int can_do(const char *text);
private:
    // Needed to talk to the host, e.g. send MIDI.
    const VstHostCallback host_callback;
    VstEffectInterface vst;
    const bool is_synth;
};

#pragma pack (pop)
