import("stdfaust.lib");

declare description "Simple sine wave.";
declare control0_pitch "Pitch signal.";
declare control1_amp "Amplitude signal.";

process(pitch, amp) = amp * os.osc(ba.midikey2hz(pitch));
