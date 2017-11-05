import("stdfaust.lib");

declare description "Simple sine wave.";
declare control0_pitch "Pitch signal.";
declare control1_dyn "Dynamic signal.";

process(pitch, dyn) = dyn * os.osc(ba.midikey2hz(pitch));
