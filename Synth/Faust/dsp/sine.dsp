import("stdfaust.lib");

declare description "Simple sine wave.";
declare control0_gate "Gate signal.";
declare control1_pitch "Pitch signal.";
declare control2_dyn "Dynamic signal.";

smooth = si.smooth(ba.tau2pole(0.03));

process(gate, pitch, dyn) =
    (dyn * (gate > 0)) : smooth * os.osc(ba.midikey2hz(pitch) : smooth);
