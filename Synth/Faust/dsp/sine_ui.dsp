// This is just a test case using GUI controls instead of audio signals.

import("stdfaust.lib");

declare description "Sine wave using GUI UI.";

process = dyn * os.osc(ba.midikey2hz(pitch))
with {
    dyn = hslider("vol[unit: dB]", 0.75, 0, 1, 0.01);
    pitch = hslider("pitch[unit: hz]", 440, 1, 1000, 0.01);
};
