// This is just a test case using GUI controls instead of audio signals.

import("stdfaust.lib");

declare description "Sine wave using GUI UI.";

smooth = si.smooth(ba.tau2pole(0.05)); // 50 ms smoothing
dyn = hslider("dyn[unit: dB]", 0.75, 0, 1, 0.01);
gate = button("gate");
pitch = hslider("pitch[unit: nn]", 440, 1, 1000, 0.01);

process = ((dyn * gate) : smooth) * os.osc(ba.midikey2hz(pitch) : smooth);
