import("stdfaust.lib");

declare description "Comb filter to add a pitch.";
pitch = hslider("comb_pitch[unit: nn]", 60, 1, 127, 0.01);

// Pretty much only the 0.75 to 0.99 range is useful.
// Could I map from decay in seconds?
feedback = hslider("comb_feedback", 0, 0, 1, 0.01);

process = comb, comb
with {
    comb = fi.fb_fcomb(maxDelay, delay, outputGain, feedback : smooth);
    outputGain = 1; // This just scales the overall output.
    delay = ma.SR / ba.midikey2hz(smooth(pitch));
    maxDelay = 4096; // 10hz = delay of 44100
};

smooth = si.smooth(ba.tau2pole(0.01)); // 10 ms smoothing
