import("stdfaust.lib");

declare description "Comb filter to add a pitch.";
pitch = hslider("pitch[unit: nn]", 440, 1, 1000, 0.01);

// gain could be decay in seconds
gain = hslider("gain", 0.5, 0, 1, 0.01);

process = comb, comb
with {
    comb = fi.fb_fcomb(maxDelay, delay, 1, gain : smooth);
    delay = ba.midikey2hz(pitch) : smooth : /(ma.SR);
    maxDelay = 1024;
};

smooth = si.smooth(ba.tau2pole(0.05)); // 50 ms smoothing
