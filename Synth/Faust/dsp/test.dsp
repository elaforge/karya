declare description "Instrument for tests.";
declare control0_dyn "Dynamic signal.";

import("stdfaust.lib");

// Just multiplying the inputs makes it easy to see if it got the right values.
process(dyn) = pitch * dyn
with {
    pitch = hslider("pitch[unit: nn]", 440, 1, 1000, 0.01);
};

smooth = si.smooth(ba.tau2pole(sec))
with {
    // Smooth time is equal to controlSize.  This should be synced with
    // faust-im.
    sec = 147 / 44100;
};
