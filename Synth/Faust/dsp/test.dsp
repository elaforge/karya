declare description "Instrument for tests.";
declare control0_gate "Gate signal.";
declare control1_dyn "Dynamic signal.";

import("stdfaust.lib");

// Just multiplying the inputs makes it easy to see if it got the right values.
process(gate, dyn) = pitch * dyn * gate
with {
    pitch = hslider("pitch[unit: nn]", 440, 1, 1000, 0.01);
};
