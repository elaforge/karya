declare description "Instrument for tests.";
declare control0_gate "Gate signal.";
declare control1_dyn "Dynamic signal.";

import("stdfaust.lib");

// Just multiplying the inputs makes it easy to see if it got the right values.
// But I put in a limit to not exceed 1, so let's scale it down.
process(gate, dyn) = (pitch / 100) * dyn * gate
with {
    pitch = hslider("pitch[unit: nn]", 440, 1, 1000, 0.01);
};
