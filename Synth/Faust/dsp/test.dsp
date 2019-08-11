declare description "Instrument for tests.";
declare control0_dyn "Dynamic signal.";

// Just multiplying the inputs makes it easy to see if it got the right values.
process(dyn) = pitch * dyn
with {
    pitch = hslider("pitch[unit: nn]", 440, 1, 1000, 0.01);
};
