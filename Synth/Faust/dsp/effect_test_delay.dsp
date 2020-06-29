import("stdfaust.lib");

declare description "Sample delay for testing.";
delay = hslider("delay", 0, 0, 1024, 1);
gain = hslider("gain", 1, 0, 10, 0.01);

process = delay1, delay1
with {
    delay1 = de.delay(1024, delay) * gain;
};
