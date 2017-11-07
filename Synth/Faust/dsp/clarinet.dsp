import("stdfaust.lib");

declare description "Clarinet model.";
declare control0_pitch "Pitch signal.";
declare control1_dyn "Dynamic signal.";

process(pitch, dyn) =
    pm.clarinetModel(tubeLength, pressure, reedStiffness, bellOpening)
        * outGain
with {
    tubeLength = freq : pm.f2l;
    freq = ba.midikey2hz(pitch);

    pressure = dyn;
    reedStiffness = .5; // 0 to 1
    bellOpening = .5; // 0 to 1
    outGain = .5;
};
