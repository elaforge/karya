import("stdfaust.lib");

declare description "Flute model.";
declare control0_gate "Gate.";
declare control1_pitch "Pitch signal.";
declare control2_dyn "Dynamic signal.";
declare control3_mouth "Mouth position."; // default 0.5

process(gate, pitch, dyn, mouthPosition) =
    pm.fluteModel(tubeLength, mouthPosition, blow)
with {
    tubeLength = pitch : pm.f2l;
    blow = pm.blower(dyn, 0.05, 2000, vibratoFreq, vibratoGain);
    vibratoFreq = 0;
    vibratoGain = 0;
    breathGain = 0.05;
    breathCutoff = 2000;
};
