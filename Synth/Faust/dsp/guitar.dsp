import("stdfaust.lib");

declare description "Guitar model.";
declare control0_gate "Gate.";
declare control1_pitch "Pitch signal.";
declare control2_dyn "Dynamic signal.";
declare control3_pos "Pluck position.";

process(gate, pitch, dyn, pluckPosition) =
    pm.nylonGuitar(stringLength, pluckPosition, gain, gate) * outGain
with {
    outGain = .75;
    gain = dyn;
    stringLength = freq : pm.f2l;
    freq = ba.midikey2hz(pitch);
};

/*
pluckString(stringLength, 1, 1.5, 1, gain, trigger)
    : nylonGuitarModel(stringLength, pluckPosition);
*/
