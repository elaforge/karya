import("stdfaust.lib");

declare description "Guitar model.";
declare control0_pitch "Pitch signal.";
declare control1_dyn "Dynamic signal.";
declare control2_pos "Pluck position.";

process(pitch, dyn, pluckPosition) =
    pm.nylonGuitar(stringLength, pluckPosition, gain, gate) * outGain
with {
    outGain = .5;
    gain = dyn;
    gate = 1;
    stringLength = freq : pm.f2l;
    freq = ba.midikey2hz(pitch);
};

/*
pluckString(stringLength, 1, 1.5, 1, gain, trigger)
    : nylonGuitarModel(stringLength, pluckPosition);
*/
