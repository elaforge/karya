import("stdfaust.lib");

declare description "Guitar model.";
declare control0_gate "Gate.";
declare control1_pitch "Pitch signal.";
declare control2_dyn "constant:Dynamic signal.";
declare control3_pos "constant:Pluck position.";

process(gate, pitch, dyn, pluckPosition) =
    nylonGuitar(stringLength, pluckPosition, gain, gate) * outGain
with {
    outGain = .75;
    gain = dyn;
    stringLength = freq : pm.f2l;
    freq = ba.midikey2hz(pitch);
};

nylonGuitar(stringLength, pluckPosition, gain, trigger) =
    pm.pluckString(stringLength, 1,1.5, 1, gain, trigger)
    : nylonGuitarModel(stringLength, pluckPosition);

nylonGuitarModel(stringLength, pluckPosition, excitation) = pm.endChain(egChain)
with {
    egChain = pm.chain(guitarNuts(brightness, absorption)
        : pm.nylonString(stringL, pluckPosition, excitation)
        : pm.guitarBridge : pm.guitarBody : pm.out);
    stringL = stringLength - lengthTuning;
    lengthTuning = 0.11;

    // brightness = 0.4;
    // absorption = 0.5;
    brightness = 0.9;
    absorption = 0.2;
};

guitarNuts(brightness, absorption) =
    pm.lTermination(0 - pm.bridgeFilter(brightness, absorption), pm.basicBlock);

/*
pm.bridgeFilter(brightness, absorption)

pluckString(stringLength, 1, 1.5, 1, gain, trigger)
    : nylonGuitarModel(stringLength, pluckPosition);
*/
