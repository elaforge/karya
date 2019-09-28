import("stdfaust.lib");
inst = library("instruments.lib");

// gate = button("gate");
gate = 1;
pitch = hslider("pitch", 36, 1, 127, 1);

vol = .02;

delta = hslider("delta", .02, 0, 1, .005);
// TODO make it stetch to inharmonics
// Or just set their pitches individually.  I can then do chords.
stretch = hslider("stretch", 0, 0, 1, .01);
// delta = .03;

process = panned_harmonics : *(gate*vol), *(gate*vol); // : inst.instrReverb;

// panned oscils

panned_oscils = par(i, 9, pan(i/8*2 - 1, oscil(hz + delta * (i-4)))) :> _, _
with {
    hz = ba.midikey2hz(pitch);
};

oscil(hz) = sine(1) + sum(i, 8, sine(i+5) * n)
with {
    sine(harmonic) = os.osc(hz * harmonic);
    n = .7;
};

// panned harmonics
panned_harmonics = // (gate, pitch) =
    par(i, 9, panned_oscil(hz + delta * (i-4))) :> _, _
with {
    hz = ba.midikey2hz(pitch);
};

panned_oscil(hz) =
    (sine(1) <: _, _), par(i, 15, pan(i/8 * 2 - 1, sine(i+5) * n))
with {
    sine(harmonic) = os.osc(hz * harmonic + stretch);
    n = .7;
};

pan(s) = _ <: *((1 - p) : sqrt), *(p : sqrt)
    with { p = (min(1, max(-1, s)) + 1) / 2; };
