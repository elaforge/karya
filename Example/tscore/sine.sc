// Compile by passing to sclang, then ^C out, then copy paste cp command.
// I don't know a better way and sclang is not very documented!
var syn = SynthDef(\sine, {
    |gate, pitch, dyn=1|
    var sig = SinOsc.ar(pitch.midicps, 0, dyn * 0.35)
        * EnvGen.kr(Env.adsr(), gate, doneAction: Done.freeSelf);
    Out.ar(0, sig ! 2);
});
syn.writeDefFile();
"cp '%%.scsyndef' data/sc\n".postf(SynthDef.synthDefDir, syn.name);
