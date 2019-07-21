declare name "Tambura";
declare description "Pseudo physical model of an Indian Tambura/Tanpura";
declare author "Oli Larkin (contact@olilarkin.co.uk)";
declare copyright "Oliver Larkin";
declare version "1.0";
declare licence "GPL";

// TODO
// - pitch env doesn't get triggered by autoplucker
// - autoplucker fixed to 4 strings

import("stdfaust.lib");

line (value, time) = state~(_,_) : !,_
with {
    state (t, c) = nt, ba.if (nt <= 0, value, c+(value - c) / nt)
    with {
        nt = ba.if(value != value', samples, t-1);
        samples = time * ma.SR / 1000.0;
    };
};

dtmax = 4096;

// tunings of the four strings, ratios of f0
ratios(0) = 1.5;
ratios(1) = 2.;
ratios(2) = 2.01;
ratios(3) = 1.;

NStrings = 4;

sm = si.smooth(ba.tau2pole(0.05)); // 50 ms smoothing

// ratios(i) = hslider("/h:main/ratio%1i [style:knob]", 1., 0.1, 2., 0.001);
pluck(i) = button("/h:trigger/pluck%1i"); // buttons for manual plucking

// the base pitch of the drone
f0 = hslider(
    "/h:main/[1]sa [style:knob]", 36, 24, 72, 1
) : sm : ba.midikey2hz;

// how long the strings decay
t60 = hslider(
    "/h:main/[2]decay_time [style:knob][unit:s]", 10, 0, 100, 0.1
) : sm;

// string brightness
damp = 1. - hslider("/h:main/[3]high_freq_loss [style:knob]", 0, 0, 1., 0.01)
    : sm;

// controls the detuning of parallel waveguides that mimics harmonic motion of
// the tambura
fd = hslider(
    "/h:main/[4]harmonic_motion [style:knob][scale:exp]",
    0.001, 0., 1, 0.0001
) : *(0.2) : sm;

// level of sympathetic coupling between strings
coupling = hslider(
    "/h:main/[5]sympathetic_coupling [style:knob]", 0.1, 0., 1., 0.0001
) : sm;

// creates the buzzing / jawari effect
jw = hslider(
    "/h:main/[6]jawari [style:knob]", 0, 0, 1, 0.001)
: *(0.1) : sm;

// stereo spread of strings
spread = hslider(
    "/h:main/[7]string_spread [style:knob]", 1., 0., 1., 0.01
) : sm;

tscale = hslider("/h:main/[8]tune_scale [style:knob]", 1, 0.9, 1.1, 0.001);
descale = hslider("/h:main/[9]decay_scale [style:knob]", 1, 0.1, 1., 0.001);
// dascale = hslider("/h:main/[10]damp_scale [style:knob]", 1, 0.5, 2, 0.01);

// crossfades between pink noise and DC excitation
ptype = hslider("/h:pick/[1]material [style:knob]", 0.13, 0.0, 1., 0.01) : sm;

// attack time of pluck envelope, 0 to 0.5 times f0 wavelength
pattack = hslider(
    "/h:pick/[2]attack_time [style:knob][scale:exp]", 0.07, 0, 0.5, 0.01
);

// decay time (1 to 10 times f0 wavelength)
ptime = hslider("/h:pick/[3]decay_time [style:knob]", 1., 1, 100., 0.01);

// pick position (ratio of f0 wavelength)
ppos = hslider("/h:pick/[4]position [style:knob]", 0.25, 0.01, 0.5, 0.01);

// pick bend depth in semitones
pbend = hslider(
    "/h:pick/[5]bend_depth [style:knob][unit:st]", 3, 0., 12., 0.01
);

// pick bend time (1 to 200 ms)
pbendtime = hslider(
    "/h:pick/[6]bend_time [style:knob][unit:ms]", 10., 1, 200., 1
);

// master volume
vol = hslider("volume [unit:dB]", 0, -36, +4, 0.1) : ba.db2linear : sm;

// s = string index
// c = comb filter index (of 9 comb filters in risset string)
tambura(NStrings) = (
    couplingmatrix(NStrings), par(s, NStrings, excitation(s))
    : ro.interleave(NStrings, 2)
    : par(s, NStrings, string(s, pluck(s)))
) // string itself with excitation + fbk as input
    ~ par(s, NStrings, !, _) // feedback only the right waveguide
    : par(s, NStrings, + : pan(s) // add left/right waveguides and pan
    ) :> _,_ // stereo output
with {
    couplingmatrix(NStrings) =
        par(s, NStrings, *(coupling) : couplingfilter) // coupling filters
        // unsel makes sure the feedback is disconnected
        <: par(s, NStrings, unsel(NStrings, s) :> _ )
    with {
        unsel(NStrings,s) = par(j, NStrings, U(s, j))
        with {
            U(s, s) = !;
            U(s, j) = _;
        };

        // couplingfilter = component("bridgeIR.dsp");
        // EQ to simulate bridge response
        couplingfilter = fi.highshelf(1,-100,5000)
            : fi.peak_eq(14, 2500, 400)
            : fi.peak_eq(20, 7500, 650);
    };

    // pan(s) = _ <: *(1-v), *(v)
    pan(s) = _ <: *((1-v) : sqrt), *((v) : sqrt)
    with {
      spreadScale = (1 / (NStrings-1));
      v = 0.5 + ((spreadScale * s) - 0.5) * spread;
    };

    // excitation(s) = _;
    excitation(s, trig) = input * ampenv : pickposfilter
    with {
        wl = (ma.SR/(f0 * ratios(s))); // wavelength of f0 in samples
        dur = (ptime * wl) / (ma.SR/1000.); // duration of the pluck in ms
        ampenv = trig * line(1. - trig, dur)
            : si.lag_ud(wl * pattack * (1/ma.SR), 0.005);
        amprand = abs(no.noise) : ba.latch(trig) *(0.25) + (0.75);
        posrand = abs(no.noise) : ba.latch(trig) *(0.2);
        // crossfade between DC and pink noise excitation source
        input = 1., no.pink_noise : si.interpolate(ptype);
        // simulation of different pluck positions
        pickposfilter = fi.ffcombfilter(dtmax, (ppos + posrand) * wl, -1);
    };

    // dual risset strings for decoupled feedback
    string(s, trig) = _, _ <: +, !,_
        : rissetstring(_, s, 1., 1., 1.),
            rissetstring(_, s, tscale, descale, 1.)
    with {
        // 9 detuned delay line resonators in parallel
        rissetstring(x, s, ts, des, das) =
            _ <: par(c, 9, stringloop(x, s, c, ts, das)) :> _
            : fi.dcblocker *(0.01);
        // waveguide string with damping filter and non linear apf for jawari
        // effect
        stringloop(x, s, c, ts, des, das) =
            (+ : delay) ~ ((dampingfilter : nlfm) * fbk)
        with {
            // allpass interpolation has better HF response
            // delay = de.fdelay1a(dtmax, dtsamples, x);

            // lagrange interpolation glitches less with pitch envelope
            delay = de.fdelaylti(2, dtmax, dtsamples, x);
            pitchenv = trig * line(1. - trig, pbendtime) <: * : *(pbend);
            thisf0 = ba.pianokey2hz(
                    ba.hz2pianokey((f0 * ratios(s)) + ((c-4) * fd) + pitchenv))
                * ts;
            dtsamples = (ma.SR/thisf0) - 2;
            fbk = pow(0.001, 1.0 / (thisf0 * (t60 * descale)));
            dampingfilter(x) = h0 * x' + h1*(x+x'')
            with {
                d = das * damp;
                h0 = (1. + d)/2;
                h1 = (1. - d)/4;
            };
            nlfm(x) = x <: fi.allpassnn(1, par(i, 1, jw * ma.PI * x));
        };
    };
};

// automatic plucking rate (Hz)
pluckrate = hslider(
    "/h:trigger/auto pluck rate [style:knob][unit:hz]", 0.1, 0.0, 0.5, 0.001
);
// enable automatic plucking
enableautoplucker = checkbox("/h:trigger/enable auto pluck");

autoplucker = phasor(pluckrate)
    <: <(0.25), >(0.25) & <(0.5), >(0.5) & <(0.75), >(0.75) & <(1)
    : par(s, NStrings, *(enableautoplucker))
with {
    phasor(freq) = (freq/float(ma.SR) : (+ : ma.decimal) ~ _);
};

// process = par(s, NStrings, pluck(s)) : tambura(NStrings) : *(vol), *(vol);

process =
    (par(s, NStrings, pluck(s)), autoplucker) :>
    tambura(NStrings) : *(vol), *(vol);
