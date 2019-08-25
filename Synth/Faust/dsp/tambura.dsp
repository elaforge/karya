declare name "Tambura";
declare description "Pseudo physical model of an Indian Tambura/Tanpura";
declare author "Oli Larkin (contact@olilarkin.co.uk)";
declare copyright "Oliver Larkin";
declare version "1.0";
declare licence "GPL";

declare flags "triggered";

import("stdfaust.lib");


dtmax = 4096;

// *** per-string

NStrings = 4;
pluck(i) = button("/h:%i/gate");
pitch(i) = hslider("/h:%i/pitch", 36, 1, 127, 1) : smooth : ba.midikey2hz;
pan(i) = hslider("/h:%i/pan", 0, -1, 1, 0.01) : smooth;

// how long the strings decay
t60 = hslider(
    "/h:_main/[2]decay_time [style:knob][unit:s]", 10, 0, 100, 0.1
) : smooth;

// string brightness
damp = 1 - hslider("/h:_main/[3]high_freq_loss [style:knob]", 0, 0, 1, 0.01)
    : smooth;

// controls the detuning of parallel waveguides that mimics harmonic motion of
// the tambura
hmotion = hslider(
    "/h:_main/[4]harmonic_motion [style:knob][scale:exp]",
    0.001, 0, 1, 0.0001
) : *(0.2) : smooth;

// creates the buzzing / jawari effect
jawari = hslider(
    "/h:_main/[6]jawari [style:knob]", 0, 0, 1, 0.001)
: *(0.1) : smooth;

// *** global

// level of sympathetic coupling between strings
coupling = hslider(
    "/h:_main/[5]sympathetic_coupling [style:knob]", 0.1, 0, 1, 0.0001
) : smooth;

tscale = hslider("/h:_main/[8]tune_scale [style:knob]", 1, 0.9, 1.1, 0.001);
descale = hslider("/h:_main/[9]decay_scale [style:knob]", 1, 0.1, 1, 0.001);
// dascale = hslider("/h:_main/[10]damp_scale [style:knob]", 1, 0.5, 2, 0.01);

// crossfades between pink noise and DC excitation
ptype = hslider("/h:_pick/[1]material [style:knob]", 0.13, 0.0, 1, 0.01)
    : smooth;

// attack time of pluck envelope, 0 to 0.5 times pitch wavelength
pattack = hslider(
    "/h:_pick/[2]attack_time [style:knob][scale:exp]", 0.07, 0, 0.5, 0.01
);

// decay time (1 to 10 times pitch wavelength)
ptime = hslider("/h:_pick/[3]pick_decay_time [style:knob]", 1, 1, 100, 0.01);

// pick position (ratio of pitch wavelength)
ppos = hslider("/h:_pick/[4]position [style:knob]", 0.25, 0.01, 0.5, 0.01);

// pick bend depth in semitones
pbend = hslider(
    "/h:_pick/[5]bend_depth [style:knob][unit:st]", 3, 0, 12, 0.01
);

// pick bend time (1 to 200 ms)
pbendtime = hslider(
    "/h:_pick/[6]bend_time [style:knob][unit:s]", 0.01, 0.001, 0.2, 0.001
);

smooth = si.smooth(ba.tau2pole(sec))
with {
    // Smooth time is equal to controlSize.  This should be synced with
    // fault-im.
    sec = 147 / 44100;
};

// *** implementation

// s = string index
// c = comb filter index (of 9 comb filters in risset string)
//
// Input: gate per string.
tambura(NStrings) = (
    couplingmatrix(NStrings), par(s, NStrings, excitation(s))
    : ro.interleave(NStrings, 2)
    : par(s, NStrings, string(s, pluck(s)))
) // string itself with excitation + fbk as input
    ~ par(s, NStrings, !, _) // feedback only the right waveguide
    : par(s, NStrings, + : setPan(s) // add left/right waveguides and pan
    ) :> _,_ // stereo output
with {
    couplingmatrix(NStrings) =
        par(s, NStrings, *(coupling) : couplingfilter) // coupling filters
        // unsel makes sure the feedback is disconnected
        <: par(s, NStrings, unsel(NStrings, s) :> _ )
    with {
        unsel(NStrings, s) = par(j, NStrings, U(s, j))
        with {
            U(s, s) = !;
            U(s, j) = _;
        };

        // couplingfilter = component("bridgeIR.dsp");
        // EQ to simulate bridge response
        couplingfilter = fi.highshelf(1, -100, 5000)
            : fi.peak_eq(14, 2500, 400)
            : fi.peak_eq(20, 7500, 650);
    };

    setPan(s) = _ <: *((1 - p) : sqrt), *(p : sqrt)
        with { p = (min(1, max(-1, pan(s))) + 1) / 2; };

    excitation(s, trig) = input * ampenv : pickposfilter
    with {
        wl = ma.SR / pitch(s); // wavelength of pitch(s) in samples

        dur = (ptime * wl) / (ma.SR / 1000); // duration of the pluck in ms
        ampenv = trigger(trig, dur)
            : si.lag_ud(wl * pattack * (1/ma.SR), 0.005);
        amprand = abs(no.noise) : ba.latch(trig) *(0.25) + 0.75;
        posrand = abs(no.noise) : ba.latch(trig) *(0.2);
        // crossfade between DC and pink noise excitation source
        input = 1, no.pink_noise : si.interpolate(ptype);
        // simulation of different pluck positions
        pickposfilter = fi.ffcombfilter(dtmax, (ppos + posrand) * wl, -1);
    };

    // dual risset strings for decoupled feedback
    string(s, trig) = _, _ <: +, !,_
        : rissetstring(_, s, 1, 1, 1),
          rissetstring(_, s, tscale, descale, 1)
    with {
        // 9 detuned delay line resonators in parallel
        rissetstring(x, s, tscale1, des, das) =
            _ <: par(c, 9, stringloop(x, s, c, tscale1, das)) :> _
            : fi.dcblocker *(0.01);
        // waveguide string with damping filter and non linear apf for jawari
        // effect
        stringloop(x, s, c, tscale1, des, das) =
            (+ : delay) ~ ((dampingfilter : nlfm) * fbk)
        with {
            // allpass interpolation has better HF response
            // delay = de.fdelay1a(dtmax, dtsamples, x);
            // lagrange interpolation glitches less with pitch envelope
            delay = de.fdelaylti(2, dtmax, dtsamples, x);
            // trig is scaled by dynamic, so this will also scale the pitch
            // bend depth, but that seems desirable.
            pitchenv = trigger(trig, pbendtime * 1000) <: * : *(pbend);
            this_pitch =
                ba.pianokey2hz(
                    ba.hz2pianokey(pitch(s) + (c-4) * hmotion)
                    + pitchenv
                ) * tscale1;
            dtsamples = ma.SR / this_pitch - 2;
            fbk = pow(0.001, 1 / (this_pitch * (t60 * descale)));
            dampingfilter(x) = h0 * x' + h1*(x+x'')
            with {
                d = das * damp;
                h0 = (1 + d) / 2;
                h1 = (1 - d) / 4;
            };
            nlfm(x) = x <: fi.allpassnn(1, par(i, 1, jawari * ma.PI * x));
        };
    };
};

// On gate's rising edge, make a linear slope from the gate value to 0 over the
// given time in seconds.
trigger(gate, time) = state ~ (_, _, _) : !, !, _
with {
    // count down so t/samples -> 0/samples
    state(t, val, _out) = nt, nval, val * (nt / samples)
    with {
        nt = ba.if(gate > gate', samples, max(0, t-1));
        nval = ba.if(gate > gate', gate, val);
    };
    // Avoid a time of 0, which will cause NaNs.
    samples = max(1, time * ma.SR / 1000.0);
};
/*
trigger time = scanl state (0, (0, 0, 0))
    where
    state (gate', (t, val, _out)) gate = (gate, (nt, nval, nval * (nt / time)))
        where
        nt = if gate > gate' then time else max 0 (t-1)
        nval = if gate > gate' then gate else val
*/

// automatic plucking rate (Hz)
pluckrate = hslider(
    "/h:_trigger/_auto pluck rate [style:knob][unit:hz]", 0.1, 0.0, 0.5, 0.001
);
// enable automatic plucking
enableautoplucker = checkbox("/h:_trigger/_enable auto pluck");

autoplucker = phasor(pluckrate)
    <: <(0.25), >(0.25) & <(0.5), >(0.5) & <(0.75), >(0.75) & <(1)
    : par(s, NStrings, *(enableautoplucker))
with {
    phasor(freq) = (freq/float(ma.SR) : (+ : ma.decimal) ~ _);
};

process = (par(s, NStrings, pluck(s)), autoplucker) :> tambura(NStrings);
