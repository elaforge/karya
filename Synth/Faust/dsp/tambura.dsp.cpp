declare name "Tambura";
declare description "Pseudo physical model of an Indian Tambura/Tanpura";
declare author "Oli Larkin (contact@olilarkin.co.uk)";
declare copyright "Oliver Larkin";
declare version "1.0";
declare licence "GPL";

declare flags "triggered";

import("stdfaust.lib");

// Smooth time is equal to controlSize.  This should be synced with faust-im.
controlSize = 147;
// Max size of delay lines.  Determines the lowest pitch.
maxDelay = 4096;
// Number of strings to simulate.
stringCount = STRING_COUNT;

// *** per-string

gate(i) = button("/h:%i/gate");
pitch(i) = hslider("/h:%i/pitch", 36, 1, 127, 1)
    : polySmooth(i) : ba.midikey2hz;
pan(i) = hslider("/h:%i/pan [style:knob]", 0, -1, 1, 0.01) : smooth;

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

smooth = si.smooth(ba.tau2pole(controlSize / ma.SR));
polySmooth(i) = si.polySmooth(gate(i), ba.tau2pole(controlSize / ma.SR), 147);

// *** implementation

// s = string index
// c = comb filter index (of 9 comb filters in risset string)
//
// Input: gate per string.
tambura(stringCount) = (
    coupling_matrix(stringCount), par(s, stringCount, excitation(s))
    : ro.interleave(stringCount, 2)
    : par(s, stringCount, string(s, gate(s)))
) // string itself with excitation + fbk as input
    ~ par(s, stringCount, !, _) // feedback only the right waveguide
    : par(s, stringCount, + : setPan(s) // add left/right waveguides and pan
    ) :> _, _ // stereo output
with {
    coupling_matrix(stringCount) =
        par(s, stringCount, *(coupling) : coupling_filter) // coupling filters
        // unsel makes sure the feedback is disconnected
        <: par(s, stringCount, unsel(stringCount, s) :> _)
    with {
        unsel(stringCount, s) = par(j, stringCount, broadcast(s, j))
        with {
            broadcast(s, s) = !;
            broadcast(s, j) = _;
        };

        // coupling_filter = component("bridgeIR.dsp");
        // EQ to simulate bridge response
        coupling_filter = fi.highshelf(1, -100, 5000)
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
        pickposfilter = fi.ffcombfilter(maxDelay, (ppos + posrand) * wl, -1);
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
            // delay = de.fdelay1a(maxDelay, dtsamples, x);
            // lagrange interpolation glitches less with pitch envelope
            delay = de.fdelaylti(2, maxDelay, dtsamples, x);
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

#if STRING_COUNT == 4

autoplucker = phasor(pluckrate)
    <: <(0.25), >(0.25) & <(0.5), >(0.5) & <(0.75), >(0.75) & <(1)
    : par(s, stringCount, *(enableautoplucker))
with {
    phasor(freq) = (freq/float(ma.SR) : (+ : ma.decimal) ~ _);
    // automatic plucking rate (Hz)
    pluckrate = hslider(
        "/h:_trigger/_auto pluck rate [style:knob][unit:hz]",
        0.1, 0.0, 0.5, 0.001
    );
    // enable automatic plucking
    enableautoplucker = checkbox("/h:_trigger/_enable auto pluck");
};

#else
// It's hard-coded to 4 strings.
autoplucker = par(s, stringCount, 0);
#endif

process = (par(s, stringCount, gate(s)), autoplucker) :> tambura(stringCount);
