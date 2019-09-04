declare name "blowBottle";
declare description "Blown Bottle Instrument";
declare author "Romain Michon (rmichon@ccrma.stanford.edu)";
declare copyright "Romain Michon";
declare version "1.0";
declare licence "STK-4.3"; // Synthesis Tool Kit 4.3 (MIT style license);
declare description "This object implements a helmholtz resonator (biquad
    filter) with a polynomial jet excitation (a la Cook).";

import("stdfaust.lib");
inst = library("instruments.lib");

//==================== GUI SPECIFICATION ================

freq = nentry(
    "h:_Basic/freq [1][unit:Hz] [tooltip:Tone frequency]",
    440, 20, 20000,1);
gain = nentry(
    "h:_Basic/gain [1][tooltip:Gain (value between 0 and 1)]",
    1, 0, 1, 0.01);
gate = button("h:_Basic/gate [1][tooltip:noteOn = 1, noteOff = 0]");

noiseGain = hslider(
    "h:_Physical_and_Nonlinearity/v:_Physical/noise_gain
        [2][tooltip:Breath noise gain (value between 0 and 1)]",
    0.5, 0, 1, 0.01)*2;

pressure = hslider(
    "h:_Physical_and_Nonlinearity/v:_Physical/pressure
        [2][tooltip:Breath pressure (value bewteen 0 and 1)]", 1, 0, 1, 0.01);

typeModulation = nentry(
    "h:_Physical_and_Nonlinearity/v:_Nonlinear_Filter/modulation_type
        [3][tooltip: 0=theta is modulated by the incoming signal;
        1=theta is modulated by the averaged incoming signal;
        2=theta is modulated by the squared incoming signal;
        3=theta is modulated by a sine wave of frequency freqMod;
        4=theta is modulated by a sine wave of frequency freq;]",
    0, 0, 4, 1);
nonLinearity = hslider(
    "h:_Physical_and_Nonlinearity/v:_Nonlinear_Filter/nonlinearity
        [3][tooltip:Nonlinearity factor (value between 0 and 1)]",
    0, 0, 1, 0.01);

frequencyMod = hslider(
    "h:_Physical_and_Nonlinearity/v:_Nonlinear_Filter/modulation_frequency
    [3][unit:Hz][tooltip:Frequency of the sine wave for the modulation of
    theta (works if Modulation Type=3)]", 220, 20, 1000, 0.1);
nonLinAttack = hslider(
    "h:_Physical_and_Nonlinearity/v:_Nonlinear_Filter/nonlinearity_attack
        [3][unit:s][Attack duration of the nonlinearity]",
    0.1, 0, 2, 0.01);

envelopeAttack = hslider(
    "h:_Envelopes_and_Vibrato/v:_Envelope/envelope_attack
        [5][unit:s][tooltip:Envelope attack duration]",
    0.01, 0, 2, 0.01);
envelopeDecay = hslider(
    "h:_Envelopes_and_Vibrato/v:_Envelope/envelope_decay
        [5][unit:s][tooltip:Envelope decay duration]",
    0.01, 0, 2, 0.01);
envelopeRelease = hslider(
    "h:_Envelopes_and_Vibrato/v:_Envelope/envelope_release
        [5][unit:s][tooltip:Envelope release duration]",
    0.5, 0, 2, 0.01);


//==================== SIGNAL PROCESSING ================

// Nonlinear filter
//nonlinearities are created by the nonlinear passive allpass ladder filter
//declared in miscfilter.lib

//nonlinear filter order
nlfOrder = 6;

//attack - sustain - release envelope for nonlinearity (declared in
//instruments.lib)
envelopeMod = inst.en.asr(nonLinAttack, 1, envelopeRelease, gate);

//nonLinearModultor is declared in instruments.lib, it adapts allpassnn from
//miscfilter.lib for using it with waveguide instruments
nlfm =  inst.nonLinearModulator((nonLinearity : si.smoo), envelopeMod, freq,
    typeModulation, (frequencyMod : si.smoo), nlfOrder);

// Synthesis parameters computing and functions declaration

//botlle radius
bottleRadius = 0.999;

stereo = stereoizer(ma.SR / freq);

bandPassFilter = inst.bandPass(freq, bottleRadius);

// Algorithm implementation

//global envelope is of type attack - decay - sustain - release
envelopeG =  gain * inst.en.adsr(
    gain * envelopeAttack, envelopeDecay, 1, envelopeRelease, gate);

//pressure envelope is also ADSR
envelope = pressure * inst.en.adsr(gain*0.02, 0.01, 1, gain * 0.2, gate);

// breath pressure
breathPressure = envelope;

//breath noise
randPressure = noiseGain * no.noise * breathPressure;

process =
    // differential pressure
    (-(breathPressure)
        <: ((+(1)) * randPressure : +(breathPressure)) - *(inst.jetTable), _
        : bandPassFilter,_) ~ nlfm : !, _
    //signal scaling
    : fi.dcblocker * envelopeG * 0.5 : stereo; //  : inst.instrReverb;

// stereoizer is declared in instruments.lib and implement a stereo
// spatialisation in function of the frequency period in number of samples
stereoizer(periodDuration) = _ <: _,widthdelay : stereopanner
with {
    W = hslider("v:_Spat/spatial-width", 0.5, 0, 1, 0.01);
    A = hslider("v:_Spat/pan-angle", 0.6, 0, 1, 0.01);
    widthdelay = de.delay(4096,W*periodDuration/2);
    stereopanner = _,_ : *(1.0-A), *(A);
};
