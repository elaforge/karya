// Copyright 2023 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

// Example hello world faust instrument.
//
// This uses audio input controls, which seem to be better than the control
// ones, but less well supported by faust, they have no built-in declarations
// to attach metadata so I have to do a hack with declare.

import("stdfaust.lib");

declare description "Simple sine wave.";
declare control0_gate "Gate signal.";
declare control1_pitch "Pitch signal.";
declare control2_dyn "Dynamic signal.";

// TODO: this should be Control.minimumDb, let's use CPP to inject it
minimumDb = -96;
smooth = si.smooth(ba.tau2pole(0.005));

process(gate, pitch, dyn) =
    noteon * ba.db2linear(db) : smooth * os.osc(ba.midikey2hz(pitch) : smooth)
with {
    db = dyn * -minimumDb + minimumDb;
    // This is necessary to get down to 0.
    noteon = gate > 0 & dyn > 0;
};
