// Copyright 2023 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

// Same as sine.dsp, but using GUI controls instead of audio signals.

import("stdfaust.lib");

declare description "Sine wave using GUI UI.";

dyn = hslider("dyn[unit: dB]", 0.75, 0, 1, 0.01);
gate = button("gate");
pitch = hslider("pitch[unit: nn]", 440, 1, 1000, 0.01);

// TODO: this should be Control.minimumDb, let's use CPP to inject it
minimumDb = -96;
smooth = si.smooth(ba.tau2pole(0.005));

process =
    noteon * ba.db2linear(db) : smooth * os.osc(ba.midikey2hz(pitch) : smooth)
with {
    db = dyn * -minimumDb + minimumDb;
    // This is necessary to get down to 0.
    noteon = gate > 0 & dyn > 0;
};
