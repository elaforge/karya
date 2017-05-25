// Copyright 2017 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#ifndef __FAUST_DRIVER_H
#define __FAUST_DRIVER_H

#include <faust/gui/UI.h>
#include <faust/gui/meta.h>
#include <faust/dsp/dsp.h>

// Used as a prototype to instantiate a dsp via the clone method.
typedef const dsp *Patch;
typedef dsp *Instrument;

extern "C" {

// Get all instruments and their names.  Return the count.
int faust_patches(const char ***names, Patch **patches);

// Get an array of null-terminated control strings.  This is the number of
// inputs.
int faust_metadata(Patch inst, const char ***keys, const char ***values);

// Initilaize a new instrument.
Instrument faust_initialize(Patch patch, int sample_rate);
void faust_destroy(Instrument instrument);

// Same as struct ControlSample, aka Storable (TimeVector Double).
struct Point { double x, y; };

// Pass a Point array for each control.  Interpolate controls from start_frame
// to end_frame, passing them to inst->compute, and writing output.
void faust_render(Instrument inst, int start_frame, int end_frame,
    Point **controls, float **output);

}

#endif
