// Copyright 2017 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

#include <faust/gui/UI.h>
#include <faust/gui/meta.h>
#include <faust/dsp/dsp.h>

#include "Patch.h"
#include "fltk/TimeVector.h" // for ControlSample

extern "C" {

// Functions that take (const Patch *) are suitable for both unallocated
// Patch prototypes from all_patches, and allocated ones from Patch::allocate.
// Ones that take (Patch *) require allocated state.

// unallocated Patch

// Get all instruments and their names.  Return the count.
int faust_patches(const Patch ***patches);

const char *faust_name(const Patch *patch) { return patch->name; }
int faust_num_inputs(const Patch *patch) { return patch->inputs; }
int faust_num_outputs(const Patch *patch) { return patch->outputs; }
size_t faust_get_state_size(const Patch *patch) { return patch->size; }

// Get an array of null-terminated control strings.  This is the number of
// inputs.
//
// The arrays are allocated, so the caller must free them.  The strings
// themselves are static.
int faust_metadata(
    const Patch *patch, const char ***keys, const char ***values);

// Get UI controls, with docs, and pointers to their values for modification.
//
// The arrays are allocated.  Control strings are static, but docs are also
// allocated.
int faust_controls(const Patch *patch, const char ***out_controls,
    char ***out_docs, FAUSTFLOAT ***out_vals);

// allocated Patch

// Initilaize a new instrument.
Patch *faust_initialize(const Patch *patch, int srate);
void faust_destroy(Patch *patch) { delete patch; }

void faust_render(
    Patch *patch, int frames, const float **controls, float **outputs);

size_t faust_get_state(const Patch *patch, const char **state) {
    return patch->getState((const Patch::State **) state);
}

// Caller should assert the state size matches patch->size.
void faust_put_state(Patch *patch, const char *state) {
    patch->putState((const Patch::State *) state);
}

}
