// Copyright 2017 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

#include <faust/dsp/dsp.h> // for FAUSTFLOAT

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
int faust_controls(const Patch *patch, const char ****out_paths,
    const char ***out_controls, char ***out_docs);
int faust_control_ptrs(Patch *inst, FAUSTFLOAT ***out_vals);

// allocated Patch

// This is the way to convert a const Patch to a non-const Patch with internal
// state.
Patch *faust_allocate(const Patch *patch, int srate);
void faust_destroy(Patch *inst);

// Render control_size * controls_per_block frames.  'control_ptrs' and
// 'controls' should be 'control_count' long, and each array in controls should
// be controls_per_block long.
void faust_render(
    Patch *inst,
    int control_size, int controls_per_block,
    int control_count, float **control_ptrs, const float **controls,
    const float **inputs, float **outputs);

size_t faust_get_state(const Patch *patch, const char **state) {
    return patch->getState((const Patch::State **) state);
}

// Caller should assert the state size matches patch->size.
void faust_put_state(Patch *inst, const char *state) {
    inst->putState((const Patch::State *) state);
}

}
