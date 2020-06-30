// Copyright 2017 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <stdio.h>
#include <vector>
#include <utility>

#include "patch_c.h"

#include "build/faust_all.cc"

#include "fltk/util.h"


extern "C" {

int
faust_patches(const Patch ***patches)
{
    // These variables come from the faust_all.cc include above.
    *patches = all_patches;
    return all_patches_count;
}

int
faust_metadata(const Patch *patch, const char ***keys, const char ***values)
{
    std::vector<std::pair<const char *, const char *>> pairs =
        patch->getMetadata();

    int size = pairs.size();
    *keys = (const char **) calloc(size, sizeof(char *));
    *values = (const char **) calloc(size, sizeof(char *));
    int i = 0;
    for (auto const &entry : pairs) {
        (*keys)[i] = entry.first;
        (*values)[i] = entry.second;
        i++;
    }
    return size;
}

int
faust_controls(const Patch *patch, const char ****out_paths,
    const char ***out_controls, char ***out_docs)
{
    std::vector<UIGlue::Widget> widgets(patch->getUiMetadata());
    int size = widgets.size();
    const char ***paths = (const char ***) calloc(size, sizeof(char *));
    const char **controls = (const char **) calloc(size, sizeof(char *));
    char **docs = (char **) calloc(size, sizeof(char *));

    for (int i = 0; i < size; i++) {
        const UIGlue::Widget &w = widgets[i];
        if (w.boolean)
            asprintf(docs + i, "%s", "boolean");
        else
            asprintf(docs + i, "init:%.3g, %.3g -- %.3g", w.init, w.min, w.max);
        paths[i] = (const char **) calloc(w.path.size() + 1, sizeof(char *));
        for (int j = 0; j < w.path.size(); j++)
            paths[i][j] = w.path[j];
        controls[i] = w.label;
    }
    *out_paths = paths;
    *out_controls = controls;
    *out_docs = docs;
    return size;
}

int
faust_control_ptrs(Patch *inst, FAUSTFLOAT ***out_vals)
{
    std::vector<UIGlue::Widget> widgets(inst->getUiMetadata());
    int size = widgets.size();
    FAUSTFLOAT **vals = (FAUSTFLOAT **) calloc(size, sizeof(FAUSTFLOAT *));
    for (int i = 0; i < size; i++) {
        vals[i] = widgets[i].value;
    }
    *out_vals = vals;
    return size;
}

Patch *
faust_allocate(const Patch *patch, int srate)
{
    return patch->allocate(srate);
}

void
faust_render(
    Patch *inst,
    int control_size, int controls_per_block,
    int control_count, float **control_ptrs, const float **controls,
    const float **inputs, float **outputs)
{
    for (int block = 0; block < controls_per_block; block++) {
        for (int i = 0; i < control_count; i++) {
            *control_ptrs[i] = controls[i][block];
        }
        inst->compute(control_size, inputs, outputs);
        for (int i = 0; i < inst->inputs; i++)
            inputs[i] += control_size;
        for (int i = 0; i < inst->outputs; i++)
            outputs[i] += control_size;
    }
}

}
