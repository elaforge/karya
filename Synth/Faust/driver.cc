// Copyright 2017 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <stdio.h>
#include <vector>
#include <utility>

#include "driver.h"

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
faust_controls(const Patch *patch, const char ***out_controls, char ***out_docs,
    FAUSTFLOAT ***out_vals)
{
    std::vector<UIGlue::Widget> widgets(patch->getUiMetadata());
    int size = widgets.size();
    const char **controls = (const char **) calloc(size, sizeof(char *));
    char **docs = (char **) calloc(size, sizeof(char *));
    FAUSTFLOAT **vals = (FAUSTFLOAT **) calloc(size, sizeof(FAUSTFLOAT *));

    for (int i = 0; i < size; i++) {
        const UIGlue::Widget &w = widgets[i];
        if (w.boolean)
            asprintf(docs + i, "%s", "boolean");
        else
            asprintf(docs + i, "init:%.3g, %.3g -- %.3g", w.init, w.min, w.max);
        controls[i] = w.label;
        vals[i] = w.value;
    }
    *out_controls = controls;
    *out_docs = docs;
    *out_vals = vals;
    return size;
}

Patch *
faust_initialize(const Patch *patch, int srate)
{
    return patch->allocate(srate);
}

void
faust_render(Patch *patch, int frames, const float **controls, float **outputs)
{
    patch->compute(frames, controls, outputs);
}

}
