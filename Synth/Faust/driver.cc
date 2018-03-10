// Copyright 2017 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <stdio.h>
#include <vector>
#include <utility>

#include "driver.h"
#include <faust/dsp/dsp.h>

#include "build/faust_all.cc"

#include "fltk/util.h"


extern "C" {

int
faust_patches(const char ***names, Patch **patches)
{
    *names = all_names;
    *patches = all_dsps;
    return all_count;
}

class StoreMeta : public Meta {
public:
    std::vector<std::pair<const char *, const char *>> variables;
    virtual void declare(const char *key, const char *value) override {
        // For now I only want controls and description.
        if (strncmp(key, "control", 7) == 0 || strcmp(key, "description") == 0)
            variables.push_back(std::make_pair(key, value));
    }
};

int
faust_metadata(Patch patch, const char ***keys, const char ***values)
{
    StoreMeta store;
    const_cast<dsp *>(patch)->metadata(&store);

    int size = store.variables.size();
    *keys = (const char **) calloc(size, sizeof(char *));
    *values = (const char **) calloc(size, sizeof(char *));
    int i = 0;
    for (auto const &entry : store.variables) {
        (*keys)[i] = entry.first;
        (*values)[i] = entry.second;
        i++;
    }
    return size;
}

struct Widget {
    Widget(const char *label, FAUSTFLOAT *value, bool boolean,
            FAUSTFLOAT init = 0, FAUSTFLOAT min = 0, FAUSTFLOAT max = 0,
            FAUSTFLOAT step = 0)
        : label(label), value(value), boolean(boolean), init(init),
            min(min), max(max), step(step)
        {}
    const char *label;
    FAUSTFLOAT *value;
    bool boolean;
    FAUSTFLOAT init, min, max, step;
};

class StoreUi : public UI {
public:
    std::vector<Widget> widgets;

    virtual void openTabBox(const char *label) {}
    virtual void openHorizontalBox(const char *label) {}
    virtual void openVerticalBox(const char *label) {}
    virtual void closeBox() {}

    // -- active widgets

    virtual void addButton(const char *label, FAUSTFLOAT *zone) {
        widgets.push_back(Widget(label, zone, true));
    }
    virtual void addCheckButton(const char *label, FAUSTFLOAT *zone) {
        widgets.push_back(Widget(label, zone, true));
    }
    virtual void addVerticalSlider(const char *label, FAUSTFLOAT *zone,
            FAUSTFLOAT init, FAUSTFLOAT min, FAUSTFLOAT max, FAUSTFLOAT step) {
        widgets.push_back(Widget(label, zone, false, init, min, max, step));
    }
    virtual void addHorizontalSlider(const char *label, FAUSTFLOAT *zone,
            FAUSTFLOAT init, FAUSTFLOAT min, FAUSTFLOAT max, FAUSTFLOAT step) {
        widgets.push_back(Widget(label, zone, false, init, min, max, step));
    }
    virtual void addNumEntry(const char *label, FAUSTFLOAT *zone,
            FAUSTFLOAT init, FAUSTFLOAT min, FAUSTFLOAT max, FAUSTFLOAT step) {
        widgets.push_back(Widget(label, zone, false, init, min, max, step));
    }

    // -- passive widgets

    virtual void addHorizontalBargraph(const char *label, FAUSTFLOAT *zone,
        FAUSTFLOAT min, FAUSTFLOAT max) {}
    virtual void addVerticalBargraph(const char *label, FAUSTFLOAT *zone,
        FAUSTFLOAT min, FAUSTFLOAT max) {}
};

int
faust_controls(Patch patch, const char ***out_controls, char ***out_docs,
    FAUSTFLOAT ***out_vals)
{
    StoreUi ui;
    const_cast<dsp *>(patch)->buildUserInterface(&ui);
    int size = ui.widgets.size();
    const char **controls = (const char **) calloc(size, sizeof(char *));
    char **docs = (char **) calloc(size, sizeof(char *));
    FAUSTFLOAT **vals = (FAUSTFLOAT **) calloc(size, sizeof(FAUSTFLOAT *));

    for (int i = 0; i < size; i++) {
        const Widget &w = ui.widgets[i];
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

Instrument
faust_initialize(Patch patch, int sample_rate)
{
    // The clone() method should be const, but it looks like the faust
    // authors didn't know about const.
    Instrument instrument = const_cast<dsp *>(patch)->clone();
    instrument->init(sample_rate);
    return instrument;
}

void
faust_destroy(Instrument instrument)
{
    delete instrument;
}

int
faust_num_inputs(Patch patch)
{
    return const_cast<dsp *>(patch)->getNumInputs();
}

int
faust_num_outputs(Patch patch)
{
    return const_cast<dsp *>(patch)->getNumOutputs();
}

void
faust_render(Instrument inst, int frames, const float **controls,
    float **outputs)
{
    // TODO input is treated as const, I should fix faust's generated c++.
    inst->compute(frames, const_cast<float **>(controls), outputs);
}

}
