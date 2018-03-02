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

// render //////////////////////////////

static RealTime
frame_to_second(int srate, int frame)
{
    return double(frame) / double(srate);
}

// TODO copied from fltk/EventTrack.cc.  Make a TimeVector class with things
// like this.

static bool
compare_control_sample(const ControlSample &s1, const ControlSample &s2)
{
    return s1.time < s2.time;
}

static int
find_sample(const ControlSample *samples, int length, RealTime start)
{
    if (!samples)
        return 0;
    ControlSample sample(start, 0);
    const ControlSample *found = std::lower_bound(
        samples, samples + length, sample, compare_control_sample);
    // Back up one to make sure I have the sample before start.
    if (found > samples)
        found--;
    return found - samples;
}

static FAUSTFLOAT
interpolate(const ControlSample *samples, int length, int index, RealTime time)
{
    if (index >= length)
        return 0;
    const ControlSample &prev = samples[index];
    // As a special case, the signal is 0 before the first sample.
    if (time < prev.time)
        return 0;
    if (index + 1 >= length)
        return prev.val;
    const ControlSample &next = samples[index + 1];
    return (next.val - prev.val) / (next.time - prev.time)
        * (time - prev.time) + prev.val;
}

static FAUSTFLOAT *
interpolate_control(int srate, const ControlSample *samples, int length,
    int start_frame, int end_frame)
{
    // TODO these should also always be the same size, so I can allocate once.
    // Maybe stash them in Instrument.
    FAUSTFLOAT *out =
        (FAUSTFLOAT *) calloc(end_frame - start_frame, sizeof(FAUSTFLOAT));

    RealTime start = frame_to_second(srate, start_frame);
    int index = find_sample(samples, length, start);
    for (int frame = start_frame; frame < end_frame; frame++) {
        RealTime time = frame_to_second(srate, frame);
        while (index+1 < length && samples[index+1].time <= time)
            index++;
        out[frame - start_frame] = interpolate(samples, length, index, time);
    }
    return out;
}


// For debugging.
static void
print_control(const ControlSample *control, int len)
{
    for (int i = 0; i < len; i++) {
        DEBUG(control[i].time << ", " << control[i].val);
    }
}


void
faust_render(Instrument inst, int start_frame, int end_frame,
    const ControlSample **controls, const int *control_lengths, float **output)
{
    ASSERT(end_frame >= start_frame);

    int ncontrols = inst->getNumInputs();
    FAUSTFLOAT **input =
        (FAUSTFLOAT **) calloc(ncontrols, sizeof(FAUSTFLOAT *));
    for (int i = 0; i < ncontrols; i++) {
        // DEBUG("control " << i);
        // print_control(controls[i], control_lengths[i]);
        input[i] = interpolate_control(
            inst->getSampleRate(), controls[i], control_lengths[i],
            start_frame, end_frame);
    }
    inst->compute(end_frame - start_frame, input, output);
    for (int i = 0; i < ncontrols; i++) {
        free(input[i]);
    }
    free(input);
}

}
