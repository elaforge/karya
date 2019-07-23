// Copyright 2017 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

#include <memory>
#include <stdlib.h>
#include <string.h>
#include <utility>
#include <vector>

#include <faust/dsp/dsp.h> // just for FAUSTFLOAT
#include "fltk/util.h" // TODO for ASSERT


// Normally MetaGlue and UIGlue come from <faust/gui/CInterface.h>, but they're
// declared as structs, which means I need to manually fill in the fields and
// if I forget one or they add one, which is common, it's segfault time.  So
// I'll define my own, and not include CInterface.h.
struct MetaGlue {
    typedef std::vector<std::pair<const char *, const char *>> Pairs;
    Pairs pairs;
    void *metaInterface = nullptr;
    void declare(void *, const char *key, const char *value) {
        pairs.push_back(std::make_pair(key, value));
    }
};


struct UIGlue {
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
    std::vector<Widget> widgets;
    void *uiInterface = nullptr;
    typedef void Soundfile;

    void declare(void *, FAUSTFLOAT *zone, const char *key, const char *value)
    {}
    void openTabBox(void *, const char *label) {}
    void openHorizontalBox(void *, const char *label) {}
    void openVerticalBox(void *, const char *label) {}
    void closeBox(void *) {}
    void addSoundfile(
        void *, const char *label, const char *filename, Soundfile **sf_zone)
    {}

    // -- active widgets

    void addButton(void *, const char *label, FAUSTFLOAT *zone) {
        widgets.push_back(Widget(label, zone, true));
    }
    void addCheckButton(void *, const char *label, FAUSTFLOAT *zone) {
        widgets.push_back(Widget(label, zone, true));
    }
    void addVerticalSlider(void *, const char *label, FAUSTFLOAT *zone,
        FAUSTFLOAT init, FAUSTFLOAT min, FAUSTFLOAT max, FAUSTFLOAT step)
    {
        widgets.push_back(Widget(label, zone, false, init, min, max, step));
    }
    void addHorizontalSlider(
        void *, const char *label, FAUSTFLOAT *zone,
        FAUSTFLOAT init, FAUSTFLOAT min, FAUSTFLOAT max, FAUSTFLOAT step)
    {
        widgets.push_back(Widget(label, zone, false, init, min, max, step));
    }
    void addNumEntry(
        void *, const char *label, FAUSTFLOAT *zone,
        FAUSTFLOAT init, FAUSTFLOAT min, FAUSTFLOAT max, FAUSTFLOAT step)
    {
        widgets.push_back(Widget(label, zone, false, init, min, max, step));
    }

    // -- passive widgets

    void addHorizontalBargraph(void *, const char *label, FAUSTFLOAT *zone,
        FAUSTFLOAT min, FAUSTFLOAT max) {}
    void addVerticalBargraph(void *, const char *label, FAUSTFLOAT *zone,
        FAUSTFLOAT min, FAUSTFLOAT max) {}
};

// A wrapper around faust's generated dsp class.
//
// This is so I can implement getState and putState as directly serializing the
// dsp state.  Since the faust C++ output has virtual methods, I think I can't
// directly serialize it without some scary hacks to avoid overwriting the
// vtable.  So I use the C output, which provides the entire state as a plain
// struct, and then wrap it back up as an object.  This is a bit awkward, but
// it lets me fix some other problems with faust's generated C++, namely that
// you have to instantiate the class and allocate all its memeory just to get
// number of inputs and outputs and metadata.
class Patch {
public:
    // An opaque pointer to the internal state.  The actual struct type varies
    // per patch, but all I do here is treat it like a sized block of memory.
    // I allocate it with calloc(), which should satisfy any alignment
    // requirement.
    struct State;
    typedef void (*Initialize)(State *dsp, int samplingFreq);
    typedef void (*Metadata)(MetaGlue *m);
    typedef void (*UiMetadata)(State *, UIGlue *);
    // This correspands to the faust C backend generated compute__faust_$name
    // function.
    typedef void (*Compute)(
        State *state, int, const float **inputs, float **outputs);

    Patch(const char *name, size_t size, int inputs, int outputs,
            Initialize initialize, Metadata metadata, UiMetadata uiMetadata,
            Compute compute_) :
        name(name), size(size), inputs(inputs), outputs(outputs),
        state(nullptr),
        metadata(metadata), uiMetadata(uiMetadata), initialize(initialize),
        compute_(compute_)
    {}
    ~Patch() {
        if (state)
            free(state);
    }

    Patch *allocate(int srate) const {
        Patch *p = new Patch(
            name, size, inputs, outputs, initialize, metadata, uiMetadata,
            compute_);
        p->state = static_cast<State *>(calloc(1, size));
        ASSERT(p->state != nullptr);
        p->initialize(p->state, srate);
        return p;
    }

    MetaGlue::Pairs getMetadata() const {
        MetaGlue glue;
        this->metadata(&glue);
        return glue.pairs;
    };

    std::vector<UIGlue::Widget> getUiMetadata() const {
        UIGlue ui;
        this->uiMetadata(state, &ui);
        return ui.widgets;
    }

    size_t getState(const State **p) const {
        *p = state;
        return size;
    }
    void putState(const State *p) {
        memcpy(state, p, size);
    }

    void compute(int count, const float **inputs, float **outputs) {
        ASSERT(state != nullptr);
        compute_(state, count, inputs, outputs);
    }

    const char *name;
    const size_t size;
    const int inputs, outputs;
private:
    State *state;
    Metadata metadata;
    UiMetadata uiMetadata;
    Initialize initialize;
    Compute compute_;
};
