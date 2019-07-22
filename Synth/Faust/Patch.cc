// Copyright 2017 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <faust/gui/CInterface.h>
#include <faust/gui/UI.h>

#include "Patch.h"


static void
addPair(void *list, const char *key, const char *value) {
    static_cast<Patch::Pairs *>(list)->push_back(std::make_pair(key, value));
}

Patch::Pairs
Patch::getMetadata() const
{
    Patch::Pairs pairs;
    MetaGlue glue;
    glue.metaInterface = static_cast<void *>(&pairs);
    glue.declare = addPair;
    this->metadata(&glue);
    return pairs;
}


class StoreUi : public UI {
public:
    std::vector<Patch::Widget> widgets;

    virtual void openTabBox(const char *label) override {}
    virtual void openHorizontalBox(const char *label) override {}
    virtual void openVerticalBox(const char *label) override {}
    virtual void closeBox() override {}
    virtual void addSoundfile(
        const char *label, const char *filename, Soundfile **sf_zone) override
    {}

    // -- active widgets

    virtual void addButton(const char *label, FAUSTFLOAT *zone) override {
        widgets.push_back(Patch::Widget(label, zone, true));
    }
    virtual void addCheckButton(const char *label, FAUSTFLOAT *zone) override {
        widgets.push_back(Patch::Widget(label, zone, true));
    }
    virtual void addVerticalSlider(const char *label, FAUSTFLOAT *zone,
            FAUSTFLOAT init, FAUSTFLOAT min, FAUSTFLOAT max, FAUSTFLOAT step)
        override
    {
        widgets.push_back(
            Patch::Widget(label, zone, false, init, min, max, step));
    }
    virtual void addHorizontalSlider(const char *label, FAUSTFLOAT *zone,
            FAUSTFLOAT init, FAUSTFLOAT min, FAUSTFLOAT max, FAUSTFLOAT step)
        override
    {
        widgets.push_back(
            Patch::Widget(label, zone, false, init, min, max, step));
    }
    virtual void addNumEntry(const char *label, FAUSTFLOAT *zone,
            FAUSTFLOAT init, FAUSTFLOAT min, FAUSTFLOAT max, FAUSTFLOAT step)
        override
    {
        widgets.push_back(
            Patch::Widget(label, zone, false, init, min, max, step));
    }

    // -- passive widgets

    virtual void addHorizontalBargraph(const char *label, FAUSTFLOAT *zone,
        FAUSTFLOAT min, FAUSTFLOAT max) override {}
    virtual void addVerticalBargraph(const char *label, FAUSTFLOAT *zone,
        FAUSTFLOAT min, FAUSTFLOAT max) override {}
};

std::vector<Patch::Widget>
Patch::getUiMetadata() const
{
    StoreUi ui;

    // UIGlue glue;
    // glue.uiInterface = &ui;
    // TODO this is implemented as a struct with a zillion function pointers,
    // which is not only awkward and annoying, but loses type safety when they
    // decide to add a new function, which they definitely do.  All this hassle
    // is not worth it for the GUI signal mechanism, which I don't even like
    // or want to use.
    // glue.openTabBoxFun = StoreUi::openTabBox;
    // this->uiMetadata(&glue);
    return ui.widgets;
}
