// Copyright 2017 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <vector>
#include <utility>

#include "driver.h"
#include <faust/dsp/dsp.h>

#include "build/faust_all.cc"


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

void
faust_render(Instrument inst, int start_frame, int end_frame, Point **controls,
    float **output)
{
}

}
