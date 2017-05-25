// Copyright 2017 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include "driver.h"
#include <faust/dsp/dsp.h>

#include "build/faust_all.cc"


extern "C" {

int
get_patches(const char ***names, Patch **patches)
{
    *names = all_names;
    *patches = all_dsps;
    return all_count;
}

int
get_controls(Patch inst, const char ***controls)
{
    return 0; // TODO
}

Instrument
initialize(Patch patch, int sample_rate)
{
    // The clone() method should be const, but it looks like the fault
    // authors didn't know about const.
    Instrument instrument = const_cast<Instrument >(patch)->clone();
    instrument->init(sample_rate);
    return instrument;
}

void
destroy(Instrument instrument)
{
    delete instrument;
}

void
render(Instrument inst, int start_frame, int end_frame, Point **controls,
    float **output)
{
}

}
