// Copyright 2017 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

#include "types.h"

// TODO make these const, except test_block wants to initialize them...
struct ControlSample {
    RealTime time;
    double val;
    ControlSample(RealTime time, double val) : time(time), val(val) {}
};
