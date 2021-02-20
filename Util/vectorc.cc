// Copyright 2019 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <cstddef>

// "C++"
extern "C" {

// This loop in ST with an MVector and strictness, explicit recursion,
// unsafeWrite, and all that takes around 0.10s.  Doing it in C with the same
// input takes, around, uh, 0.00s.
void
mix_vectors(float *out, size_t out_len,
    const float **ins, size_t *in_lens, size_t ins_len)
{
    for (size_t i = 0; i < ins_len; i++) {
        for (size_t j = 0; j < in_lens[i]; j++) {
            out[j] += ins[i][j];
        }
    }
}

};
