// Copyright 2016 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

#include <stdint.h>

namespace utf8 {

// This is a UCS-4 char, which is the same as a Haskell char.
typedef uint32_t rune;

const char *backward(const char *str, const char *start);
const char *forward(const char *str, const char *end);
int width(const char *str);
// The byte index of 'chars' char index into a utf8 string, of the given length.
// Returns 'len' if the index is past the end.
int bytes(const char *str, int len, int chars);

// Encode and return in a null-terminated buffer.  The buffer is owned by
// 'encode'.
const char *encode(rune c);

}
