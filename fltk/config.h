// This has constants which are not worth putting under host control by putting
// in a *Config class.
#ifndef __CONFIG_H
#define __CONFIG_H

#include "util.h"

namespace Config {

// This color signifies that some content has been omitted due to lack of space.
extern Color abbreviation_color;

enum { max_selections = 5 };

}

#endif
