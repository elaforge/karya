// This has constants which are not worth putting under host control by putting
// in a *Config class.
#ifndef __CONFIG_H
#define __CONFIG_H

#include <FL/Enumerations.H>
#include "util.h"

namespace Config {

// This color signifies that some content has been omitted due to lack of space.
extern const Color abbreviation_color;
// Mark the playing end of events with this.
extern const Color event_trigger_color;

enum {
    max_selections = 5,
    font = FL_HELVETICA
};

namespace font_size {
    enum {
        event = 12,
        input = 12,
        ruler = 9,
        track_status = 12
    };
};

}

#endif
