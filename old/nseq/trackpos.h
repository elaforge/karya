/*

track units go from 0 - 2^32
zoom maps track units to pixels
playback will map track units to seconds

*/

#include "util.h"

typedef unsigned long Trackpos;
typedef Range_tmpl<Trackpos> Trange;
